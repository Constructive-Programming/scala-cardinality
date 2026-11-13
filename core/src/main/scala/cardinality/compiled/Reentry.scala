package cardinality.compiled

import scala.collection.mutable
import scala.util.Try

import tastyquery.Contexts.*
import tastyquery.Modifiers.TermSymbolKind
import tastyquery.Names.SignedName
import tastyquery.Symbols.*
import tastyquery.Traversers.TreeTraverser
import tastyquery.Trees.*
import tastyquery.Types.{
  AppliedType,
  ByNameType,
  MethodType,
  PolyType,
  TermType,
  ThisType,
  Type,
  TypeLambda,
  TypeParamRef,
  TypeRef,
  WildcardTypeArg
}

/** Whether a concrete member a class of the program overrides may call the member being measured
  * (the old reading's view): such a member is a behavior of the measured member's own, not a
  * capability beside it, and `Receivers` leaves the row. (An abstract member is a capability
  * whatever its implementations call, as the scalameta front end has it.)
  *
  * Every behavior is searched: the member's own body and each overrider's, and from them every
  * member they call that the module declares, with what overrides it in turn. A call may reach the
  * measured member
  *
  *   - by a reference that may run it (`runs`: the member, or one of its name it may dispatch to);
  *   - by calling, on the instance, code the module does not declare, whose body is not read and
  *     may call back (what `Any` declares aside);
  *   - by giving the instance away (`this` anywhere but as the receiver of a selection);
  *   - by building an instance of the measured member's class, which runs its constructor;
  *   - where code outside the module may reach the measured member (`outward`), by handing that
  *     code a value of the module (`List(a).sorted(o)`), or by building a class whose body reaches
  *     it;
  *   - by first accessing an object of the module whose body reaches it.
  *
  * A body that is not read (an overrider no symbol stands for), a name that does not resolve, or a
  * search that reaches `Reach` members, may.
  *
  * A value of a capability record is another instance than the site's (`may` with `instance`
  * false), universally quantified: a free capability even where it may be an instance of the
  * measured member's class (the owner's ruling). Its behaviors re-enter the measured member only on
  * the same instance: handing on `this` (`selfReenters`), or where the module hands on an instance
  * of the record's class it builds or names (`handsMade`) and they reach the member (`reached`),
  * and only if a chain of references leads from them to it (`callersOf`).
  */
final private[compiled] class Reentry(world: World)(using Context) {

  def may(c: ClassSymbol, member: TermSymbol, measured: TermSymbol, instance: Boolean): Boolean = {
    val seen = mutable.Set.empty[Symbol]
    // The class the measured member runs in (a local method's enclosing one).
    val home = World.owners(measured.owner).collectFirst { case k: ClassSymbol => k }

    // Members a finished search on another instance found reach nothing of this measured member.
    val cleared =
      if (instance) mutable.Set.empty[Symbol]
      else unreaching.getOrElseUpdate(measured, mutable.Set.empty)

    // Code outside the module may call the measured member, which no reference names.
    val external = calledOutside(measured)
    // Whether code outside the module may run code that reaches the measured member: it may call
    // the member, or references do not settle what calls the member (`callersOf`).
    lazy val outward = external || callersOf(measured).isEmpty

    // Every body that may run where `m` is called on an instance of `owner`: its own and each
    // overrider's; and, where code outside the module may reach the measured member (else no such
    // body names it), a function literal's, not read, that implements a subclass of `owner` (whose
    // abstract method may redeclare `m`) or a class that does not resolve.
    def behaviors(owner: ClassSymbol, m: TermSymbol, reach: TermSymbol => Boolean): Boolean = {
      val others = world.overriders(owner, m)
      others.exists(_.isEmpty) || (m :: others.flatten).exists(reach) ||
      outward && literalFor(owner)
    }

    def dispatched(m: TermSymbol): Boolean =
      runs(m, measured) || (m.owner match {
        case owner: ClassSymbol => behaviors(owner, m, reaches)
        case _                  => reaches(m)
      })

    def reaches(m: TermSymbol): Boolean =
      seen.size >= Reentry.Reach || (!cleared(m) && seen.add(m) && m.tree.exists(calls))

    // A member called on the instance: the module's is searched, any other may call back.
    def fromInstance(m: TermSymbol): Boolean =
      if (Receivers.accessor(m)) {
        val target = m.name.toString.split("\\$\\$").last
        target == measured.name.toString || m.owner.isClass && m.owner.asClass.linearization
          .drop(1)
          .exists {
            _.declarations.exists {
              case parent: TermSymbol if parent.name.toString == target => fromInstance(parent)
              case _                                                    => false
            }
          }
      } else if (world.ofModule(m)) dispatched(m)
      else runs(m, measured) || !Reentry.silent(m.owner)

    // Whether a reference may run the measured member.
    def measures(reference: Tree): Boolean = called(reference).exists(runs(_, measured))

    // On another instance than the site's (a value of a capability record), a behavior re-enters
    // the measured member only by reaching it: a reference that may run it, a call of a member of
    // the module that does, a name that does not resolve, applying a function a field or parameter
    // holds or naming a by-name parameter (built where the search does not look), building an
    // instance of its class or of one that does not resolve, or first accessing an object whose
    // body reaches it. Where code outside the module may reach it (`outward`), so may that code:
    // when the behavior hands it a value of the module (`handsOver`, a member declared outside the
    // module selected on the instance included), or builds a class whose body reaches it.
    def reached(body: Tree): Boolean = search(body) {
      case reference: TermReferenceTree if measures(reference) =>
        Some(true)
      case reference: TermReferenceTree if Try(reference.symbol).isFailure => Some(true)
      case tree if outward && handsOver(tree)                              => Some(true)
      case Apply(fun @ Select(held, _), _)
          if called(fun).exists(_.name.toString == "apply") && called(held).exists { m =>
            (m.isParamAccessor || !m.isMethod) && World.functional(m.declaredType)
          } =>
        Some(true)
      case reference: TermReferenceTree
          if called(reference).exists(_.declaredType.isInstanceOf[ByNameType]) =>
        Some(true)
      // A selection on the instance names a member, and gives nothing away.
      case tree @ Select(_: This | _: Super, _)                   => Some(module(tree))
      case reference: TermReferenceTree if initializes(reference) => Some(true)
      case reference: TermReferenceTree => Option.when(module(reference))(true)
      case New(tpt)                     => Option.when(built(tpt).forall(reachesBuilt))(true)
      case _                            => None
    }

    // Whether building an instance of `k` may reach the measured member: it is of the measured
    // member's class, or its construction reaches it (an object's whatever code outside the module
    // may do, since references name its body; a class's where that code may reach the member).
    def reachesBuilt(k: ClassSymbol): Boolean =
      if (instance)
        // The site's own instance is not the one built: only its construction may reach the
        // measured member (a class of the module without a tree, anything).
        k.linearization.exists(a => world.ofModule(a) && a.tree.isEmpty) || constructs(k)
      else home.exists(k.isSubClass) || (outward || k.isModuleClass) && constructs(k)

    // Whether a reference to an object of the module, or to a member of one, runs the object's
    // body (its first access does) and that reaches the measured member.
    def initializes(reference: Tree): Boolean = called(reference).exists { s =>
      (s.moduleClass.toList ++ List(s.owner).collect {
        case k: ClassSymbol if k.isModuleClass => k
      })
        .exists(k => world.ofModule(k) && constructs(k))
    }

    // A member, or a module's value, the module declares.
    def own(m: TermSymbol): Boolean = world.ofModule(m) || m.moduleClass.exists(world.ofModule)

    // Whether a call or selection runs code outside the module (what `Any` declares aside).
    def outside(tree: Tree): Boolean =
      called(Reentry.unapplied(tree)._1).exists(m => !own(m) && !Reentry.silent(m.owner))

    // Whether a call or selection hands a value of the module to code outside it: an argument, on
    // which that code may call anything; or the receiver, on which the member selected runs its
    // body (any, where it is not read) or what overrides it.
    def handsOver(tree: Tree): Boolean = tree match {
      case Apply(function, arguments)            => outside(function) && arguments.exists(handed)
      case Select(qualifier, _) if outside(tree) =>
        handed(qualifier) && called(tree).exists { m =>
          Try(dispatched(m) || m.tree.isEmpty && !m.isAbstractMember).getOrElse(true)
        }
      case _ => false
    }

    // Whether a term may hold a value of the module: the instance, or a reference to what the module
    // declares (or one that does not resolve) whose value is no primitive, `String` or `Unit`. A
    // function literal's closure names only its own method, whose body beside it is searched.
    def handed(term: Tree): Boolean = search(term) {
      case _: This                                 => Some(true)
      case _: Lambda                               => Some(false)
      case plain: TermTree if Reentry.plain(plain) => Some(false)
      case reference: TermReferenceTree => Option.when(called(reference).forall(own))(true)
      case _                            => None
    }

    // Whether building an instance of `k` runs code that reaches the measured member: the
    // constructors of the module's classes it is, their parents' arguments and the statements of
    // their bodies (a method, a lazy value or a class declared there runs only where it is used).
    def constructs(k: ClassSymbol): Boolean = k.linearization.filter(world.ofModule).exists { c =>
      seen.size >= Reentry.Reach || seen.add(c) && c.tree.exists { definition =>
        val template = definition.rhs
        (template.parents ++ template.body).exists {
          // The measured field's own initializer runs.
          case value: ValDef if value.symbol == measured                    => true
          case _: DefDef | _: ClassDef | _: TypeMember | _: TypeTree        => false
          case value: ValDef if value.symbol.kind == TermSymbolKind.LazyVal => false
          case statement                                                    => calls(statement)
        }
      }
    }

    // Whether a reference calls a member of the module that reaches the measured one; overriders
    // that are not read may be anything.
    def module(reference: Tree): Boolean =
      called(reference).exists(m => world.ofModule(m) && Try(dispatched(m)).getOrElse(true))

    def calls(body: Tree): Boolean = if (!instance) reached(body)
    else
      search(body) {
        // The measured member's own definition (a local one's, in the member it is written in)
        // is the body being counted.
        case definition: DefDef if definition.symbol == measured => Some(false)
        case reference: TermReferenceTree if measures(reference) =>
          Some(true)
        case tree if outward && handsOver(tree)   => Some(true)
        case tree @ Select(_: This | _: Super, _) => Some(called(tree).forall(fromInstance))
        case _: This                              => Some(true)
        // A name that does not resolve may be anything (a package is no call).
        case reference: Ident
            if !Try(reference.symbol).toOption.exists(_.isPackage) &&
              called(reference).isEmpty =>
          Some(true)
        // A member named alone is the instance's, or an object's (imported, or `Predef`'s `???`),
        // which is read as a selection on that object.
        case reference: Ident
            if called(reference).exists(m => m.owner.isClass && !Reentry.objectMember(m)) =>
          Some(called(reference).forall(fromInstance))
        case reference: TermReferenceTree
            if reference.isInstanceOf[Select] || called(reference).exists(Reentry.objectMember) =>
          Option.when(called(reference).forall { m =>
            runs(m, measured) || (world.ofModule(m) && dispatched(m))
          })(true)
        case reference: TermReferenceTree if initializes(reference) => Some(true)
        case New(tpt) if built(tpt).forall(reachesBuilt)            => Some(true)
        case _                                                      => None
      }

    // A record value re-enters only as the same instance (the owner's ruling, D6 in
    // `compiled-own-types.md`, and the class's comment): handed to where it may re-enter (`flows`).

    // The members, with the parameters given such an instance, `passes` searched; the members
    // `selfReenters` searched.
    val passing = mutable.Set.empty[(Symbol, Set[(Int, Int)])]
    val onSelf = mutable.Set.empty[Symbol]

    // Whether a call hands a value `carries` says holds to where it may re-enter: the measured member,
    // code outside the module (as an argument or the receiver) where that code may reach it and is
    // given something it may call (a term that is not `inert`), or the call gives it what may
    // (`callsBack`), a constructor of the module whose class's members (its nested classes'
    // included: they read its fields) or construction may, a method of the module that hands it on
    // in turn (`passes`), or a name that does not resolve.
    def flows(call: Apply, carries: Tree => Boolean): Boolean = {
      val (head, clauses) = Reentry.unapplied(call)
      val taint = for {
        (arguments, i) <- clauses.zipWithIndex
        (argument, j) <- arguments.zipWithIndex
        if carries(argument)
      } yield (i, j)
      lazy val received = Reentry.receiver(head).exists(carries)
      (taint.nonEmpty || received) && called(head).forall { m =>
        taint.nonEmpty && runs(m, measured) || Try(
          if (!own(m))
            outward && !Reentry.silent(m.owner) &&
            (clauses.flatten ++ Reentry.receiver(head)).exists(!inert(_)) ||
            callsBack(head, clauses)
          else if (taint.isEmpty) false
          else if (World.isConstructor(m)) m.owner match {
            case k: ClassSymbol =>
              val templates = k.linearization.filter(world.ofModule)
              val nested = templates.flatMap(_.tree).flatMap(World.classes).map(_.symbol)
              val members = (templates ++ nested).distinct.flatMap(_.declarations)
              members.collect { case member: TermSymbol => member }.exists(dispatched) ||
              constructs(k)
            case _ => true
          }
          else passes(m, taint.toSet)
        ).getOrElse(true)
      }
    }

    // Whether a call gives code outside the module what may run the measured member: it applies a
    // function, or is given a function literal it may call (not `inert`) whose body reaches the
    // member, or another function (built where the search does
    // not look): a term of a function type, or of a class other than the record's that a function
    // literal of the module may implement (`Ordering`).
    def callsBack(head: Tree, clauses: List[List[TermTree]]): Boolean = {
      def function(term: Tree) = term match {
        case value: TermTree =>
          Try(value.tpe.widenTermRef match {
            case tpe: Type =>
              World.functional(tpe) ||
              Reentry.classOf(tpe).exists(k => literalFor(k) && !mixes(k, c))
            case _ => false
          }).getOrElse(true)
        case _ => false
      }
      Reentry.receiver(head).exists(function) || clauses.flatten.exists {
        case block @ Block(List(literal: DefDef), _: Lambda) =>
          !inert(block) && literal.rhs.exists(reached)
        case argument => function(argument)
      }
    }

    // Whether a body hands a value `carries` says holds to where it may re-enter: by a call, or as the
    // receiver of a member it selects (one selected on `this` runs on the instance, which
    // `selfReenters` reads): code outside the module that may reach the measured member, or a
    // member of the module whose behaviors, run on that value, hand it on.
    def handsOn(body: Tree, carries: Tree => Boolean): Boolean = search(body) {
      case call: Apply if flows(call, carries) => Some(true)
      case tree @ Select(qualifier, _) if !Reentry.onInstance(tree) && carries(qualifier) =>
        Option.when(called(tree).forall { m =>
          if (own(m)) Try(selfReenters(m)).getOrElse(true)
          else outward && !Reentry.silent(m.owner) && !inert(qualifier)
        })(true)
      case _ => None
    }

    // Whether `m`, or what overrides it, hands on what its parameters at `taint` are given.
    // A function literal of the module implementing an abstract `m` is a body not read.
    def passes(m: TermSymbol, taint: Set[(Int, Int)]): Boolean = {
      val others = overridersOf(m)
      val literal = m.isAbstractMember && m.owner.isClass && literalFor(m.owner.asClass)
      others.exists(_.isEmpty) || literal || (m :: others.flatten).exists { d =>
        passing.add(d -> taint) && d.tree.exists { definition =>
          val parameters = d.paramSymss.collect { case Left(clause) => clause }
          val tainted = taint.flatMap((i, j) => parameters.lift(i).flatMap(_.lift(j)))
          handsOn(definition, holder(reference => called(reference).exists(tainted.contains), None))
        }
      }
    }

    // Whether the behaviors of `m` on the value itself (of `owner`) hand it on to where it may
    // re-enter: as `this`, or through a member they call on it (a concrete member's body not read
    // is code outside the module given `this`).
    def selfReenters(m: TermSymbol, owner: Option[ClassSymbol] = None): Boolean = {
      val others = owner.fold(overridersOf(m))(world.overriders(_, m))
      others.exists(_.isEmpty) || (m :: others.flatten).exists { d =>
        val unread = d.tree.isEmpty && !d.isAbstractMember && !Reentry.silent(d.owner)
        onSelf.add(d) && (outward && unread || d.tree.exists { body =>
          handsOn(body, holdsSelf(c)) || search(body) {
            case tree if Reentry.onInstance(tree) =>
              Option.when(called(tree).exists { n =>
                !Reentry.silent(n.owner) && Try(selfReenters(n)).getOrElse(true)
              })(true)
            // Building a class nested in the value's, or first accessing such an object, runs its
            // construction on the value as its outer instance.
            case tree @ New(tpt) if holdsSelf(c)(tree) =>
              Option.when(built(tpt).forall(constructs))(true)
            case reference: TermReferenceTree
                if called(reference).exists(_.moduleClass.exists(constructs)) &&
                  holdsSelf(c)(reference) =>
              Some(true)
            case _ => None
          }
        })
      }
    }

    // Whether the module hands on, anywhere, an instance it builds or names that may be the record
    // value and an input of the measured member (`handings`): of the record's class, where the
    // member has an input that may hold it: a value parameter, a capture (of a local member, or an
    // outer instance's), its receiver (an instance of such a class) or the receiver's state (a
    // field of its class), or what its result's implementation is given (`opens`: `E => Boolean`).
    // Read from fresh searches (each with its own `Reach`), the answer depends on the measured
    // member and the record's class alone.
    def handsMade: Boolean = {
      val inputs = measured.paramSymss.exists(_.isLeft) || !measured.owner.isClass ||
        home.exists(h => !World.inObjects(h)) || Openings.opens(measured.declaredType)
      def stateful(k: ClassSymbol) = k.linearization.exists { a =>
        world.ofModule(a) && a.declarations.exists {
          case field: TermSymbol => !field.isMethod
          case _                 => false
        }
      }
      def ofRecord(made: Made) = made.forall(_.exists { k =>
        Try(k.isSubClass(c) && (inputs || home.forall(h => mixes(k, h) || stateful(h))))
          .getOrElse(true)
      })
      madeHanded.getOrElseUpdate(
        measured -> c, {
          seen.clear(); onSelf.clear(); passing.clear()
          handings.exists((site, made) => ofRecord(made) && handsOn(site, t => ofRecord(madeOf(t))))
        }
      )
    }

    // On another instance, a behavior no chain of references leads from to the measured
    // member (`callersOf`) does not reach it, whatever the search says. A measured strict field
    // runs when its class is built, which no reference names: not read so. A behavior that is a
    // field runs in its class's body, so it is a caller where that body is.
    val field = !measured.isMethod && measured.kind != TermSymbolKind.LazyVal
    val callers = Option.when(!instance && !field && !external)(callersOf(measured)).flatten
    val found = callers.forall { set =>
      behaviors(c, member, m => set(m) || (!m.isMethod && set(m.owner)))
    } && (if (instance) behaviors(c, member, reaches)
          else
            selfReenters(member, Some(c)) || handsMade && {
              // What the searches before found reaching may not be found again.
              seen.clear()
              behaviors(c, member, reaches)
            })
    // Every member such a search visited, finished below the cap, reaches nothing.
    if (!instance && !found && seen.size < Reentry.Reach) cleared ++= seen
    found
  }

  // For a record value, the members a finished search found to reach nothing, by the measured
  // member: what `reached` reads depends on it alone (and on the program).
  private val unreaching = mutable.Map.empty[TermSymbol, mutable.Set[Symbol]]

  // Whether code outside the module given a term can run no code of the module through it (no
  // side effects: that code holds nothing from before, and calls only what its own types declare).
  // Such a term is a plain value; one of a class of the module whose parents outside it declare no
  // member (`silent`); one of an object outside the module (built without the module's code); one
  // of a type parameter bounded by such a type; or one of a strict container outside the module
  // (`Strict`) whose type arguments are such. Any other value outside the module may wrap a
  // function of the module (`Ordering.by(f)`, a view, a lazy list); `Product` and `Equals` declare
  // members a class of the module may implement; a value of a top type (an unbounded parameter's
  // included) may be anything, which that code may test (`ScalaRunTime.stringOf`); and an opaque
  // type's representation may be anything.
  private def inert(term: Tree): Boolean = term match {
    case value: TermTree =>
      Try(value.tpe.widenTermRef match {
        case tpe: Type => inertType(tpe)
        case _         => false
      }).getOrElse(false)
    case _ => false
  }

  private def inertType(tpe: Type): Boolean = Reentry.plainType(tpe) || (tpe.dealias match {
    case applied: AppliedType =>
      Reentry.classOf(applied.tycon).exists { k =>
        if (world.ofModule(k)) inertClass(k)
        else
          Reentry.Strict(k.displayFullName) && applied.args.forall {
            case arg: Type                 => inertType(arg)
            case wildcard: WildcardTypeArg => inertType(wildcard.bounds.high)
          }
      }
    case ref: TypeRef =>
      ref.optSymbol match {
        case Some(k: ClassSymbol) =>
          if (world.ofModule(k)) inertClass(k) else k.isModuleClass
        case Some(p: (ClassTypeParamSymbol | LocalTypeParamSymbol)) =>
          inertType(p.declaredBounds.high)
        case _ => false
      }
    case ref: TypeParamRef => inertType(ref.bounds.high)
    case _                 => false
  })

  // Whether no parent outside the module of a class of the module, or of a class below it (the value
  // may be one, which that code may test for), declares a member.
  private def inertClass(k: ClassSymbol): Boolean = world
    .below(k)
    .exists(below =>
      (k :: below).forall(_.linearization.forall(a => world.ofModule(a) || Reentry.silent(a)))
    )

  // Whether an instance may be of both classes (`ClosedProgram.mixes`); one that cannot be told may.
  private def mixes(a: ClassSymbol, b: ClassSymbol): Boolean =
    Try(world.mixes(a, b)).getOrElse(true)

  // Whether a term's value holds what `named` says of a reference, or, where `self` is given,
  // `this` of a class that may be an instance of it: also through the result of a function
  // literal, an alias read through its body (a local, or, where `self`, a member of such a class),
  // a pattern's variable or a literal's parameter (`bound`), and, where `self`, an instance of a
  // class nested in such a class (which holds its outer instance).
  private def holder(named: Tree => Boolean, self: Option[ClassSymbol]): Tree => Boolean = {
    val fixed = Fixed[Boolean](false, identity)
    // Whether `this` of `k` may be an instance of `self`.
    def mayBe(k: ClassSymbol) = self.exists(mixes(k, _))
    def ofSelf(m: TermSymbol) =
      m.owner.isClass && !Reentry.objectMember(m) && mayBe(m.owner.asClass)
    // Whether a member outside the module (or, selected, a name that does not resolve) selected on,
    // or named in, an instance that may be `self` may return that instance (`this.type`).
    def returned(reference: Tree, instance: => Boolean): Boolean = self.nonEmpty &&
      !called(reference).exists(m => world.ofModule(m) || Reentry.silent(m.owner)) && instance
    def alias(reference: Tree): Boolean = called(reference).exists { m =>
      val member = !m.owner.isClass || ofSelf(m)
      // Each body a call may run: the member's own and each overrider's (one not read, any).
      val others = overridersOf(m)
      world.ofModule(m) && member && (others.exists(_.isEmpty) || (m :: others.flatten).exists {
        d =>
          fixed(d)(bound.get(d).exists(_.exists(value)) || d.tree.exists {
            case definition: ValOrDefDef => World.rhs(definition).exists(value)
            case _: Bind                 => false
          })
      })
    }
    def value(term: Tree): Boolean = fixed(term)(term match {
      case here: This => self.nonEmpty && Reentry.thisClass(here).forall(mayBe)
      case plain: TermTree if Reentry.plain(plain) => false
      case Block(List(literal: DefDef), _: Lambda) => literal.rhs.exists(value)
      case _: Lambda                               => false
      // A member selected on `this` is that member's value, not the instance.
      case reference @ Select(here @ (_: This | _: Super), _) =>
        named(reference) || alias(reference) || returned(reference, value(here))
      case New(tpt)
          if built(tpt)
            .exists(k => World.owners(k.owner).exists(o => o.isClass && mayBe(o.asClass))) =>
        true
      case reference: TermReferenceTree if named(reference) || alias(reference)      => true
      case reference: Ident if returned(reference, called(reference).exists(ofSelf)) => true
      case _ => Reentry.children(term).exists(value)
    })
    value
  }

  // Whether `this` of a class that may be an instance of `c` is part of a term's value (`holder`).
  private def holdsSelf(c: ClassSymbol): Tree => Boolean =
    selfHeld.getOrElseUpdate(c, holder(_ => false, Some(c)))

  private val selfHeld = mutable.Map.empty[ClassSymbol, Tree => Boolean]

  // Each variable a match's pattern binds in the module, with the scrutinee and what its pattern is
  // matched against (`Matches.boundTo`)
  // (a caught exception is no value: a throw supplies nothing); and each parameter of a function
  // literal (a partial function's included) a call is given, with the call's receiver and other
  // arguments, which code not read may give it (`Option(this).map(e => new Wrap(e))`).
  private lazy val bound: Map[Symbol, List[Tree]] = {
    val found = mutable.Map.empty[Symbol, List[Tree]]
    everyTree {
      case m @ Match(scrutinee, cases) =>
        matches(m): Unit
        visit(cases.map(_.pattern)) {
          case variable: Bind =>
            found(variable.symbol) = scrutinee :: matches.boundTo(variable)
        }
      case call: Apply =>
        val (head, clauses) = Reentry.unapplied(call)
        val arguments = clauses.flatten
        arguments.foreach {
          case literal @ Block(List(definition: DefDef), _: Lambda) =>
            val others = Reentry.receiver(head).toList ++ arguments.filterNot(_ eq literal)
            definition.symbol.paramSymss.foreach(_.left.foreach(_.foreach(found(_) = others)))
          case _ => ()
        }
    }
    found.toMap
  }

  // What may override `m` in the instances of its class's subclasses; none for a member of no class.
  private def overridersOf(m: TermSymbol): List[Option[TermSymbol]] = m.owner match {
    case k: ClassSymbol => world.overriders(k, m)
    case _              => Nil
  }

  // For a record value, by the measured member and the record's class: whether the module hands an
  // instance of that class it builds or names to where it may re-enter (`handsMade`).
  private val madeHanded = mutable.Map.empty[(TermSymbol, ClassSymbol), Boolean]

  // Whether a reference to `s` may run `t`: the same member, one of its name in a class an instance
  // may share with `t`'s (or one `mixes` cannot compare), through which it dispatches (a
  // constructor does not), or the field of a constructor's parameter, which its class's body names.
  private def runs(s: TermSymbol, t: TermSymbol): Boolean =
    s == t || s.name == t.name && !World.isConstructor(t) && ((s.owner, t.owner) match {
      case (a: ClassSymbol, b: ClassSymbol) => mixes(a, b)
      case (a: ClassSymbol, b: TermSymbol)  => World.isConstructor(b) && b.owner == a
      case _                                => false
    })

  // Whether code outside the module may call `m`: it overrides a member of that code, or a class
  // of the program that inherits it has a parent outside the module declaring a member of its name,
  // which `m` implements there (`trait Half { def compare… }; object C extends Half with Ordering`).
  private def calledOutside(m: TermSymbol): Boolean = Try(
    m.allOverriddenSymbols.exists(o => !world.ofModule(o)) || (m.owner match {
      case owner: ClassSymbol =>
        world.below(owner).forall { below =>
          (owner :: below).exists(_.linearization.exists { parent =>
            !world.ofModule(parent) && parent.declarations.exists(_.name == m.name)
          })
        }
      case _ => false
    })
  ).getOrElse(true)

  // The classes a function literal of the module implements other than a function type (a single
  // abstract method conversion): its body runs where that method is called. `None` for one whose
  // class does not resolve, or for a class of the module without a tree, which may hold any.
  private lazy val implemented: Set[Option[ClassSymbol]] = {
    val found = mutable.Set.empty[Option[ClassSymbol]]
    everyTree {
      case Lambda(_, Some(tpt)) => found += Reentry.classOf(tpt.toType)
    }
    if (world.module.exists(_.tree.isEmpty)) found += None
    found.toSet
  }

  // Whether a function literal of the module may implement `k`.
  private def literalFor(k: ClassSymbol): Boolean = implemented.exists(_.forall(_.isSubClass(k)))

  // The calls the compiler inserts (`Matches`), which `search` and `visit` read as written ones.
  private val matches = new Matches
  private def search(body: Tree)(d: Tree => Option[Boolean]) = Reentry.search(body, matches(_))(d)

  // Runs `f` on every tree of the module it is defined at; `visit`, on every tree in `roots`.
  private def everyTree(f: PartialFunction[Tree, Unit]): Unit =
    visit(world.module.flatMap(_.tree))(f)

  private def visit(roots: Iterable[Tree])(f: PartialFunction[Tree, Unit]): Unit = {
    val traverser = new TreeTraverser {
      override def traverse(tree: Tree): Unit = {
        f.applyOrElse(tree, (_: Tree) => ())
        super.traverse(tree)
        matches(tree).foreach(traverse)
      }
    }
    roots.foreach(traverser.traverse)
  }

  // What a term's value may be an instance of, of the instances the module builds or names: the
  // classes of a `new` or a function literal, an object, a member whose value is one (a factory),
  // or a name that does not resolve and may be such a member; none (`None`) where it may be any.
  private type Made = Option[Set[ClassSymbol]]

  private def union(parts: Iterable[Made]): Made =
    parts.foldLeft(Option(Set.empty[ClassSymbol]))((a, b) => a.zip(b).map(_ ++ _))

  // What a term, and a factory's value, was found to make.
  private val madeMemo = Fixed[Made](Some(Set.empty), _.isEmpty)

  private def madeOf(term: Tree): Made = {
    // What a node makes itself, and, unless that may be any, what the nodes inside it make.
    def inside(own: Made) =
      union(own :: (if (own.isEmpty) Nil else Reentry.children(term).map(madeOf)))
    madeMemo(term)(term match {
      // A type (an annotation's `new` included) builds nothing.
      case _: TypeTree => Some(Set.empty)
      case New(tpt)    => built(tpt).map(Set(_))
      // A function, or an instance of the class it names (one that does not resolve, any).
      case literal: Lambda =>
        literal.tpt.fold[Made](Some(Set.empty))(tpt => Reentry.classOf(tpt.toType).map(Set(_)))
      case self: This => Reentry.thisClass(self).map(k => Set(k).filter(_.isModuleClass))
      case plain: TermTree if Reentry.plain(plain) => Some(Set.empty)
      case reference: TermReferenceTree if Try(reference.symbol.isPackage).getOrElse(false) =>
        Some(Set.empty)
      case reference: TermReferenceTree =>
        val candidates =
          called(reference).fold(ownNamed.getOrElse(Reentry.written(reference), Nil))(List(_))
        inside(union(candidates.flatMap(m => List(Some(m.moduleClass.toSet), factory(m)))))
      case _ => inside(Some(Set.empty))
    })
  }

  // What a member's value makes: its definition's, or what `bound` gives a variable or parameter.
  private def factory(m: TermSymbol): Made =
    if (!world.ofModule(m)) Some(Set.empty)
    else
      madeMemo(m)(union(bound.getOrElse(m, Nil).map(madeOf) ++ m.tree.toList.flatMap {
        case definition: ValOrDefDef => World.rhs(definition).map(madeOf)
        case _: Bind                 => Nil
      }))

  // Every call and selection of the module that hands on (as a call's argument, a selection's
  // receiver) what may be an instance the module builds or names, with what those may be
  // (`madeOf`); `handsMade` keeps those that may be of the record's class.
  private lazy val handings: List[(Tree, Made)] = {
    val found = List.newBuilder[(Tree, Made)]
    everyTree {
      case tree =>
        val handed = tree match {
          case call: Apply =>
            val (head, clauses) = Reentry.unapplied(call)
            clauses.flatten ++ Reentry.receiver(head)
          case Select(qualifier, _) => List(qualifier)
          case _                    => Nil
        }
        val made = union(handed.map(madeOf))
        if (made.forall(_.nonEmpty)) found += tree -> made
    }
    found.result()
  }

  // The terms the module's classes declare, by name.
  private lazy val ownNamed: Map[String, List[TermSymbol]] =
    world.module.toList
      .flatMap(_.tree)
      .flatMap(World.classes)
      .flatMap { definition =>
        definition.symbol.declarations.collect { case m: TermSymbol => m }
      }
      .groupBy(_.name.toString)

  /** The definitions of the module that may call `measured`: those whose code holds a reference
    * that may run it, or run a caller (`runs`: dispatch; a reference that does not resolve, by its
    * name alone). Code a module class's body holds is run by any access to it, which names its
    * value or a member. None where code may run that no reference names: the body of a function
    * literal or a by-name argument (run by whatever holds it), of a member code outside the module
    * may call (`calledOutside`), of a class that is no object (which `new` runs), or a class of the
    * module without a tree.
    */
  private def callersOf(measured: TermSymbol): Option[Set[Symbol]] = callerSets.getOrElseUpdate(
    measured, {
      val found = mutable.Set.empty[Symbol]
      val targets = mutable.Queue(measured)
      val queued = mutable.Set[Symbol](measured)
      def call(symbol: Symbol): Unit = symbol match {
        case t: TermSymbol if queued.add(t) => targets += t
        case _                              => ()
      }
      var open = world.module.exists(_.tree.isEmpty)
      while (!open && targets.nonEmpty) {
        val target = targets.dequeue()
        referrers.getOrElse(target.name.toString, Nil).foreach {
          case (_, Some(symbol)) if !runs(symbol, target) => ()
          case (Some(holder), _)                          =>
            if (found.add(holder)) holder match {
              case k: ClassSymbol if k.isModuleClass =>
                Try {
                  k.moduleValue.foreach(call)
                  k.linearization.filterNot(World.universal).flatMap(_.declarations).foreach(call)
                }.getOrElse { open = true }
              case t: TermSymbol if !calledOutside(t) => call(t)
              case _                                  => open = true
            }
          case (None, _) => open = true
        }
      }
      Option.when(!open)(found.toSet)
    }
  )

  private val callerSets = mutable.Map.empty[TermSymbol, Option[Set[Symbol]]]

  // Each name the module references, with the code that holds such a reference: the nearest
  // method or lazy value around it, the class whose body does, or the parameter a by-name argument
  // of a method of the module nothing overrides is given (its code runs where the parameter is
  // named); none for a function literal's body or another by-name argument (run by whatever holds
  // it). Each reference comes with the member it resolves to, if it does. A pattern's extractor is
  // applied, and its result asked what the match reads.
  private lazy val referrers: Map[String, List[(Option[Symbol], Option[TermSymbol])]] = {
    val found = mutable.Map.empty[String, mutable.Set[(Option[Symbol], Option[TermSymbol])]]
    class Holder(holder: Option[Symbol]) extends TreeTraverser {
      def note(name: String, symbol: Option[TermSymbol] = None): Unit =
        found.getOrElseUpdate(name, mutable.Set.empty) += holder -> symbol
      def inside(tree: Tree): Unit = super.traverse(tree)
      override def traverse(tree: Tree): Unit = tree match {
        case d: DefDef =>
          Holder(Option.unless(d.symbol.name.toString.startsWith("$anonfun"))(d.symbol)).inside(d)
        case v: ValDef if v.symbol.kind == TermSymbolKind.LazyVal =>
          Holder(Some(v.symbol)).inside(v)
        case c: ClassDef                             => Holder(Some(c.symbol)).inside(c)
        case pattern @ (_: Unapply | _: ExprPattern) =>
          Matches.selected(pattern).foreach(note(_))
          super.traverse(tree)
        case call: Apply =>
          val (head, clauses) = Reentry.unapplied(call)
          traverse(head)
          val callee = World.named(head)
          val parameters = Try(callee.toList.flatMap(_.paramSymss.collect { case Left(p) => p }))
            .getOrElse(Nil)
          // Which parameters are by name, as the call's function is typed where it is selected (a
          // function value's `apply` of a by-name type included); one not read is taken for one.
          def flags(tpe: TermType, left: Int): List[List[Boolean]] = tpe match {
            case _ if left == 0     => Nil
            case poly: PolyType     => flags(poly.resultType, left)
            case method: MethodType =>
              method.paramTypes.map(_.isInstanceOf[ByNameType]) :: flags(
                method.resultType,
                left - 1
              )
            case _ => Nil
          }
          val byNames = head match {
            case function: TermTree =>
              Try(flags(function.tpe.widenTermRef, clauses.size)).getOrElse(Nil)
            case _ => Nil
          }
          // A call whose value holds no function runs what it is given, if at all, before it
          // returns (no side effects: nothing else may keep it).
          val plain = Reentry.plain(call)
          // A by-name argument is forced where its parameter is named, if the module's method
          // declaring it is the only one the call may run.
          val sole = callee.exists { m =>
            world.ofModule(m) && !m.isAbstractMember &&
            overridersOf(m).isEmpty
          }
          for {
            (arguments, i) <- clauses.zipWithIndex
            (argument, j) <- arguments.zipWithIndex
          } {
            val parameter = parameters.lift(i).flatMap(_.lift(j))
            val byName = byNames.lift(i).flatMap(_.lift(j)).getOrElse(true)
            argument match {
              case Block(List(literal: DefDef), _: Lambda) if plain => inside(literal)
              case _ if plain || !byName                            => traverse(argument)
              case _ => Holder(parameter.filter(_ => sole)).traverse(argument)
            }
          }
        case reference: TermReferenceTree =>
          note(Reentry.written(reference), World.named(reference))
          super.traverse(reference)
        case _ => super.traverse(tree)
      }
    }
    world.module.flatMap(_.tree).foreach(c => Holder(Some(c.symbol)).inside(c))
    found.view.mapValues(_.toList).toMap
  }

  // The class a `new` builds; none where its type does not resolve (its arguments included), which
  // may be any. (A case class's synthetic `apply` names its class through a type lambda over its
  // parameters.)
  private def built(tpt: TypeTree): Option[ClassSymbol] = Try(tpt.toType).toOption.flatMap {
    case lambda: TypeLambda => Reentry.classOf(lambda.resultType)
    case tpe                => Reentry.classOf(tpe)
  }

  // The member a reference names; none where it does not resolve, which may be anything.
  private def called(tree: Tree): Option[TermSymbol] = World.named(tree)

}

private[compiled] object Reentry {

  /** A reference's written name, without the signature a selection may carry. */
  def written(reference: TermReferenceTree): String = (reference match {
    case Ident(name)     => name
    case Select(_, name) => name
  }) match {
    case signed: SignedName => signed.underlying.toString
    case other              => other.toString
  }

  /** The class a type names; none where it names none or does not resolve. */
  def classOf(tpe: => Type)(using Context): Option[ClassSymbol] =
    Try(World.classNamed(tpe)).toOption.flatten

  /** The class `this` stands for; none where it does not resolve. */
  private def thisClass(here: This)(using Context): Option[ClassSymbol] = Try(here.tpe match {
    case tpe: ThisType => Some(tpe.cls)
    case _             => None
  }).toOption.flatten

  /** Whether a member is an object's, which a name alone reaches through an import or `Predef`. */
  def objectMember(m: TermSymbol): Boolean = m.owner match {
    case k: ClassSymbol => k.isModuleClass
    case _              => false
  }

  /** The receiver a call's function is selected on, if any. */
  private def receiver(head: Tree): Option[Tree] = head match {
    case Select(qualifier, _) => Some(qualifier)
    case _                    => None
  }

  /** A call's function and the value clauses it is applied to, in order. */
  def unapplied(tree: Tree): (Tree, List[List[TermTree]]) = tree match {
    case Apply(function, arguments) =>
      val (head, clauses) = unapplied(function)
      (head, clauses :+ arguments)
    case TypeApply(function, _) => unapplied(function)
    case head                   => (head, Nil)
  }

  /** Whether a reference names a member of the instance: selected on `this`, or a member of a class
    * that is no object named alone (an object's is a reference to that object).
    */
  private def onInstance(tree: Tree)(using Context): Boolean = tree match {
    case Select(_: This | _: Super, _) => true
    case reference: Ident              =>
      World
        .named(reference)
        .exists(_.owner match {
          case k: ClassSymbol => !k.isModuleClass
          case _              => false
        })
    case _ => false
  }

  /** The trees directly inside `tree`. */
  private def children(tree: Tree): List[Tree] = {
    val found = List.newBuilder[Tree]
    new TreeTraverser {
      override def traverse(inner: Tree): Unit =
        if (inner eq tree) super.traverse(inner) else found += inner
    }.traverse(tree)
    found.result()
  }

  /** Whether a term's value holds no function: a primitive value, a `String` or `Unit`. */
  private def plain(term: TermTree)(using Context): Boolean = Try(term.tpe match {
    case tpe: Type => plainType(tpe)
    case _         => false
  }).getOrElse(false)

  def plainType(tpe: Type)(using Context): Boolean =
    List(
      defn.IntType,
      defn.LongType,
      defn.FloatType,
      defn.DoubleType,
      defn.BooleanType,
      defn.ByteType,
      defn.ShortType,
      defn.CharType,
      defn.UnitType,
      defn.StringType
    ).exists(tpe.isSubType)

  /** The containers outside the module that hold only the values of their type arguments, built
    * eagerly: no function of the module hides in one.
    */
  private val Strict: Set[String] = Set(
    "scala.Option",
    "scala.Some",
    "scala.collection.immutable.List",
    "scala.collection.immutable.Vector"
  )

  /** Whether a class declares no member but those every value has (`Product` and `Equals` aside,
    * whose members a class of the module may implement and code outside it call): a call of such a
    * member calls nothing back. (`World.universal`, which `callersOf` and `opens` read, also holds
    * `Product` and `Equals`: there the question is which members a count reads.)
    */
  private def silent(owner: Symbol): Boolean = Silent(owner.displayFullName)

  private val Silent =
    Set("scala.Any", "scala.Matchable", "scala.AnyVal", "java.lang.Object", "java.io.Serializable")

  /** How many members the search reads before it gives up. */
  val Reach = 200

  /** Whether a node of `body` is found: `decide` says so for a node it knows (`false` leaves the
    * node's children unsearched), and the children of any other node, and what `expand` adds.
    */
  def search(body: Tree, expand: Tree => List[Tree] = _ => Nil)(
      decide: Tree => Option[Boolean]
  ): Boolean = {
    var found = false
    new TreeTraverser {
      override def traverse(tree: Tree): Unit = if (!found) decide(tree) match {
        case Some(answer) => found = answer
        case None         => super.traverse(tree); expand(tree).foreach(traverse)
      }
    }.traverse(body)
    found
  }

}
