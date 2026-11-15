package cardinality.compiled

import scala.collection.mutable

import tastyquery.Constants.Constant
import tastyquery.Contexts.*
import tastyquery.Exceptions.{InvalidProgramStructureException, MemberNotFoundException}
import tastyquery.Modifiers.{OpenLevel, TermSymbolKind}
import tastyquery.Symbols.*
import tastyquery.Traversers
import tastyquery.Trees.*
import tastyquery.Types.*

/** Which inline methods count as methods (AGENTS.md, "Inline methods count as methods"; the plan's
  * decisions D2 and D4), read from typed trees.
  *
  * An expansion differs from the method's own body only through what Scala decides at the call
  * site. Over the compiler's trees those are nodes and symbols: `inline if` and `inline match`, a
  * quote or a splice, a reference to `scala.compiletime` or `scala.quoted`, a runtime class that
  * depends on a type parameter, an inline method the body calls, which brings its own body along,
  * and an inline override the call site's classes select on a parameter or on `this`. Everything
  * else was typed once, where the method is written: the overloads, the conversions and extensions
  * the body applies (D2) and the givens it passes (D4) are calls in the tree, read like any call. A
  * body free of all that, calling only inline methods whose bodies are, is the same function of its
  * values at every expansion: it counts as a `def`.
  *
  * `own` are the module's top-level classes and `dependencies` those of its classpath: where an
  * inline override may be (the closed world). The dependencies are asked for only where a body
  * selects a member of a class outside the module on a receiver, since listing them reads them.
  */
final private[compiled] class Inlines(own: Set[ClassSymbol], dependencies: => List[ClassSymbol])(
    using Context
) {

  /** Why a method is not counted as a method, if it is not: it is an inline method an expansion of
    * which a call site decides, or a definition inside one, which each expansion copies (Scala
    * allows no inline method inside another). A constructor's value space is the same in every
    * copy.
    */
  def refusal(method: TermSymbol): Option[String] =
    if (inlineMethod(method))
      narrowed(method).orElse(verdict(method).map(why => s"${Inlines.Prefix}: $why"))
    else if (World.isConstructor(method)) None
    else
      World
        .owners(method.owner)
        .iterator
        .collect { case around: TermSymbol if inlineMethod(around) => around }
        .flatMap { around =>
          verdict(around).map { why =>
            s"${Inlines.Inside}: ${around.name} $why, and every expansion copies this definition"
          }
        }
        .nextOption()

  /** Whether a symbol is an inline method: a call of it is its expansion. */
  def inlineMethod(symbol: TermSymbol): Boolean =
    symbol.isInline && symbol.kind == TermSymbolKind.Def

  private val verdicts = mutable.Map.empty[TermSymbol, Option[String]]

  // The first reason in the method's own definition or in one an expansion brings along. The
  // definitions reached are a finite set, read once each: a cycle of inline calls is decided with
  // everything it reaches, never assumed to pass.
  private def verdict(method: TermSymbol): Option[String] = verdicts.getOrElseUpdate(
    method, {
      // The overrides the method's own body reaches by a selection: the member and on what.
      val selections = mutable.Map.empty[TermSymbol, (TermSymbol, String)]
      @annotation.tailrec
      def first(queue: List[TermSymbol], seen: Set[TermSymbol]): Option[(TermSymbol, String)] =
        queue match {
          case Nil          => None
          case head :: rest =>
            val body = read(head)
            body.reason match {
              case Some(why) => Some(head -> why)
              case None      =>
                val reached = body.brings.distinct.filterNot(seen)
                // A selection is the method's own only where its own body makes it.
                if (head == method)
                  reached.foreach(m => body.selected.get(m).foreach(selections.update(m, _)))
                first(rest ++ reached, seen ++ reached)
            }
        }
      first(List(method), Set(method)).map { (at, why) =>
        if (at == method) why
        else
          selections.get(at).fold(s"calls inline method ${named(at)}, which $why") { (member, on) =>
            s"selects ${member.name} on $on, which a call site may type as a subclass of " +
              s"${member.owner.name} that overrides ${member.name} inline, and inline method " +
              s"${named(at)} $why"
          }
      }
    }
  )

  // Rule X: the call site of a `transparent inline` method types the expansion by what its body
  // returns, unless the declared type is a singleton, which leaves nothing narrower. An export's
  // forwarder is transparent by construction: it is the member it forwards to, narrowed where
  // that member is.
  def narrowed(method: TermSymbol): Option[String] =
    if (method.isExport) read(method).brings.iterator.flatMap(narrowed).nextOption()
    else
      method.tree.collect {
        case d: DefDef if method.isTransparentInline && !singleton(d.resultTpt) =>
          "transparent inline result: the call site observes the expansion's type, not the " +
            s"declared ${d.resultTpt.toType.showBasic}"
      }

  private def singleton(tpt: TypeTree): Boolean = tpt match {
    case _: SingletonTypeTree => true
    case other                =>
      other.toType match {
        case _: TermRef | _: ConstantType | _: ThisType => true
        case _                                          => false
      }
  }

  // One inline method's own definition: why an expansion of it is decided at the call site, and
  // the inline methods an expansion brings along (the ones it calls, and the inline overrides of
  // the members it selects on a parameter or on `this`).
  // `selected` keeps, for an override brought along, the member selected and what it is selected
  // on.
  final private case class Body(
      reason: Option[String],
      brings: List[TermSymbol],
      selected: Map[TermSymbol, (TermSymbol, String)] = Map.empty
  )

  private val bodies = mutable.Map.empty[TermSymbol, Body]

  private def read(method: TermSymbol): Body = bodies.getOrElseUpdate(
    method,
    method.tree match {
      case Some(definition: DefDef) =>
        val walk = new Walk(definition)
        // A default argument is written in the definition, and read with it; the compiler keeps
        // it as a method beside this one.
        (definition :: defaults(method)).foreach(walk.traverse)
        Body(
          walk.reason.orElse(Option.when(method.isMacro)("is a macro")),
          walk.brings.toList,
          walk.selected.toMap
        )
      // An inline method of a class read without its trees: nothing says what it expands to.
      case _ => Body(Some("has no body in the classes read"), Nil)
    }
  )

  // The methods the compiler makes of a method's default arguments (`m$default$2`), declared where
  // the method is: in its class, or in the definition around it.
  private def defaults(method: TermSymbol): List[DefDef] = {
    val prefix = s"${method.name}$$default$$"
    method.owner.tree.toList.flatMap(methods).collect {
      case d: DefDef if d.symbol.owner == method.owner && d.name.toString.startsWith(prefix) => d
    }
  }

  // Every method a tree defines, at any depth.
  private def methods(tree: Tree): List[DefDef] = {
    val found = List.newBuilder[DefDef]
    new Traversers.TreeTraverser {
      override def traverse(tree: Tree): Unit = {
        found ++= Option(tree).collect { case d: DefDef => d }
        super.traverse(tree)
      }
    }.traverse(tree)
    found.result()
  }

  // The inline methods some classes declare, by name: what may override a member inline.
  private def inlineMembers(classes: Iterable[ClassSymbol]): Map[String, List[TermSymbol]] =
    classes.toList
      .flatMap(_.tree)
      .flatMap(World.classes)
      .flatMap(_.rhs.body)
      .collect { case d: DefDef if inlineMethod(d.symbol) => d.symbol }
      .groupBy(_.name.toString)

  private lazy val inModule = inlineMembers(own)
  private lazy val inDependencies = inlineMembers(dependencies.filterNot(own))

  // The inline methods that override `member` in a class of the program. A call site may type the
  // receiver as that class, and then expands the override. No dependency extends a class of the
  // module, which it was compiled before.
  private def overriders(member: TermSymbol): List[TermSymbol] = {
    val name = member.name.toString
    val ofModule = World.owners(member).exists {
      case c: ClassSymbol => own(c)
      case _              => false
    }
    (inModule.getOrElse(name, Nil) ++ (if (ofModule) Nil else inDependencies.getOrElse(name, Nil)))
      .filter(candidate => candidate != member && candidate.allOverriddenSymbols.contains(member))
  }

  // Whether a subclass may override a member: a method of a class that may be extended, neither
  // private nor final.
  private def overridable(member: TermSymbol): Boolean = member.owner match {
    case c: ClassSymbol =>
      member.kind == TermSymbolKind.Def && !member.isPrivate && !member.isFinalMember &&
      !World.isConstructor(member) && !c.isModuleClass && c.openLevel != OpenLevel.Final
    case _ => false
  }

  final private class Walk(definition: DefDef) extends Traversers.TreeTraverser {
    var reason: Option[String] = None
    val brings = mutable.ListBuffer.empty[TermSymbol]
    val selected = mutable.Map.empty[TermSymbol, (TermSymbol, String)]

    // What an expansion may type more narrowly than declared: the method's own value parameters,
    // and those of a lambda in its body, which an expansion applied in place binds to its
    // arguments. A local method's parameters keep their declared types.
    private val parameters = mutable.Set.empty[Symbol] ++= values(definition)

    private def values(of: DefDef): List[Symbol] =
      of.paramLists.collect { case Left(values) => values.map(_.symbol) }.flatten

    private def stop(why: String): Unit = if (reason.isEmpty) reason = Some(why)

    override def traverse(tree: Tree): Unit = if (reason.isEmpty) {
      tree match {
        // What a `throw` is given is no value of the method (AGENTS.md, "Ignore throwing"), and
        // neither is the message of the library's `sys.error`.
        case _: Throw                                                      => ()
        case Apply(fun: TermReferenceTree, _) if is(fun, Inlines.SysError) => ()
        case _                                                             =>
          // A type the classes read do not hold is not known to be harmless.
          try read(tree)
          catch {
            case missing: MemberNotFoundException =>
              stop(s"names ${missing.name}, which the classes read do not hold")
          }
          super.traverse(tree)
      }
    }

    private def read(tree: Tree): Unit =
      tree match {
        case _: InlineIf                                               => stop("uses inline if")
        case _: InlineMatch                                            => stop("uses inline match")
        case _: Quote | _: Splice | _: QuotePattern | _: SplicePattern =>
          stop("uses a quote or a splice")
        case TypeApply(test: TermReferenceTree, arguments) if selects(test, "isInstanceOf") =>
          arguments.foreach(argument => reified(argument.toType, "tests the type"))
        case TypeApply(reference: TermReferenceTree, arguments) if is(reference, Inlines.ClassOf) =>
          arguments.foreach(argument => reified(argument.toType, "reifies the class of"))
        case test: TypeTest         => reified(test.tpt.toType, "tests the type")
        case Literal(constant)      => reifiedClass(constant)
        case ref: TermReferenceTree => referenced(ref)
        case tpt: TypeTree          => written(tpt)
        case closure: DefDef if closure.name.toString.startsWith("$anonfun") =>
          parameters ++= values(closure)
        case _ => ()
      }

    private def selects(reference: TermReferenceTree, name: String): Boolean =
      symbolOf(reference).exists(_.name.toString == name)

    private def is(reference: TermReferenceTree, method: String): Boolean =
      symbolOf(reference).exists(_.displayFullName == method)

    // A `classOf` the compiler already folded into a constant.
    private def reifiedClass(constant: Constant): Unit = constant.value match {
      case tpe: Type => reified(tpe, "reifies the class of")
      case _         => ()
    }

    // A reference's symbol; none, and a reason, where the classpath does not hold what it names:
    // an unknown callee is never taken for a harmless one.
    private def symbolOf(reference: TermReferenceTree): Option[Symbol] =
      try Some(reference.symbol)
      catch {
        case missing: MemberNotFoundException =>
          stop(s"names ${missing.name}, which the classes read do not hold")
          None
      }

    private def referenced(reference: TermReferenceTree): Unit = symbolOf(reference).foreach {
      case symbol: TermSymbol if compileTime(symbol) => stop(s"uses ${named(symbol)}")
      case symbol: TermSymbol                        =>
        if (inlineMethod(symbol)) brings += symbol
        if (onReceiver(reference) && overridable(symbol)) bringOverriders(reference, symbol)
      case _ => ()
    }

    private def bringOverriders(reference: TermReferenceTree, symbol: TermSymbol): Unit = {
      val found = overriders(symbol)
      brings ++= found
      found.foreach(selected.getOrElseUpdate(_, symbol -> receiver(reference)))
    }

    // Whether a member is selected on what a call site may type as a subclass: `this` (a member
    // named alone is selected on it), a parameter, what is selected or called on one, whose type
    // an override may narrow, or a receiver whose type an expansion instantiates. An ascription
    // to a type that names no type parameter fixes the receiver's type, and so does such a local
    // value's declaration.
    // What a member is selected on, as a reason says it.
    private def receiver(reference: TermReferenceTree): String = reference match {
      case Select(_: This, _) | _: Ident => "this"
      case Select(Ident(name), _)        => name.toString
      case _                             => "a receiver"
    }

    private def onReceiver(reference: TermReferenceTree): Boolean = reference match {
      case Select(qualifier, _) => narrowable(qualifier) || instantiated(qualifier.tpe)
      case _: Ident             => symbolOf(reference).exists(_.owner.isClass)
    }

    // The type parameters an expansion instantiates: the method's own, and those of the classes
    // around it, which the call site's prefix binds.
    private val typeParameters: Set[Symbol] =
      (definition.paramLists.collect { case Right(types) => types.map(_.symbol) }.flatten ++
        World
          .owners(definition.symbol)
          .collect { case c: ClassSymbol => c.typeParams }
          .flatten).toSet

    // Whether a receiver's type differs between expansions: it names one of those type
    // parameters or a parameter (its singleton, a member of its path), itself or through the
    // declared type of the local value it is read from.
    private def instantiated(tpe: TermType): Boolean = tpe match {
      case tpe: Type =>
        World.mentioned(tpe, symbol => typeParameters(symbol) || parameters(symbol)).nonEmpty ||
        (tpe match {
          case local: TermRef => instantiated(local.underlying)
          case _              => false
        })
      case _ => false
    }

    private def narrowable(receiver: TermTree): Boolean = receiver match {
      case _: This      => true
      case ident: Ident =>
        symbolOf(ident).exists { symbol =>
          parameters(symbol) || (symbol.owner match {
            case c: ClassSymbol => !c.isModuleClass
            case _              => false
          })
        }
      case Select(inner, _)     => narrowable(inner)
      case Apply(called, _)     => narrowable(called)
      case TypeApply(called, _) => narrowable(called)
      case _                    => false
    }

    // A type the body writes or the compiler inferred for it.
    private def written(tpt: TypeTree): Unit =
      try World.mentioned(tpt.toType, compileTime).foreach(symbol => stop(s"uses ${named(symbol)}"))
      catch {
        // A qualifier that names a package is no type.
        case _: InvalidProgramStructureException => ()
      }

    // A type whose runtime class the body reads. Scala substitutes the expansion's type arguments
    // into an inline body, so a class that depends on a type parameter tells instantiations apart.
    private def reified(tpe: Type, what: String): Unit =
      if (!fixed(tpe))
        stop(s"$what ${tpe.showBasic} at run time, which each expansion instantiates")

  }

  // Whether a type has one runtime class whatever an expansion instantiates: a class (an array's
  // class is its element's too), a singleton, a union or intersection of such. A type parameter or
  // an abstract member has none.
  private def fixed(tpe: Type): Boolean = tpe.dealias match {
    case ref: TypeRef         => ref.optSymbol.exists(_.isClass)
    case applied: AppliedType =>
      fixed(applied.tycon) && (!array(applied.tycon) || applied.args.forall {
        case argument: Type => fixed(argument)
        case _              => false
      })
    case _: TermRef | _: ConstantType | _: ThisType => true
    case and: AndType                               => fixed(and.first) && fixed(and.second)
    case or: OrType                                 => fixed(or.first) && fixed(or.second)
    case annotated: AnnotatedType                   => fixed(annotated.typ)
    case _                                          => false
  }

  private def array(constructor: Type): Boolean =
    World.classNamed(constructor).exists(_.displayFullName == "scala.Array")

  // Whether a symbol is declared under `scala.compiletime` or `scala.quoted`.
  private def compileTime(symbol: Symbol): Boolean =
    Inlines.CompileTime.exists(World.under(symbol, _))

  // A symbol as a reason names it: its path, an object by its own name, and without the object
  // the compiler makes of a file's top-level definitions.
  // A name as the report spells it: an anonymous given (a compiler-made `given_` name) by its
  // line, as its row is (`<given@line>`).
  private def named(symbol: Symbol): String = {
    val path = symbol.displayFullName
      .replace("$.", ".")
      .split('.')
      .filterNot(_.endsWith("$package"))
    symbol match {
      case g: TermSymbol if g.isGivenOrUsing && g.name.toString.startsWith("given_") =>
        g.tree.fold(path.mkString("."))(tree =>
          (path.init :+ s"<given@${tree.pos.startLine + 1}>").mkString(".")
        )
      case _ => path.mkString(".")
    }
  }

}

private[compiled] object Inlines {

  /** How a refusal for an inline method starts, and for a definition inside one (AGENTS.md, "Inline
    * methods count as methods": the reason says so).
    */
  val Prefix = "inline method"
  val Inside = "definition inside an inline method"

  private val CompileTime = List("scala.compiletime", "scala.quoted")
  private val SysError = "scala.sys.package$.error"
  private val ClassOf = "scala.Predef$.classOf"

}
