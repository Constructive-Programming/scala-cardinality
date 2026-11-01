package cardinality.types

import scala.meta.*

/** How a sealed family's subtype instantiates its parent: which of the subtype's own parameters
  * stands for each of the parent's, read off the `extends` clause. Both halves of a report read a
  * family through it — the method side's representation and the stored-value estimate's sum.
  *
  * `C[Ȳ] extends P[ā]` holds |C[t̄]| values of `P[t̄]` for every instantiation exactly when each
  * `aᵢ` is either a parameter of the subtype bound to that slot and no other, or a slot pinned
  * bottom-up (`Nothing` into a covariant slot, `Any` into a contravariant one), which every
  * instantiation admits. `P[t]` holds `C[u]` for every `u` the slot's variance allows: `u = t` when
  * invariant, `u <: t` when covariant, `u >: t` when contravariant. The union of those `C[u]` is
  * `C[t]` when `C` varies the same way (`C[u] <: C[t]`), or, for a covariant slot, when every field
  * of `C` holds a `u`-value only where it is a `t`-value too. A slot pinned to any other type (a
  * GADT index such as `extends Box[Int]`), a subtype parameter no slot binds, or a parameter that
  * varies otherwise has no summand: each is a refusal.
  */
private[cardinality] object SealedSlots {

  // Trees are parsed and reprinted under Scala 3 dialect rules.
  private given scala3: Dialect = dialects.Scala3

  /** A sealed parent as its subtypes' clauses instantiate it: its name and type parameters. */
  final case class Parent(name: String, params: List[Type.Param])

  /** A subtype as its clause and constructor read: its name, its `extends` clause, its own type
    * parameters, the types it stores (`None` for an abstraction whose values are its subtypes'),
    * and the variances of a constructor a field names, where that name denotes a declaration the
    * reader knows.
    */
  final case class Subtype(
      name: String,
      inits: List[Init],
      params: List[Type.Param],
      fields: Option[List[Type]],
      declared: String => Option[List[Variance]] = _ => None
  )

  /** How a subtype instantiates its parent: each subtype parameter with the parent parameter its
    * slot binds, and the names the reading took for the standard library's, which the caller checks
    * no source binds where the clause is written — the bottom pins (`Nothing`, `Any`) and the data
    * constructors a field holds a parameter through.
    */
  final case class Instantiation(
      bindings: List[(String, String)],
      pins: List[String],
      data: List[String]
  )

  /** A subtype definition as `instantiation` reads it: an object stores nothing, a concrete class
    * its constructor's fields, an enum its cases', an abstraction (`None`) its own subtypes';
    * `declared` as `Subtype`'s.
    */
  def subtype(
      d: Defn.Object | Defn.Class | Defn.Trait | Defn.Enum,
      declared: String => Option[List[Variance]]
  ): Subtype = {
    val fields = d match {
      case _: Defn.Object => Some(Nil)
      case c: Defn.Class  => Option.unless(c.mods.exists(_.is[Mod.Abstract]))(Trees.fields(c.ctor))
      case e: Defn.Enum   =>
        Some(e.templ.body.stats.collect { case c: Defn.EnumCase => Trees.fields(c.ctor) }.flatten)
      case _ => None
    }
    val params = d match {
      case _: Defn.Object                           => Nil
      case p: (Defn.Class | Defn.Trait | Defn.Enum) => p.tparamClause.values
    }
    Subtype(d.name.value, d.templ.inits, params, fields, unlessOwn(params, declared))
  }

  // A field's constructor named like one of the subtype's own type parameters is that parameter
  // applied, never a declaration.
  private def unlessOwn(
      params: List[Type.Param],
      declared: String => Option[List[Variance]]
  ): String => Option[List[Variance]] =
    name => if (params.exists(_.name.value == name)) None else declared(name)

  /** The variances of the constructor named `name` that the file of `d` declares, when exactly one
    * of its declarations of that name is one `denotes` accepts — the reader's check that the name
    * means that declaration where `d` is written.
    */
  def declaredIn(d: Tree, denotes: (Tree, String) => Boolean)(
      name: String
  ): Option[List[Variance]] =
    Trees.root(d).collect(Trees.typeNamed(name)).filter(denotes(_, name)) match {
      case List(declared) => Some(declared.tparamClause.values.map(Variance.of))
      case _              => None
    }

  /** How an enum case with its own `extends` clause instantiates its enum. */
  def enumCase(
      c: Defn.EnumCase,
      e: Defn.Enum,
      declared: String => Option[List[Variance]]
  ): Either[String, Instantiation] = {
    val params = caseParams(c, e)
    instantiation(
      Parent(e.name.value, e.tparamClause.values),
      Subtype(
        c.name.value,
        c.inits,
        params,
        Some(Trees.fields(c.ctor)),
        unlessOwn(params, declared)
      )
    )
  }

  /** How the subtype instantiates the parent, or why its clause does not instantiate it uniformly.
    */
  def instantiation(parent: Parent, subtype: Subtype): Either[String, Instantiation] =
    parentArguments(subtype, parent.name).flatMap { args =>
      if (args.size != parent.params.size)
        Left(
          s"sealed subtype ${subtype.name} pins ${parent.name} with ${args.size} of its " +
            s"${parent.params.size} parameters"
        )
      else
        Traversals
          .sequence(args.zip(parent.params).map(slotBinding(_, _, subtype, parent.name)))
          .flatMap(perSlot => bindsEachOnce(perSlot.flatten, subtype, parent.name))
          .flatMap { bound =>
            val fit = Fit(parent, subtype, bound)
            Traversals
              .sequence(bound.map(fit.misfit))
              .map(data => Instantiation(bound, bottomPins(args), data.flatten.distinct))
          }
    }

  /** An enum case's own parameters: those it declares or, failing that, the enum's parameters its
    * fields or its `extends` clause mention — Scala gives `case K(a: A) extends G[A]` and `case
    * K(n: Boolean) extends G[A]` the parameter `A` (3.9.0 fixtures: `G.K[String](…)`).
    */
  def caseParams(c: Defn.EnumCase, e: Defn.Enum): List[Type.Param] =
    if (c.tparamClause.values.nonEmpty) c.tparamClause.values
    else {
      val mentioned = Trees.fields(c.ctor) ++ c.inits.map(_.tpe)
      e.tparamClause.values.filter(p => mentioned.exists(TypeName.words(_).contains(p.name.value)))
    }

  // The names the clause pins slots bottom-up with (`Nothing`, `Any`): sound only where they are
  // the standard library's, which the caller checks where the clause is written.
  private def bottomPins(args: List[Type]): List[String] =
    args.collect { case Type.Name(name @ ("Nothing" | "Any")) => name }

  // The arguments the child's clause gives the parent: none for a bare name.
  private def parentArguments(subtype: Subtype, parentName: String): Either[String, List[Type]] =
    subtype.inits.find(init => plainCallee(init.tpe).contains(parentName)) match {
      case None =>
        Left(s"sealed subtype ${subtype.name} reaches $parentName only through a qualified name")
      case Some(init) =>
        init.tpe match {
          case application: Type.Apply => Right(application.argClause.values.toList)
          case _: Type.Name            => Right(Nil)
          case _ => Left(s"sealed subtype ${subtype.name} applies $parentName unreadably")
        }
    }

  private def slotBinding(
      arg: Type,
      parameter: Type.Param,
      subtype: Subtype,
      parentName: String
  ): Either[String, Option[(String, String)]] = {
    val childNames = subtype.params.map(_.name.value)
    arg match {
      case name: Type.Name if childNames.contains(name.value) =>
        Right(Some(name.value -> parameter.name.value))
      case _ if !mentionsAny(arg, childNames) && isBottomPinned(parameter, arg) => Right(None)
      case _                                                                    =>
        Left(
          s"sealed subtype ${subtype.name} leaves a parameter of $parentName unpinned: " +
            arg.syntax.replaceAll("\\s+", " ")
        )
    }
  }

  // Every subtype parameter must stand for exactly one slot: one bound to none ranges over every
  // type, one bound to two equates them.
  private def bindsEachOnce(
      bindings: List[(String, String)],
      subtype: Subtype,
      parentName: String
  ): Either[String, List[(String, String)]] = {
    val names = subtype.params.map(_.name.value)
    def offending(count: Int => Boolean) =
      names.filter(n => count(bindings.count(_._1 == n))).mkString(", ")
    if (names.exists(n => !bindings.exists(_._1 == n)))
      Left(s"sealed subtype ${subtype.name} binds ${offending(_ == 0)} to no slot of $parentName")
    else if (bindings.groupBy(_._1).exists(_._2.size > 1))
      Left(
        s"sealed subtype ${subtype.name} binds ${offending(_ > 1)} to several slots of $parentName"
      )
    else Right(bindings)
  }

  // One subtype's bindings, checked parameter by parameter against the slots they bind.
  final private case class Fit(
      parent: Parent,
      subtype: Subtype,
      bindings: List[(String, String)]
  ) {

    private def param(ps: List[Type.Param], name: String) = ps.find(_.name.value == name)

    // A binding admits every instantiation as its slot does when the subtype parameter is bounded
    // as the slot is and varies as it does — the standard constructors that answer rests on, or
    // why it does not. A contravariant slot holds `C[u]` for every supertype `u`: `C[Any]("x")` is
    // a `P[Boolean]` of `P[-A]` whatever `C`'s field says, unless `C` is contravariant too. A
    // covariant slot holds `C[u]` for every subtype: a value of `C[u]` is one of `C[t]` when `C` is
    // covariant, or when no field holds `u` where a `t` could not stand (a function's domain).
    def misfit(binding: (String, String)): Either[String, List[String]] = {
      val (childName, slotName) = binding
      param(subtype.params, childName).zip(param(parent.params, slotName)) match {
        case None                => Right(Nil)
        case Some((child, slot)) =>
          val name = s"${subtype.name}.$childName"
          def refusal(v: String) =
            s"sealed subtype parameter $name of a $v slot of ${parent.name} is not $v"
          if (!sameBounds(child, slot))
            Left(
              s"sealed subtype parameter $name is bounded otherwise than its slot of ${parent.name}"
            )
          else
            (Variance.of(slot), Variance.of(child)) match {
              case (Variance.Contravariant, v) if v != Variance.Contravariant =>
                Left(refusal("contravariant"))
              case (Variance.Covariant, Variance.Invariant) =>
                subtype.fields
                  .flatMap(fs =>
                    Traversals.defined(fs.map(Variance.holdsBelow(_, childName, subtype.declared)))
                  )
                  .map(_.flatten)
                  .toRight(refusal("covariant"))
              case _ => Right(Nil)
            }
      }
    }

    // A bound spelled as the slot's means what the slot's does — `C[A <: X] extends P[A]` is a
    // member of every `P[t]` only when `X` is the slot's `X` — when each parameter it names is the
    // subtype parameter bound to the slot parameter of that name, and every other name reads in
    // the same scope: the subtype is declared beside the parent with no import between them. A
    // subtype in an object reads its bound where the object's names shadow the parent's.
    private def sameBounds(child: Type.Param, slot: Type.Param): Boolean = {
      def bounds(p: Type.Param) = (p.bounds.lo.map(_.syntax), p.bounds.hi.map(_.syntax))
      val words = (child.bounds.lo.toList ++ child.bounds.hi).flatMap(TypeName.words).toSet
      val childNames = subtype.params.map(_.name.value).toSet
      val parentNames = parent.params.map(_.name.value).toSet
      def means(word: String): Boolean =
        if (childNames(word) || parentNames(word)) bindings.contains(word -> word)
        else besideParent(child, slot)
      bounds(child) == bounds(slot) && words.forall(means)
    }

  }

  // Whether two parameters' definitions are statements of one list with no import between them,
  // where every name outside the two definitions reads alike.
  private def besideParent(child: Type.Param, slot: Type.Param): Boolean = {
    def definition(p: Type.Param) = p.parent.flatMap(_.parent)
    definition(child).zip(definition(slot)).exists { (c, s) =>
      c.parent.zip(s.parent).exists { (cp, sp) =>
        val stats = cp.children
        val (i, j) = (stats.indexWhere(_ eq c), stats.indexWhere(_ eq s))
        (cp eq sp) && !stats.slice(i.min(j), i.max(j)).exists(_.is[Import])
      }
    }
  }

  /** The callee of an `extends` clause, when it is a plain (unqualified) name: either bare or
    * applied — `Focus` or `Focus[S, A]` — but never a selection like `data.Focus`, whose meaning
    * depends on resolution this lookup does not model here.
    */
  def plainCallee(tpe: Type): Option[String] = tpe match {
    case n: Type.Name  => Some(n.value)
    case a: Type.Apply =>
      a.tpe match {
        case n: Type.Name => Some(n.value)
        case _            => None
      }
    case _ => None
  }

  private def mentionsAny(tpe: Type, names: List[String]): Boolean =
    TypeName.words(tpe).iterator.exists(names.contains)

  // Nothing inhabits every covariant slot; Any is the only value of a contravariant slot pinned
  // from below — both make the subtype a member of every instantiation, which is all the sum
  // needs. An invariant slot pinned to any other type is instantiation-dependent, which this
  // fragment does not track.
  private def isBottomPinned(parameter: Type.Param, arg: Type): Boolean =
    (arg, Variance.of(parameter)) match {
      case (name: Type.Name, Variance.Covariant)     => name.value == "Nothing"
      case (name: Type.Name, Variance.Contravariant) => name.value == "Any"
      case _                                         => false
    }

}
