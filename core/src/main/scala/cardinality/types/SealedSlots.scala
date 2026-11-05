package cardinality.types

import scala.annotation.unused
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
  * `C[t]` when `C` varies the same way (`C[u] <: C[t]`). A slot pinned to any other type (a GADT
  * index such as `extends Box[Int]`), a subtype parameter no slot binds, or a parameter that varies
  * otherwise has no summand: each is a refusal.
  */
private[cardinality] object SealedSlots {

  // Trees are parsed and reprinted under Scala 3 dialect rules.
  private given scala3: Dialect = dialects.Scala3

  /** A sealed parent as its subtypes' clauses instantiate it: its name and type parameters. */
  final case class Parent(name: String, params: List[Type.Param])

  /** A subtype as its clause reads: its name, its `extends` clause and its own type parameters. */
  final case class Subtype(name: String, inits: List[Init], params: List[Type.Param])

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

  /** A subtype definition as `instantiation` reads it. `declared` is unread. */
  def subtype(
      d: Defn.Object | Defn.Class | Defn.Trait | Defn.Enum,
      @unused declared: String => Option[List[Variance]]
  ): Subtype = {
    val params = d match {
      case _: Defn.Object                           => Nil
      case p: (Defn.Class | Defn.Trait | Defn.Enum) => p.tparamClause.values
    }
    Subtype(d.name.value, d.templ.inits, params)
  }

  /** No variances: the covariant-field reading that asked for them is gone. */
  def declaredIn(d: Tree, denotes: (Tree, String) => Boolean)(
      name: String
  ): Option[List[Variance]] = None

  /** How an enum case with its own `extends` clause instantiates its enum. `declared` is unread. */
  def enumCase(
      c: Defn.EnumCase,
      e: Defn.Enum,
      @unused declared: String => Option[List[Variance]]
  ): Either[String, Instantiation] =
    instantiation(
      Parent(e.name.value, e.tparamClause.values),
      Subtype(c.name.value, c.inits, caseParams(c, e))
    )

  /** How the subtype instantiates the parent, or why its clause does not instantiate it uniformly.
    */
  def instantiation(parent: Parent, subtype: Subtype): Either[String, Instantiation] =
    parentArguments(subtype, parent.name).flatMap { args =>
      if (args.size != parent.params.size)
        Left(s"sealed subtype pins its parent with another number of arguments: ${subtype.name}")
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
        Left(
          s"sealed subtype reaches its parent only through a qualified name: ${subtype.name} " +
            s"(of $parentName)"
        )
      case Some(init) =>
        init.tpe match {
          case application: Type.Apply => Right(application.argClause.values.toList)
          case _: Type.Name            => Right(Nil)
          case _ => Left(s"sealed subtype applies its parent unreadably: ${subtype.name}")
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
          s"sealed subtype leaves a parameter of its parent unpinned: ${subtype.name} " +
            s"(${Spelling.of(arg)}, of $parentName)"
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
      Left(
        s"sealed subtype binds a parameter to no slot of its parent: ${subtype.name} " +
          s"(${offending(_ == 0)}, of $parentName)"
      )
    else if (bindings.groupBy(_._1).exists(_._2.size > 1))
      Left(
        s"sealed subtype binds a parameter to several slots of its parent: ${subtype.name} " +
          s"(${offending(_ > 1)}, of $parentName)"
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
    // a `P[Boolean]` of `P[-A]` whatever `C`'s field says, unless `C` is contravariant too; a
    // covariant slot holds `C[u]` for every subtype, so an invariant `C` is refused.
    def misfit(binding: (String, String)): Either[String, List[String]] = {
      val (childName, slotName) = binding
      param(subtype.params, childName).zip(param(parent.params, slotName)) match {
        case None                => Right(Nil)
        case Some((child, slot)) =>
          val name = s"${subtype.name}.$childName"
          def refusal(v: String) =
            s"sealed subtype parameter of a $v slot is not $v: $name (of ${parent.name})"
          if (!sameBounds(child, slot))
            Left(
              s"sealed subtype parameter is bounded otherwise than its slot: $name (of ${parent.name})"
            )
          else
            (Variance.of(slot), Variance.of(child)) match {
              case (Variance.Contravariant, v) if v != Variance.Contravariant =>
                Left(refusal("contravariant"))
              case (Variance.Covariant, Variance.Invariant) => Left(refusal("covariant"))
              case _                                        => Right(Nil)
            }
      }
    }

    // Unbounded on both sides: a bound that names anything is refused, whatever it means.
    private def sameBounds(child: Type.Param, slot: Type.Param): Boolean = {
      def bounds(p: Type.Param) = (p.bounds.lo.map(_.syntax), p.bounds.hi.map(_.syntax))
      bounds(child) == bounds(slot) && child.bounds.lo.isEmpty && child.bounds.hi.isEmpty
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

  // Nothing inhabits every covariant slot, which makes the subtype a member of every
  // instantiation, all the sum needs. Any other pin is instantiation-dependent, which this fragment
  // does not track.
  private def isBottomPinned(parameter: Type.Param, arg: Type): Boolean =
    (arg, Variance.of(parameter)) match {
      case (name: Type.Name, Variance.Covariant) => name.value == "Nothing"
      case _                                     => false
    }

}
