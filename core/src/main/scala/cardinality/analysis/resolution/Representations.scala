package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.capacity.Counter
import cardinality.types.*

import MethodAnalysis.{sequence, Frame, TypeEntry}

/** A source type's parameters and the cases its representation is built from: one case of one type
  * for an alias, one case of its fields for a case class, one per case for an enum or a closed
  * sealed family. A declaration the model cannot read carries the reason instead.
  */
final private[cardinality] class Representations(
    indexer: SourceIndex,
    accessible: (Tree, Frame, Frame) => Boolean,
    packageGrant: (List[Mod], List[String]) => Option[List[String]]
) {
  import Representations.{Family, Representation}

  def of(entry: TypeEntry, key: String, useSite: Frame): Representation =
    entry.tree match {
      case a: Defn.Type =>
        (a.tparamClause.values, Right(List(List(a.body))))
      case c: Defn.Class if Counter.isSealed(c) && Counter.isAbstractClass(c) =>
        (c.tparamClause.values, sealedCases(Family(entry, c, Nil, useSite)))
      case t: Defn.Trait if Counter.isSealed(t) =>
        (t.tparamClause.values, sealedCases(Family(entry, t, Nil, useSite)))
      case d: (Defn.Trait | Defn.Class) if Representations.typesOnly(d) =>
        (d.tparamClause.values, Right(List(Nil)))
      case c: Defn.Class => caseClassRepresentation(c, key)
      case e: Defn.Enum  => enumRepresentation(e, key)
      case _             => abstractRepresentation(key)
    }

  // `sealed` is the compiler's promise that every direct subtype of a trait or abstract class is
  // declared in its own source file. Where that promise holds, the model reads the family as a
  // tagged sum: an object contributes one value, a case class or a final class with declared
  // parameters contributes the product of those parameters, and a sealed abstract intermediate is
  // enumerated in turn. A subtype the model cannot read whole — open extension, fields invisible at
  // the use site, a parameter left unpinned — leaves the whole parent unreadable: a sum with one
  // missing branch is a wrong sum, not a partial answer. The promise covers named subtypes only:
  // an anonymous instance (`new P {…}`) or a `given` instance in the file is a value of the family
  // that no case enumerates, so such a family is refused rather than read without it. Subtype slots
  // must name the parent's parameters directly, or pin them bottom-up (nothing into covariant
  // slots, anything into contravariant ones); a slot pinned to a fixed type would make membership
  // depend on the caller's instantiation, which this model does not track.
  private def sealedCases(family: Family): Either[String, List[List[Type]]] = {
    val Family(entry, _, seen, _) = family
    val file = entry.owner.chain.last.id
    val children = indexer.subtypes.getOrElse((file, entry.name), Nil)
    if (seen.contains(entry.name)) Left(s"recursive sealed family: ${entry.name}")
    else if (indexer.typesInFile(file, entry.name).size != 1)
      Left(s"ambiguous sealed family: ${entry.name}")
    // A sealed family with no subtypes in its file has none anywhere: the compiler's own
    // promise makes the empty enumeration an empty sum, and the empty sum says 0.
    else
      indexer
        .unnamedInstances(file, entry.name)
        .fold(
          sequence(children.map(sealedCase(_, family.copy(seen = seen :+ entry.name))))
            .map(_.flatten)
        )(Left(_))
  }

  /** A subtype of a sealed family through which a file outside the family's may extend it: a trait,
    * an abstract class or a plain class that is neither sealed nor final, at any depth of the
    * family's sealed intermediates.
    */
  def openSubtype(entry: TypeEntry, seen: Set[String] = Set.empty): Option[String] = {
    val file = entry.owner.chain.last.id
    indexer.subtypes
      .getOrElse((file, entry.name), Nil)
      .filterNot(child => seen(child.key))
      .collectFirst(Function.unlift { child =>
        child.tree match {
          case _: Defn.Object                                      => None
          case c: Defn.Class if closedLeaf(c)                      => None
          case d: (Defn.Class | Defn.Trait) if Counter.isSealed(d) =>
            openSubtype(child, seen + entry.key)
          case _ => Some(child.name)
        }
      })
  }

  private def sealedCase(child: TypeEntry, family: Family): Either[String, List[List[Type]]] = {
    val Family(parent, level, _, useSite) = family
    if (!accessible(child.tree, child.owner, useSite))
      Left(s"sealed subtype not visible at use site: ${child.name}")
    else
      child.tree match {
        case d: Defn.Object =>
          SealedSlots
            .instantiation(slots(parent.name, level), SealedSlots.subtype(d, inFile(d)))
            .map(_ => List(Nil))
        case d: (Defn.Class | Defn.Trait) if intermediate(d) => sealedIntermediate(child, d, family)
        case c: Defn.Class if closedLeaf(c)                  =>
          SealedSlots
            .instantiation(slots(parent.name, level), SealedSlots.subtype(c, inFile(c)))
            .flatMap(i => sealedLeaf(c, child.owner, useSite).map(renamed(_, i.bindings)))
        case _ =>
          Left(s"sealed subtype outside the closed fragment: ${child.name}")
      }
  }

  // A leaf no file outside the family's extends: a case class or a final class.
  private def closedLeaf(c: Defn.Class): Boolean = isCaseClass(c) || c.mods.exists(_.is[Mod.Final])

  private def intermediate(declaration: Defn.Class | Defn.Trait): Boolean = declaration match {
    case c: Defn.Class => c.mods.exists(_.is[Mod.Abstract])
    case t: Defn.Trait => Counter.isSealed(t)
  }

  private def sealedIntermediate(
      child: TypeEntry,
      declaration: Defn.Class | Defn.Trait,
      family: Family
  ): Either[String, List[List[Type]]] =
    SealedSlots
      .instantiation(
        slots(family.entry.name, family.level),
        SealedSlots.subtype(declaration, inFile(declaration))
      )
      .flatMap { i =>
        if (!Counter.isSealed(declaration))
          Left(s"sealed family has an open abstract subtype: ${child.name}")
        else
          sealedCases(Family(child, declaration, family.seen, family.useSite))
            .map(renamed(_, i.bindings))
      }

  private def slots(name: String, level: Defn.Class | Defn.Trait): SealedSlots.Parent =
    SealedSlots.Parent(name, level.tparamClause.values)

  // The constructor a subtype's field names: the one its file declares under that name, when it
  // declares exactly one.
  private def inFile(d: Tree): String => Option[List[Variance]] =
    SealedSlots.declaredIn(d, (_, _) => true)

  private def renamed(cases: List[List[Type]], bindings: List[(String, String)]): List[List[Type]] =
    cases.map(rename(_, bindings))

  private def rename(fields: List[Type], bindings: List[(String, String)]): List[Type] =
    fields.map(replaceSlots(_, bindings))

  // The fields a concrete leaf stores, read as a product: one parameter clause, every parameter
  // declared, and — separately for the constructor and the fields — permission at the use site.
  // Constructing a value and projecting its fields are different rights: a `private[p]`
  // constructor hides who may build the summand, not what the space holds, while a field's
  // qualifier decides whether this reading may project it at all. An unqualified `private`,
  // `protected`, `var` or `implicit` reads neither and refuses the family. A case class body may
  // hold methods — members add no values to the space — but a plain class must carry `final`, or
  // outside subtypes would extend the family past its file.
  private def sealedLeaf(
      c: Defn.Class,
      childOwner: Frame,
      useSite: Frame
  ): Either[String, List[List[Type]]] =
    if (c.ctor.paramClauses.size != 1)
      Left(s"sealed subtype shape unreadable: ${c.name.value}")
    else if (!usableWhere(c.ctor.mods, childOwner, useSite))
      Left(s"sealed subtype constructor not visible at use site: ${c.name.value}")
    else {
      val parameters = c.ctor.paramClauses.toList.flatMap(_.values)
      if (
        parameters.exists(p =>
          p.mods.exists(_.is[Mod.VarParam]) || !usableWhere(p.mods, childOwner, useSite)
        )
      )
        Left(s"sealed subtype hides a parameter: ${c.name.value}")
      else declaredFields(parameters, c.name.value).map(List(_))
    }

  // Mods that grant their declaration to an enclosing package are readable wherever that package
  // reaches; anything else that restricts access is a refusal here.
  private def usableWhere(mods: List[Mod], childOwner: Frame, useSite: Frame): Boolean =
    mods.forall {
      case _: Mod.ValParam => true
      case m: Mod.Private  =>
        packageGrant(List(m), childOwner.path)
          .exists(prefix => useSite.chain.exists(_.path.startsWith(prefix)))
      case _ => false
    }

  private def replaceSlots(field: Type, bindings: List[(String, String)]): Type =
    TypeSubstitution.replace(
      field,
      bindings.map((from, to) => TypeName.of(from) -> (Type.Name(to): Type)).toMap
    )

  // An enum is the sum of its cases; a repeated case is one constructor per name, and a case whose
  // field type is not declared leaves the representation unreadable. A case with its own `extends`
  // clause is a subtype like any sealed family's: it must instantiate the enum's parameters with
  // its own or bottom-pin them, since `case I extends G[Unit]` is a value of `G[Unit]` only, not of
  // every `G[A]` (its own parameters: `SealedSlots.caseParams`).
  private def enumRepresentation(e: Defn.Enum, key: String): Representation =
    if (e.mods.exists(_.is[Mod.Abstract])) abstractRepresentation(key)
    else (e.tparamClause.values, enumCases(e, key))

  private def enumCases(e: Defn.Enum, key: String): Either[String, List[List[Type]]] =
    sequence(
      e.templ.body.stats.flatMap {
        case c: Defn.EnumCase         => List(enumCase(c, e, key))
        case r: Defn.RepeatedEnumCase => List.fill(r.cases.size)(Right(Nil))
        case _                        => Nil
      }
    )

  private def enumCase(c: Defn.EnumCase, e: Defn.Enum, key: String): Either[String, List[Type]] = {
    val parameters = c.ctor.paramClauses.toList.flatMap(_.values)
    val fields = declaredFields(parameters, key)
    if (c.inits.isEmpty) fields
    else
      SealedSlots.enumCase(c, e, inFile(c)).flatMap(i => fields.map(rename(_, i.bindings)))
  }

  private def abstractRepresentation(key: String): Representation =
    (Nil, Left(s"abstract type or method-valued representation: $key"))

  // Only a case class whose single parameter list *is* the product can be projected: no second
  // list, no constructor modifiers, no parents, no members and no hidden parameter.
  private def caseClassRepresentation(c: Defn.Class, key: String): Representation =
    if (!isCaseClass(c) || !isPlainProduct(c)) abstractRepresentation(key)
    else
      (
        c.tparamClause.values,
        declaredFields(c.ctor.paramClauses.toList.flatMap(_.values), key).map(List(_))
      )

  private def isCaseClass(c: Defn.Class): Boolean =
    c.mods.exists(_.is[Mod.Case]) && !c.mods.exists(_.is[Mod.Abstract])

  private def isPlainProduct(c: Defn.Class): Boolean =
    c.ctor.paramClauses.size == 1 &&
      c.ctor.mods.isEmpty &&
      c.templ.inits.isEmpty &&
      c.templ.body.stats.isEmpty &&
      !c.ctor.paramClauses.flatMap(_.values).exists(hiddenParameter)

  private def hiddenParameter(p: Term.Param): Boolean =
    p.mods.exists(m => m.is[Mod.Private] || m.is[Mod.Protected] || m.is[Mod.VarParam])

  // A field without a declared type leaves the product unreadable.
  private def declaredFields(fields: List[Term.Param], key: String): Either[String, List[Type]] =
    sequence(fields.map(_.decltpe.toRight(s"missing field type: $key")))

}

private[cardinality] object Representations {
  type Representation = (List[Type.Param], Either[String, List[List[Type]]])

  /** A trait or plain class whose only members are types (members M3): no parent, parameter or self
    * type adds a term member, so no observation tells two of its values apart — its record is
    * empty. It has a value only where it can be instantiated: not a `final abstract` class, and no
    * member with two bounds (`type X >: Int <: String` has no instance, and `>: A <: B` holds only
    * where A conforms to B). A sealed family is the sum of its cases instead, and a case class has
    * its product.
    */
  def typesOnly(d: Defn.Trait | Defn.Class): Boolean =
    !Counter.isSealed(d) && !d.mods.exists(_.is[Mod.Case]) && d.templ.inits.isEmpty &&
      d.ctor.mods.isEmpty && d.ctor.paramClauses.forall(_.values.isEmpty) &&
      d.templ.body.selfOpt.forall(_.decltpe.isEmpty) && !finalAbstract(d) &&
      d.templ.body.stats.forall(stat => stat.is[Decl.Type] || stat.is[Defn.Type]) &&
      TypeMembers.instantiable(d.templ.body.stats)

  /** A `final abstract` class: neither instantiated nor extended, so it has no value. */
  def finalAbstract(d: Defn.Trait | Defn.Class): Boolean =
    d.mods.exists(_.is[Mod.Final]) && d.mods.exists(_.is[Mod.Abstract])

  /** One family being enumerated: its entry and declaration, the families already open above it (a
    * cycle is refused) and the use site that reads it.
    */
  final case class Family(
      entry: TypeEntry,
      level: Defn.Class | Defn.Trait,
      seen: List[String],
      useSite: Frame
  )

}
