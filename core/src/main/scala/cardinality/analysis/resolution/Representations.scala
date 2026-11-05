package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.model.{MemberSyntax, Throws}
import cardinality.analysis.{
  ConstrainedReceivers,
  MethodAnalysis,
  Resolver,
  SourceIndex,
  StdlibModel
}
import cardinality.capacity.Counter
import cardinality.types.{Traversals, *}

import MethodAnalysis.{Frame, TypeEntry}

/** A source type's parameters and the cases its representation is built from: one case of one type
  * for an alias, one case of its fields for a case class, one per case for an enum or a closed
  * sealed family. A declaration the model cannot read carries the reason instead.
  */
final private[cardinality] class Representations(
    indexer: SourceIndex,
    packageGrant: (List[Mod], List[String]) => Option[List[String]],
    resolver: Resolver
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
      case c: Defn.Class => caseClassRepresentation(entry, c, key)
      case e: Defn.Enum  => enumRepresentation(e, key, entry.owner)
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
    val Family(entry, level, seen, _) = family
    val file = entry.owner.chain.last.id
    val children = indexer.subtypes.getOrElse((file, entry.name), Nil)
    if (seen.contains(entry.name)) Left(s"recursive sealed family: ${entry.name}")
    else if (mayThrow(level)) Left(s"sealed family construction not resolved: ${entry.name}")
    else if (indexer.typesInFile(file, entry.name).size != 1)
      Left(s"ambiguous sealed family: ${entry.name}")
    // An abstract member of the empty type declared on the family's level (the root, or an
    // intermediate through the recursion): every leaf inherits it, and no pure, total body
    // implements `Nothing`, so the family enumerates no value - the empty sum says 0 (absurd
    // elimination, the reading C4's contract rule gives a record's members).
    else if (empties(level)) Right(Nil)
    // A sealed family with no subtypes in its file has none anywhere: the compiler's own
    // promise makes the empty enumeration an empty sum, and the empty sum says 0.
    else
      indexer
        .unnamedInstances(file, entry.name)
        .fold(
          Traversals
            .sequence(children.map(sealedCase(_, family.copy(seen = seen :+ entry.name))))
            .map(_.flatten)
        )(Left(_))
  }

  // An abstract declaration of the empty type: `Nothing` as written (an alias of it is refound
  // through the resolution the cases' types run, so the spelling is what this check reads).
  private def empties(level: Defn.Class | Defn.Trait): Boolean =
    level.templ.body.stats.exists {
      case d: (Decl.Def | Decl.Val) =>
        d.decltpe match {
          case Type.Name("Nothing") => true
          case _                    => false
        }
      case _ => false
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
          case _: Defn.Object                                               => None
          case c: Defn.Class if closedLeaf(c) && openLeaf(child, c).isEmpty => None
          case d: (Defn.Class | Defn.Trait) if Counter.isSealed(d)          =>
            openSubtype(child, seen + entry.key)
          case _ => Some(child.name)
        }
      })
  }

  private def sealedCase(child: TypeEntry, family: Family): Either[String, List[List[Type]]] = {
    val Family(parent, level, _, useSite) = family
    child.tree match {
      case t: (Defn.Object | Defn.Class) if mayThrow(t) =>
        Left(s"sealed family construction not resolved: ${child.name}")
      case d: Defn.Object =>
        SealedSlots
          .instantiation(slots(parent.name, level), SealedSlots.subtype(d, inFile(d)))
          .map(_ => List(Nil))
      case d: (Defn.Class | Defn.Trait) if intermediate(d) => sealedIntermediate(child, d, family)
      case c: Defn.Class if closedLeaf(c)                  =>
        openLeaf(child, c)
          .map(why => s"sealed family leaf may be extended: ${child.name} ($why)")
          .toLeft(())
          .flatMap { _ =>
            SealedSlots
              .instantiation(slots(parent.name, level), SealedSlots.subtype(c, inFile(c)))
              .flatMap(i => sealedLeaf(c, child, useSite).map(renamed(_, i.bindings)))
          }
      case _ =>
        Left(s"sealed subtype outside the closed fragment: ${child.name}")
    }
  }

  // A leaf no file outside the family's extends: a case class or a final class.
  private def closedLeaf(c: Defn.Class): Boolean =
    Representations.caseClass(c) || c.mods.exists(_.is[Mod.Final])

  // A case class that is not final may be extended all the same, and a subclass may override a
  // member it has (`Overrides.openLeaf`): each parent is read as its clause's name resolves where
  // it is written — a supplied class or trait, or the members the stdlib model lists of a parent it
  // summarizes (none for a root or marker type, nor for `Product` and `Equals`, whose members the
  // compiler synthesizes for a case class).
  private def openLeaf(child: TypeEntry, c: Defn.Class): Option[String] =
    child.self.fold(Some(s"leaf not read: ${child.name}")) { frame =>
      Overrides.openLeaf(c, frame, Overrides.ancestry(frame, leafParents), leafParents)
    }

  private object leafParents extends Overrides.Parents[Frame] {

    def body(level: Frame): List[Stat] = level.stats

    def fields(level: Frame): List[Term.Param] = level.tree.toList.flatMap(MemberSyntax.fieldParams)

    def key(level: Frame): String = level.id

    def parents(level: Frame): List[(String, Overrides.Parent[Frame])] =
      level.parents.map(init => TypeApplications.parentName(init) -> parent(init, level))

    private def parent(init: Init, level: Frame): Overrides.Parent[Frame] =
      resolver.parent(init, level) match {
        case Right(Some((entry, _))) =>
          TypeIdentities.classFrame(entry).fold(Overrides.Parent.Unread)(Overrides.Parent.Source(_))
        case Right(None) =>
          resolver
            .outsideParent(init, level)
            .filter(_.shadow.isEmpty)
            .flatMap(outside => StdlibModel.leafParent(outside.path))
            .fold(Overrides.Parent.Unread)(Overrides.Parent.Library(_))
        case Left(_) => Overrides.Parent.Unread
      }

  }

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
  // declared, built by a public constructor, and each field permitted at the use site. The values
  // a leaf holds are what its public constructors build (`ConstrainedReceivers`): a restriction on
  // the leaf itself is disregarded, a restricted constructor ignored, and a leaf no public
  // constructor builds whole is refused. A field's qualifier decides whether this reading may
  // project it at all: an unqualified `private`, `protected` or `implicit` refuses the family, and
  // so does a plain class's parameter written without `val` or `var`: it is no member (E173), so
  // values that differ only there may be one value. A `var` field holds the argument it was given, as a `val` does (the
  // model has no mutation). A case class body may hold methods — members add no
  // values to the space — but a plain class must carry `final`, or outside subtypes would extend
  // the family past its file.
  private def sealedLeaf(
      c: Defn.Class,
      child: TypeEntry,
      useSite: Frame
  ): Either[String, List[List[Type]]] =
    if (c.ctor.paramClauses.size != 1)
      Left(s"sealed subtype shape unreadable: ${c.name.value}")
    else {
      val parameters = c.ctor.paramClauses.toList.flatMap(_.values)
      val fields = Representations.caseClass(c) || parameters.forall(MemberSyntax.keptParameter)
      if (!fields || parameters.exists(p => !usableWhere(p.mods, child.owner, useSite)))
        Left(s"sealed subtype hides a parameter: ${c.name.value}")
      else
        built(child, useSite).flatMap(_ => declaredFields(parameters, c.name.value).map(List(_)))
    }

  // Whether a leaf's public constructors build its whole field space, or why not.
  private def built(child: TypeEntry, useSite: Frame): Either[String, Unit] =
    child.self.fold(Right(())) { frame =>
      ConstrainedReceivers
        .space(frame, useSite, resolver, resolver.resolve(_, frame, resolver.typeParameters(frame)))
        .flatMap(
          _.map(_ =>
            s"sealed subtype built by public constructors with fewer values: ${child.name}"
          )
            .toLeft(())
        )
    }

  // Mods that grant their declaration to an enclosing package are readable wherever that package
  // reaches; anything else that restricts access is a refusal here.
  private def usableWhere(mods: List[Mod], childOwner: Frame, useSite: Frame): Boolean =
    mods.forall {
      case _: (Mod.ValParam | Mod.VarParam) => true
      case m: Mod.Private                   =>
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
  private def enumRepresentation(e: Defn.Enum, key: String, owner: Frame): Representation =
    if (e.mods.exists(_.is[Mod.Abstract])) abstractRepresentation(key)
    else if (
      (e :: e.templ.body.stats.collect { case c: Defn.EnumCase => c }).exists(mayThrow(_)) ||
      singletons(e) && companion(e, owner).exists(mayThrow(_))
    ) (e.tparamClause.values, Left(s"enum construction not resolved: $key"))
    else (e.tparamClause.values, enumCases(e, key))

  // A summand whose construction only throws has no value (register D26). Building one evaluates its
  // parents' arguments, its own body and the body of every source template it extends: any of the
  // simple name its `extends` clause spells (an alias of that name may stand for any), so none is
  // missed. This reading resolves no term names, so it cannot tell whether a body's `???` is the
  // root imports' one: such a body is refused for that gap, never read as built or as empty.
  private def mayThrow(tree: Tree, seen: Set[String] = Set.empty): Boolean = {
    val file = fileOf.get(tree.pos.input)
    tree match {
      case c: Defn.EnumCase => extendsThrowing(c.inits, seen, file)
      case _                =>
        TypeApplications.template(tree).exists { template =>
          Throws.evaluated(template.body.stats, None).exists(Throws.only(_, _ => true)) ||
          extendsThrowing(template.inits, seen, file)
        }
    }
  }

  // Every type declaration by its simple name, indexed once.
  private lazy val typesNamed = indexer.types.toList.groupBy(_.name)

  // The file each indexed tree was parsed from.
  private lazy val fileOf: Map[scala.meta.inputs.Input, String] =
    indexer.types.map(e => e.tree.pos.input -> e.owner.input).toMap

  // The names each file's imports spell; none (anything) where an import is not followed.
  private lazy val importedIn: Map[String, Option[Set[String]]] =
    indexer.imports.toList.groupMap(_._1.takeWhile(_ != ':'))(_._2).map { (file, bindings) =>
      file -> Option.when(bindings.forall(_.unfollowed.isEmpty))(
        bindings
          .flatMap(b => b.named.flatMap(_._2) ++ b.wildcards.flatten ++ b.givens.flatten)
          .toSet
      )
    }

  private lazy val exportedNames: Set[String] =
    indexer.exports.toList.flatMap(_._2.prefix.split('.')).toSet

  // Whether a parent's simple name, spelled in a file, may denote a declaration. A type nested only
  // in objects is named bare in its own file, where an import of the file names one of those
  // objects, or wherever an export (which a class's subclasses inherit) names one; a package's type
  // or one in a class or trait (which inheritance passes on) always may.
  private def reachable(e: TypeEntry, file: Option[String]): Boolean = {
    val containers = e.owner.chain.takeWhile(f => !resolver.isPackage(f) && f.parent.nonEmpty)
    def named(f: Frame) = f.name.exists(n =>
      exportedNames(n) || file.forall(importedIn.getOrElse(_, Some(Set.empty)).forall(_(n)))
    )
    file.forall(_ == e.owner.input) || containers.isEmpty ||
    containers.exists(f => !f.tree.exists(_.is[Defn.Object])) || containers.exists(named)
  }

  private def extendsThrowing(
      parents: List[Init],
      seen: Set[String],
      file: Option[String]
  ): Boolean =
    Throws.arguments(parents).exists(Throws.only(_, _ => true)) ||
      parents.map(TypeApplications.parentName).filterNot(seen).exists { parent =>
        typesNamed
          .getOrElse(parent, Nil)
          .filter(reachable(_, file))
          .exists(e => e.tree.is[Defn.Type] || mayThrow(e.tree, seen + parent))
      }

  // A singleton case is a `val` of the enum's companion, built with it (a class case is not: a
  // 3.9.0 run builds `F.C(true)` while the companion's initializer throws).
  private def singletons(e: Defn.Enum): Boolean = e.templ.body.stats.exists(singleton)

  private def singleton(stat: Stat): Boolean = stat match {
    case c: Defn.EnumCase => c.ctor.paramClauses.isEmpty && c.tparamClause.values.isEmpty
    case other            => other.is[Defn.RepeatedEnumCase]
  }

  private def companion(e: Defn.Enum, owner: Frame): Option[Defn.Object] =
    owner.stats.collectFirst { case o: Defn.Object if o.name.value == e.name.value => o }

  private def enumCases(e: Defn.Enum, key: String): Either[String, List[List[Type]]] =
    Traversals.sequence(
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
  // list, no constructor modifiers, no parents, no members and no hidden parameter. A secondary
  // constructor is no member: it delegates to the primary one, so it builds no other value. A final
  // one with more is a final class's record (`CapabilityRecords`); one no `final` closes may be
  // extended by a class overriding what it has, so its values are not its fields'.
  private def caseClassRepresentation(
      entry: TypeEntry,
      c: Defn.Class,
      key: String
  ): Representation =
    if (Representations.plainCase(c)) plainRepresentation(c, key)
    else if (c.mods.exists(_.is[Mod.Final])) abstractRepresentation(key)
    else
      (
        Nil,
        Left(
          s"case class open to extension: $key" +
            openLeaf(entry, c).fold("")(why => s" ($why)")
        )
      )

  private def plainRepresentation(c: Defn.Class, key: String): Representation =
    (
      c.tparamClause.values,
      declaredFields(c.ctor.paramClauses.toList.flatMap(_.values), key).map(List(_))
    )

  // A field without a declared type leaves the product unreadable.
  private def declaredFields(fields: List[Term.Param], key: String): Either[String, List[Type]] =
    Traversals.sequence(fields.map(_.decltpe.toRight(s"missing field type: $key")))

}

private[cardinality] object Representations {
  type Representation = (List[Type.Param], Either[String, List[List[Type]]])

  /** A concrete case class. */
  def caseClass(c: Defn.Class): Boolean =
    c.mods.exists(_.is[Mod.Case]) && !c.mods.exists(_.is[Mod.Abstract])

  /** A case class whose single parameter list is its whole product, read as that product. */
  def plainCase(c: Defn.Class): Boolean =
    caseClass(c) && c.ctor.paramClauses.size == 1 && c.ctor.mods.isEmpty &&
      c.templ.inits.isEmpty && c.templ.body.stats.forall(_.is[Ctor.Secondary]) &&
      !c.ctor.paramClauses.flatMap(_.values).exists(_.mods.exists(MemberSyntax.restricted))

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
