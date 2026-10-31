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
    else if (indexer.givenTargets.contains((file, entry.name)))
      Left(s"sealed family has a given instance outside the fragment: ${entry.name}")
    else if (indexer.anonymousTargets.contains((file, entry.name)))
      Left(s"sealed family has anonymous instances in its file: ${entry.name}")
    else if (throughAlias(file, entry.name))
      Left(s"sealed family is extended through an alias: ${entry.name}")
    else if (indexer.innerChildren.contains((file, entry.name)))
      Left(
        s"sealed subtype is an inner or local class: ${indexer.innerChildren((file, entry.name)).head}"
      )
    // A sealed family with no subtypes in its file has none anywhere: the compiler's own
    // promise makes the empty enumeration an empty sum, and the empty sum says 0.
    else
      sequence(children.map(sealedCase(_, family.copy(seen = seen :+ entry.name)))).map(_.flatten)
  }

  // Children are registered by the parent name their clause spells, so a child or an anonymous
  // instance written against an alias of the family (`type SA = S; new SA {}`) is not among them.
  // Any alias whose body mentions the family, or such an alias, used as a parent in the family's
  // file makes the enumeration incomplete.
  private def throughAlias(file: String, family: String): Boolean = {
    val aliases = indexer.aliasesOf(family)
    aliases.exists(alias =>
      indexer.subtypes.contains((file, alias)) ||
        indexer.innerChildren.contains((file, alias)) ||
        indexer.anonymousTargets((file, alias)) ||
        indexer.givenTargets((file, alias))
    )
  }

  private def sealedCase(child: TypeEntry, family: Family): Either[String, List[List[Type]]] = {
    val Family(parent, level, _, useSite) = family
    if (!accessible(child.tree, child.owner, useSite))
      Left(s"sealed subtype not visible at use site: ${child.name}")
    else
      child.tree match {
        case d: Defn.Object =>
          slotBindings(d.templ.inits, parent.name, level, Nil).map(_ => List(Nil))
        case d: (Defn.Class | Defn.Trait) if intermediate(d) => sealedIntermediate(child, d, family)
        case c: Defn.Class if isCaseClass(c) || c.mods.exists(_.is[Mod.Final]) =>
          slotBindings(c.templ.inits, parent.name, level, c.tparamClause.values)
            .flatMap(bindings => sealedLeaf(c, child.owner, useSite).map(renamed(_, bindings)))
        case _ =>
          Left(s"sealed subtype outside the closed fragment: ${child.name}")
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
    slotBindings(
      declaration.templ.inits,
      family.entry.name,
      family.level,
      declaration.tparamClause.values
    ).flatMap { bindings =>
      if (!Counter.isSealed(declaration))
        Left(s"sealed family has an open abstract subtype: ${child.name}")
      else
        sealedCases(Family(child, declaration, family.seen, family.useSite))
          .map(renamed(_, bindings))
    }

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

  // Every slot of the parent's application in the child's `extends` clause, resolved: a child
  // parameter bound to the slot, or the slot pinned bottom-up. Child parameters must bind exactly
  // one slot each, and must not spell a name any enclosing level owns.
  private def slotBindings(
      inits: List[Init],
      parentName: String,
      level: Defn.Class | Defn.Trait | Defn.Enum,
      childParams: List[Type.Param]
  ): Either[String, List[(String, String)]] = {
    val childNames = childParams.map(_.name.value)
    parentArguments(inits, parentName).flatMap { args =>
      val levelParams = level.tparamClause.values
      if (args.size != levelParams.size)
        Left(
          s"sealed subtype pins $parentName with ${args.size} of its ${levelParams.size} parameters"
        )
      else
        sequence(args.zip(levelParams).map(slotBinding(_, _, childNames, parentName)))
          .flatMap(perSlot => bindsEachOnce(perSlot.flatten, childNames, parentName))
    }
  }

  // The arguments the child's clause gives the parent: none for a bare name.
  private def parentArguments(inits: List[Init], parentName: String): Either[String, List[Type]] =
    inits.find(init => plainCallee(init.tpe).contains(parentName)) match {
      case None =>
        Left(s"sealed subtype reaches $parentName only through a qualified name: $parentName")
      case Some(init) =>
        init.tpe match {
          case application: Type.Apply => Right(application.argClause.values.toList)
          case _: Type.Name            => Right(Nil)
          case _ => Left(s"sealed subtype application unreadable: $parentName")
        }
    }

  private def slotBinding(
      arg: Type,
      parameter: Type.Param,
      childNames: List[String],
      parentName: String
  ): Either[String, Option[(String, String)]] =
    arg match {
      case name: Type.Name if childNames.contains(name.value) =>
        Right(Some(name.value -> parameter.name.value))
      case _ if !mentionsAny(arg, childNames) && isBottomPinned(parameter, arg) => Right(None)
      case _                                                                    =>
        Left(
          s"sealed subtype leaves a parameter of $parentName unpinned: " +
            arg.syntax.replaceAll("\\s+", " ")
        )
    }

  private def bindsEachOnce(
      bindings: List[(String, String)],
      childNames: List[String],
      parentName: String
  ): Either[String, List[(String, String)]] =
    if (
      bindings.groupBy(_._1).exists(_._2.size > 1) ||
      childNames.exists(name => !bindings.exists(_._1 == name))
    )
      Left(s"sealed subtype parameter is not pinned exactly once by $parentName: $childNames")
    else Right(bindings)

  // The callee of an `extends` clause, when it is a plain (unqualified) name: either bare or
  // applied — `Focus` or `Focus[S, A]` — but never a selection like `data.Focus`, whose meaning
  // depends on resolution this lookup does not model here.
  private def plainCallee(tpe: Type): Option[String] = tpe match {
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
    (
      arg,
      parameter.mods.exists(_.is[Mod.Covariant]),
      parameter.mods.exists(_.is[Mod.Contravariant])
    ) match {
      case (name: Type.Name, true, _) => name.value == "Nothing"
      case (name: Type.Name, _, true) => name.value == "Any"
      case _                          => false
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
  // every `G[A]`. Its own parameters are the ones it declares or, failing that, the enum's
  // parameters its fields or its `extends` clause mention — Scala gives `case K(a: A) extends G[A]`
  // and `case K(n: Boolean) extends G[A]` the parameter `A` (3.9.0 fixtures: `G.K[String](…)`).
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
    val mentioned = parameters.flatMap(_.decltpe) ++ c.inits.map(_.tpe)
    val own =
      if (c.tparamClause.values.nonEmpty) c.tparamClause.values
      else e.tparamClause.values.filter(p => mentioned.exists(mentionsAny(_, List(p.name.value))))
    if (c.inits.isEmpty) fields
    else
      slotBindings(c.inits, e.name.value, e, own).flatMap(bindings =>
        fields.map(rename(_, bindings))
      )
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
