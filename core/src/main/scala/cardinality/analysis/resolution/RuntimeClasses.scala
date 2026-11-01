package cardinality.analysis.resolution

import scala.annotation.tailrec
import scala.meta.*

import cardinality.analysis.inhabitation.Inhabitation
import cardinality.analysis.model.{AliasReading, ResolutionContext}
import cardinality.analysis.{MethodAnalysis, Resolver, StdlibModel}
import cardinality.capacity.Counter
import cardinality.types.{TypeApplications, TypeSubstitution}

import Inhabitation.Shape
import MethodAnalysis.{sequence, Frame, TypeEntry}
import RuntimeClasses.*
import TypeSubstitution.{Meet, Union}

/** The class a union alternative's values carry at runtime, and when two classes provably share no
  * value.
  *
  * Two classes share no value when neither derives from the other and one is final, both are
  * classes (single inheritance), or one is sealed and each subclass it enumerates shares none with
  * the other — the class-level half of Scala 3's `provablyDisjoint`. Ancestor sets come from the
  * supplied declarations; a parent this reading cannot resolve leaves them incomplete, unless no
  * supplied declaration carries its name: a dependency's classes cannot extend the sources that
  * depend on it. The builtin classes modelled here are final, or sealed in the standard library, so
  * no source class extends one and none extends a source class.
  */
final private[cardinality] class RuntimeClasses(
    resolver: Resolver,
    types: List[TypeEntry],
    sources: Map[String, Source],
    supplied: String => Boolean
) {

  private val parentCache =
    scala.collection.mutable.Map.empty[String, List[Either[String, Option[Declared]]]]

  private val childCache =
    scala.collection.mutable.Map.empty[String, Either[String, List[Declared]]]

  private lazy val aliases = sources.values.toList.flatMap(_.collect {
    case d: Defn.Type => d.name.value -> words(d.body)
  })

  private lazy val renames = sources.values.toList.flatMap(_.collect {
    case r: Importee.Rename => r.name.value -> r.rename.value
  })

  // The names that denote one supplied class, trait or enum wherever they are written: no alias
  // and no renaming selector of the sources spells them too.
  private lazy val classNames: Set[String] =
    types.collect {
      case TypeEntry(name, _: (Defn.Class | Defn.Trait | Defn.Enum), _, _) => name
    }.toSet --
      aliases.map(_._1) -- renames.map(_._2)

  /** The runtime class of one alternative where it is written, or why it has none. */
  def alternative(tpe: Type, context: ResolutionContext): Either[String, Alternative] =
    tpe match {
      case a: Type.Apply                 => headed(a.tpe, tpe, context).map(applied)
      case Union(_, _) | Meet(_, _)      => Left(s"${spell(tpe)} has no runtime class")
      case i: Type.ApplyInfix            => headed(i.op, tpe, context).map(applied)
      case Type.Tuple(args)              => Right(Alternative(Builtin(tuple(args.size)), true))
      case s: Type.Singleton             => module(s, context)
      case _: Type.Name | _: Type.Select => headed(tpe, tpe, context)
      case other                         => Left(s"${spell(other)} has no runtime class")
    }

  /** Whether `derived` is known to derive from `base`: a positive answer needs no complete
    * ancestry.
    */
  def derives(derived: Runtime, base: Runtime): Boolean = (derived, base) match {
    case (d: Declared, b: Declared) => closure(d, strict = false).exists(_(b.id))
    case _                          => false
  }

  /** Why two runtime classes may share a value, if they may. */
  def disjoint(a: Runtime, b: Runtime): Either[String, Unit] = (a, b) match {
    case _ if a == b   => Left(s"two alternatives share runtime class ${a.label}")
    case (top: Top, _) => Left(s"union with top-like alternative ${top.label} is not a sum")
    case (_, top: Top) => Left(s"union with top-like alternative ${top.label} is not a sum")
    case (x: Declared, y: Declared) => apart(x, y, Set.empty)
    // A modelled builtin class is final, or sealed in the standard library with final subclasses:
    // no source class extends one, and two distinct ones share no instance.
    case (_: Builtin, _: Builtin) | (_: Builtin, _: Declared) | (_: Declared, _: Builtin) =>
      Right(())
  }

  /** Why a sealed family's values may come from a subclass the sources do not hold as a
    * declaration, if they may: its shape counts the subclasses it reads, so one it misses (an enum
    * case of another enum, an anonymous class, a given) would leave the sum short.
    */
  def enumerable(runtime: Runtime): Either[String, Unit] = runtime match {
    case family: Declared => complete(family, Set.empty)
    case _                => Right(())
  }

  private def complete(family: Declared, seen: Set[String]): Either[String, Unit] =
    family.tree match {
      case t: (Defn.Trait | Defn.Class) if Counter.isSealed(t) && !seen(family.id) =>
        children(family).flatMap(kids => all(kids.map(complete(_, seen + family.id))))
      case _ => Right(())
    }

  /** Why a value of this declared class may belong to a subclass the call site observes, if it may:
    * only a final class, a module, an enum, or a sealed family whose every subclass is one of
    * these, admits none.
    */
  def closed(entry: TypeEntry): Either[String, Unit] = sealedOff(declared(entry), Set.empty)

  private def sealedOff(d: Declared, seen: Set[String]): Either[String, Unit] = d.tree match {
    case tree if FinalImplementations.isFinal(tree)          => Right(())
    case _: Defn.Enum                                        => Right(())
    case _ if seen(d.id)                                     => Right(())
    case t: (Defn.Trait | Defn.Class) if Counter.isSealed(t) =>
      children(d).flatMap(kids => all(kids.map(sealedOff(_, seen + d.id))))
    case _ => Left(s"${d.label} may be extended")
  }

  private def applied(found: Alternative): Alternative = found.copy(applied = true)

  private def headed(head: Type, whole: Type, context: ResolutionContext) = {
    val name = TypeApplications.name(head)
    head match {
      case _ if context.variables.contains(name) =>
        Left(s"$name has no runtime class (type parameter or alias argument)")
      case s: Type.Select if RefinedMembers.pathType(s.qual, context.frame).nonEmpty =>
        Left(s"$name has no runtime class (path-dependent member)")
      case _ =>
        Found.named(resolver, head, name, context) match {
          case Some(Found.Entry(entry))   => declaration(entry, name, context)
          case Some(Found.Absent)         => builtin(name, whole, context)
          case Some(failed: Found.Failed) => Left(failed.reason)
          case None                       => Left(s"${spell(head)} has no runtime class")
        }
    }
  }

  // A builtin answers only where the shape reading reaches it: a binder, declaration, import or
  // inherited member that shadows the name refuses it there too.
  private def builtin(
      name: String,
      whole: Type,
      context: ResolutionContext
  ): Either[String, Alternative] =
    TopTypes
      .named(name)
      .map(Top(_))
      .orElse(builtins.get(name.stripPrefix("scala.")).map(Builtin(_)))
      .toRight(
        if (StdlibModel.entry(name).isEmpty)
          s"$name has no runtime class (declaration not supplied)"
        else s"$name has no runtime class this reading tells apart (stdlib model)"
      )
      .flatMap(found =>
        resolver.resolve(whole, context).flatMap { shape =>
          val reached = found match {
            case Top(_) => shape == TopTypes.terminal
            case _      => true
          }
          Either.cond(
            reached,
            Alternative(found, false),
            s"$name has no runtime class (not the builtin)"
          )
        }
      )

  private def declaration(entry: TypeEntry, name: String, context: ResolutionContext) =
    entry.tree match {
      case _: Defn.Class | _: Defn.Trait | _: Defn.Enum =>
        Right(Alternative(declared(entry), false))
      case _: Defn.Type if OpaqueTypes.hidden(entry, context.useSite) =>
        Left(s"$name has no runtime class at this use site (opaque type)")
      case alias: Defn.Type =>
        context
          .enterAlias(entry, resolver.typeParameters(entry.owner), AliasReading.Runtime)
          .toRight(s"recursive alias: $name")
          .flatMap(alternative(alias.body, _))
      case _ => Left(s"$name has no runtime class (abstract type member)")
    }

  // A module singleton's class is the object's own; a stable value's singleton type names one value
  // of a class this reading does not know.
  private def module(singleton: Type.Singleton, context: ResolutionContext) =
    resolver.resolve(singleton, context).flatMap {
      case Shape.Singleton(id, None) =>
        SingletonIntersections
          .moduleFrame(id, resolver)
          .flatMap(moduleClass)
          .map(Alternative(_, false))
          .toRight(s"${spell(singleton)} has no runtime class")
      case _ =>
        Left(s"${spell(singleton)} has no runtime class (a stable value's singleton type)")
    }

  private def declared(entry: TypeEntry): Declared =
    Declared(s"${entry.owner.input}:${entry.tree.pos.start}", entry.name)(entry.tree, entry.owner)

  // An object's own class, declared in the frame around it.
  private def moduleClass(frame: Frame): Option[Declared] =
    frame.tree
      .collect { case o: Defn.Object => o }
      .zip(frame.parent)
      .map((tree, owner) => Declared(frame.id, tree.name.value)(tree, owner))

  private def apart(a: Declared, b: Declared, seen: Set[String]): Either[String, Unit] =
    if (a.id == b.id) Left(s"${a.label} is one runtime class")
    else
      for {
        above <- closure(a, strict = true)
        below <- closure(b, strict = true)
        _ <- Either.cond(!above(b.id), (), s"${a.label} derives from ${b.label}")
        _ <- Either.cond(!below(a.id), (), s"${b.label} derives from ${a.label}")
        _ <- if (apartByHierarchy(a, b)) Right(()) else enumeratedApart(a -> b, seen)
      } yield ()

  // Either side's enumeration proves the pair apart; the first refusal is the reason otherwise.
  private def enumeratedApart(pair: (Declared, Declared), seen: Set[String]) = {
    val (a, b) = pair
    List(enumerated(a, b, seen), enumerated(b, a, seen)).flatten match {
      case readings if readings.contains(Right(())) => Right(())
      case first :: _                               => first
      case Nil => Left(s"${a.label} and ${b.label} may overlap (${kind(a)} and ${kind(b)})")
    }
  }

  // A sealed family's values are its enumerated subclasses' values; an enum's are its cases, each
  // final and an instance of the enum and of what its own `extends` clause adds.
  private def enumerated(
      family: Declared,
      other: Declared,
      seen: Set[String]
  ): Option[Either[String, Unit]] =
    family.tree match {
      case e: Defn.Enum                                        => Some(casesApart(e, family, other))
      case t: (Defn.Trait | Defn.Class) if Counter.isSealed(t) =>
        Some(childrenApart(family, other, seen))
      case _ => None
    }

  private def childrenApart(family: Declared, other: Declared, seen: Set[String]) =
    if (seen(family.id)) Left(s"recursive sealed family: ${family.label}")
    else
      children(family).flatMap(kids => all(kids.map(apart(_, other, seen + family.id))))

  // Cases are members of the enum's companion: their `extends` clauses read in its scope, or in the
  // enum's own enclosing scope when no companion is written.
  private def casesApart(e: Defn.Enum, family: Declared, other: Declared): Either[String, Unit] = {
    val scope = resolver.modules
      .find(_.path == family.owner.path :+ family.label)
      .getOrElse(family.owner)
    val added = e.templ.body.stats.collect { case c: Defn.EnumCase => c.inits }.flatten
    all(added.map(parent(_, scope)).map {
      case Left(name) => Left(s"ancestry of a case of ${family.label} not resolved: $name")
      case Right(Some(up)) if up.id != family.id =>
        closure(up, strict = true).flatMap(above =>
          Either.cond(
            up.id != other.id && !above(other.id),
            (),
            s"a case of ${family.label} derives from ${other.label}"
          )
        )
      case Right(_) => Right(())
    })
  }

  // Every class of the family's file whose `extends` clause may name it: through its own name, an
  // alias leading to it or a renaming selector. One this reading cannot hold as a declaration —
  // an anonymous class, a given instance, an enum case, a class the index does not hold — refuses,
  // and so does a parent clause that may reach the family through a parameterised alias.
  private def children(family: Declared): Either[String, List[Declared]] =
    childCache.getOrElseUpdate(
      family.id,
      sources
        .get(family.owner.input)
        .toRight(s"sealed family ${family.label}: source not supplied")
        .flatMap { source =>
          val names = spellings(family.label)
          sequence(templates(source).flatMap { (tree, inits) =>
            if (inits.exists(init => names(lastName(init.tpe)))) List(child(tree, family))
            else inits.find(throughAlias(_, names)).map(init => Left(hidden(family, init))).toList
          })
        }
    )

  // `type Id[X] = X; case class C() extends Id[S]` extends `S`, though neither the clause's head nor
  // the alias's body names it: an argument does. A head that is one supplied class cannot be
  // dealiased; any other head — a parameterised alias, a renaming, a dependency's alias — may be.
  private def throughAlias(init: Init, names: Set[String]): Boolean =
    !classNames(lastName(init.tpe)) && words(init.tpe).exists(names)

  private def hidden(family: Declared, init: Init): String =
    s"sealed family ${family.label} may be extended through ${spell(init.tpe)}, " +
      "which this reading does not dealias"

  private def child(tree: Tree, family: Declared): Either[String, Declared] = {
    val found = tree match {
      case _: Defn.Class | _: Defn.Trait | _: Defn.Enum => types.find(_.tree eq tree).map(declared)
      case _: Defn.Object => resolver.modules.find(_.tree.exists(_ eq tree)).flatMap(moduleClass)
      case _              => None
    }
    found.toRight(
      s"sealed family ${family.label} has a subclass this reading does not enumerate: " +
        spell(tree).take(60)
    )
  }

  private def templates(source: Source): List[(Tree, List[Init])] =
    source.collect(Function.unlift((tree: Tree) => inits(tree).map(tree -> _)))

  // The names a subclass may extend a family through: its own, and every alias or renaming selector
  // of the sources that leads to one of them.
  private def spellings(name: String): Set[String] = {
    @tailrec def grow(found: Set[String]): Set[String] = {
      val more = found ++
        aliases.collect { case (alias, body) if body.exists(found) => alias } ++
        renames.collect { case (from, to) if found(from) => to }
      if (more == found) found else grow(more)
    }
    grow(Set(name))
  }

  private def parents(d: Declared): List[Either[String, Option[Declared]]] =
    parentCache.getOrElseUpdate(d.id, inits(d.tree).getOrElse(Nil).map(parent(_, d.owner)))

  private def inits(tree: Tree): Option[List[Init]] = tree match {
    case t: Defn.Class        => Some(t.templ.inits)
    case t: Defn.Trait        => Some(t.templ.inits)
    case t: Defn.Enum         => Some(t.templ.inits)
    case t: Defn.Object       => Some(t.templ.inits)
    case t: Defn.Given        => Some(t.templ.inits)
    case t: Term.NewAnonymous => Some(t.templ.inits)
    case c: Defn.EnumCase     => Some(c.inits)
    case _                    => None
  }

  // One parent clause, read where it is written: a supplied class, nothing for a builtin or a
  // foreign parent — one an import binds outside the sources, or a name no supplied declaration
  // carries — or the parent's spelling when it may be a supplied class this reading missed.
  private def parent(init: Init, scope: Frame): Either[String, Option[Declared]] = {
    val context = ResolutionContext(scope, resolver.typeParameters(scope), Set.empty)
    alternative(init.tpe, context) match {
      case Right(Alternative(found: Declared, _)) => Right(Some(found))
      case Right(_)                               => Right(None)
      case Left(_) if foreign(init.tpe, context)  => Right(None)
      case Left(_)                                => Left(spell(init.tpe))
    }
  }

  private def foreign(tpe: Type, context: ResolutionContext): Boolean = {
    val name = TypeApplications.name(head(tpe))
    resolver.declaration(head(tpe), name, context) match {
      case Found.Foreign(_, _) => true
      case Found.Absent        => !supplied(name.split('.').last)
      case _                   => false
    }
  }

  private def closure(start: Declared, strict: Boolean): Either[String, Set[String]] = {
    @tailrec def loop(pending: List[Declared], seen: Set[String]): Either[String, Set[String]] =
      pending match {
        case Nil          => Right(seen)
        case next :: rest =>
          val found = parents(next)
          found.collectFirst { case Left(name) if strict => name } match {
            case Some(name) => Left(s"ancestry of ${start.label} not resolved: $name")
            case None       =>
              val fresh = found.collect { case Right(Some(p)) if !seen(p.id) => p }.distinctBy(_.id)
              loop(rest ++ fresh, seen ++ fresh.map(_.id))
          }
      }
    loop(List(start), Set.empty)
  }

}

private[cardinality] object RuntimeClasses {

  /** A runtime class: a declaration of the sources, a builtin class, or a top type. */
  sealed trait Runtime {
    def label: String
  }

  final case class Declared(id: String, label: String)(val tree: Tree, val owner: Frame)
      extends Runtime

  final case class Builtin(label: String) extends Runtime

  final case class Top(kind: TopTypes.Kind) extends Runtime {
    def label: String = kind.label
  }

  /** The first refusal among checks, if any. */
  def all(checks: List[Either[String, Unit]]): Either[String, Unit] =
    checks.collectFirst { case Left(reason) => reason }.toLeft(())

  /** One alternative's class, and whether the alternative applies it to type arguments. */
  final case class Alternative(runtime: Runtime, applied: Boolean)

  /** The builtin names whose class this reading models: each final, or sealed in the standard
    * library with final cases.
    */
  def builtinNames: Set[String] = builtins.keySet

  private val builtins: Map[String, String] = Map(
    "String" -> "java.lang.String",
    "java.lang.String" -> "java.lang.String",
    "Predef.String" -> "java.lang.String",
    "Boolean" -> "java.lang.Boolean",
    "Byte" -> "java.lang.Byte",
    "Short" -> "java.lang.Short",
    "Char" -> "java.lang.Character",
    "Int" -> "java.lang.Integer",
    "Long" -> "java.lang.Long",
    "Float" -> "java.lang.Float",
    "Double" -> "java.lang.Double",
    "Unit" -> "scala.runtime.BoxedUnit",
    "Option" -> "scala.Option",
    "Either" -> "scala.util.Either",
    "util.Either" -> "scala.util.Either"
  )

  // Arities past 22 share one runtime class.
  private def tuple(arity: Int): String =
    if (arity <= 22) s"scala.Tuple$arity" else "scala.runtime.TupleXXL"

  @tailrec private def head(tpe: Type): Type = tpe match {
    case a: Type.Apply    => head(a.tpe)
    case a: Type.Annotate => head(a.tpe)
    case other            => other
  }

  private def lastName(tpe: Type): String =
    TypeApplications.name(head(tpe)).split('.').last

  private def words(tree: Tree): Set[String] =
    tree.syntax.split("[^\\p{L}\\p{N}_$]+").toSet

  private def spell(tree: Tree): String = tree.syntax.replaceAll("\\s+", " ")

  private def isClass(d: Declared): Boolean = !d.tree.is[Defn.Trait]

  // Neither deriving from the other, two classes share no instance when one is final (it has no
  // subclass) or both are classes (single inheritance: no class extends two of them).
  private def apartByHierarchy(a: Declared, b: Declared): Boolean =
    FinalImplementations.isFinal(a.tree) || FinalImplementations.isFinal(b.tree) ||
      (isClass(a) && isClass(b))

  private def kind(d: Declared): String =
    if (isClass(d)) "non-final class" else "open trait"

}
