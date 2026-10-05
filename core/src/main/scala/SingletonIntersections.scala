package cardinality

import scala.meta.*

import Inhabitation.Shape
import MethodAnalysis.{Frame, Resolved}

/** Stable reference identity is term identity, never the spelling or cardinality of its type.
  * Deliberately does not erase a module's callable environment into a Unit-shaped value.
  */
private[cardinality] object SingletonIntersections {
  private given Dialect = dialects.Scala3

  enum Syntax {
    case Reference(ref: Term.Ref)
    case Meet(left: Type, right: Type)
  }

  /** Normalize all source spellings of this feature family before resolving their semantics. */
  def unapply(tpe: Type): Option[Syntax] = tpe match {
    case s: Type.Singleton                            => Some(Syntax.Reference(s.ref))
    case Type.ApplyInfix(left, Type.Name("&"), right) => Some(Syntax.Meet(left, right))
    case Type.And(left, right)                        => Some(Syntax.Meet(left, right))
    case Type.With(left, right)                       => Some(Syntax.Meet(left, right))
    case _                                            => None
  }

  def resolve(
      syntax: Syntax,
      frame: Frame,
      variables: Map[String, Resolved],
      resolver: Resolver,
      visiting: Set[String]
  ): Resolved = syntax match {
    case Syntax.Reference(ref)    => singleton(ref, frame, resolver, visiting)
    case Syntax.Meet(left, right) => intersection(left, right, frame, variables, resolver, visiting)
  }

  // A public alias is not permission to read its private stable reference. Source aliases
  // resolve in their declaration frame; recheck the resulting identities at each use site.
  def validate(shape: Shape, from: Frame, resolver: Resolver): Resolved = shape match {
    case s @ Shape.Singleton(id, None) =>
      resolver.modules
        .find(owner => s"module:${owner.id}" == id)
        .toRight("singleton module identity not found")
        .flatMap(owner => module(owner.path.mkString("."), from, resolver).map(_ => s))
    case Shape.Singleton(id, Some(underlying)) =>
      if (from.chain.exists(owner => id.startsWith(s"${owner.id}:")))
        validate(underlying, from, resolver).map(value => Shape.Singleton(id, Some(value)))
      else Left("singleton alias refers to an inaccessible stable binding")
    case Shape.Product(fields) =>
      MethodAnalysis.sequence(fields.map(validate(_, from, resolver))).map(Shape.Product(_))
    case Shape.Sum(cases) =>
      MethodAnalysis.sequence(cases.map(validate(_, from, resolver))).map(Shape.Sum(_))
    case Shape.Function(args, result) =>
      for {
        parameters <- MethodAnalysis.sequence(args.map(validate(_, from, resolver)))
        output <- validate(result, from, resolver)
      } yield Shape.Function(parameters, output)
    case Shape.Repeated(element) =>
      validate(element, from, resolver).map(Shape.Repeated(_))
    case Shape.Existential(witnesses, body) =>
      validate(body, from, resolver).map(Shape.Existential(witnesses, _))
    case Shape.Evidence(left, right, equality) =>
      for {
        source <- validate(left, from, resolver)
        target <- validate(right, from, resolver)
      } yield Shape.Evidence(source, target, equality)
    case _ => Right(shape)
  }

  def singleton(
      ref: Term.Ref,
      frame: Frame,
      resolver: Resolver,
      visiting: Set[String]
  ): Resolved = {
    val name = ref.syntax.stripPrefix("_root_.")
    val key = s"singleton:${frame.id}:$name"
    if (visiting(key) || visiting.size >= resolver.limits.maxTypeDepth)
      Left(s"recursive or exhausted singleton resolution: $name")
    else {
      val next = visiting + key
      ref match {
        case Term.Name(n) =>
          frame.chain.iterator
            .map(owner => local(n, owner, frame, resolver, next))
            .collectFirst { case Some(result) => result }
            .getOrElse(module(name, frame, resolver))
        case _: Term.Select => module(name, frame, resolver)
        case _              => Left(s"unsupported stable singleton path: $name")
      }
    }
  }

  private def local(
      name: String,
      owner: Frame,
      from: Frame,
      resolver: Resolver,
      visiting: Set[String]
  ): Option[Resolved] = {
    def value(tpe: Type): Resolved =
      resolver
        .resolve(tpe, owner, resolver.typeParameters(owner), visiting)
        .map(shape => Shape.Singleton(s"${owner.id}:$name", Some(shape)))

    owner.params
      .find(_.name.value == name)
      .map { p =>
        if (p.mods.exists(_.is[Mod.VarParam])) Left(s"mutable singleton path: $name")
        else p.decltpe.toRight(s"missing singleton path type: $name").flatMap(value)
      }
      .orElse {
        owner.stats.collectFirst {
          case v: Decl.Val if v.pats.exists {
                case Pat.Var(n) => n.value == name; case _ => false
              } =>
            if (resolver.accessible(v, owner, from)) value(v.decltpe)
            else Left(s"inaccessible singleton path: $name")
          case v: Defn.Val if v.pats.exists {
                case Pat.Var(n) => n.value == name; case _ => false
              } =>
            if (!resolver.accessible(v, owner, from))
              Left(s"inaccessible singleton path: $name")
            else if (owner.local && v.pos.start >= from.tree.fold(Int.MaxValue)(_.pos.start))
              Left(s"singleton path is not an enclosing capture: $name")
            else
              v.rhs match {
                case ref: Term.Ref => singleton(ref, owner, resolver, visiting)
                case _             => Left(s"stable singleton binding identity not resolved: $name")
              }
          case v: Defn.Var if v.pats.exists {
                case Pat.Var(n) => n.value == name; case _ => false
              } =>
            Left(s"mutable singleton path: $name")
        }
      }
  }

  private def module(name: String, from: Frame, resolver: Resolver): Resolved = {
    val paths = from.chain.map(f => (f.path :+ name).mkString(".")) :+ name
    val found = paths.iterator
      .map(path => resolver.modules.filter(_.path.mkString(".") == path))
      .find(_.nonEmpty)
      .getOrElse(Nil)
    found match {
      case List(owner)
          if owner.chain.forall(scope =>
            scope.parent.forall(parent =>
              scope.tree.forall(resolver.accessible(_, parent, from)) &&
                (!parent.chain.exists(_.local) || from.chain.exists(_.id == parent.id)) &&
                (!parent.local || scope.tree
                  .forall(node => node.pos.start < from.tree.fold(Int.MaxValue)(_.pos.start)))
            )
          ) =>
        if (
          owner.parents.nonEmpty || owner.stats.exists {
            case _: Defn.Type => false
            case _            => true
          }
        )
          Left(s"singleton module member environment not resolved: $name")
        else Right(Shape.Singleton(s"module:${owner.id}", None))
      case Nil => Left(s"unresolved singleton path: $name")
      case _   => Left(s"inaccessible or ambiguous singleton path: $name")
    }
  }

  def intersection(
      left: Type,
      right: Type,
      frame: Frame,
      variables: Map[String, Resolved],
      resolver: Resolver,
      visiting: Set[String]
  ): Resolved = {
    def read(tpe: Type): Resolved = resolver.resolve(tpe, frame, variables, visiting)
    def marker(tpe: Type): Boolean =
      (tpe.syntax == "scala.Singleton" && resolver.lookup("scala.Singleton", frame).isEmpty) ||
        (tpe.syntax == "Singleton" && resolver.lookup("Singleton", frame).isEmpty &&
          !variables.contains("Singleton") && !frame.chain.exists(_.stats.exists {
            case i: Import => i.syntax.split("[^\\p{L}\\p{N}_$]+").contains("Singleton")
            case _: Export => true
            case _         => false
          }))
    if (left.structure == right.structure) read(left)
    else if (marker(left)) read(right).flatMap(singletonOnly)
    else if (marker(right)) read(left).flatMap(singletonOnly)
    else
      for {
        a <- read(left)
        b <- read(right)
        result <- (a, b) match {
          case (Shape.Sum(Nil), _) | (_, Shape.Sum(Nil))                => Right(Shape.Sum(Nil))
          case (a: Shape.Singleton, b: Shape.Singleton) if a.id == b.id => Right(a)
          // A free atom has genuine binder identity, unlike two nominal types that happen to
          // share the same product representation. A path is a subtype of its declared binder.
          case (s @ Shape.Singleton(_, Some(a: Shape.Atom)), b: Shape.Atom) if a == b =>
            Right(s)
          case (a: Shape.Atom, s @ Shape.Singleton(_, Some(b: Shape.Atom))) if a == b =>
            Right(s)
          // Distinct module objects cannot be the same value. Ordinary stable values can alias
          // at runtime; different provenance alone does not prove their intersection empty.
          case (Shape.Singleton(a, None), Shape.Singleton(b, None)) if a != b =>
            Right(Shape.Sum(Nil))
          case _ => Left(s"unsupported intersection relationship: ${left.syntax} & ${right.syntax}")
        }
      } yield result
  }

  private def singletonOnly(shape: Shape): Resolved = shape match {
    case _: Shape.Singleton => Right(shape)
    case Shape.Sum(Nil)     => Right(shape)
    case _ => Left("intersection with Singleton requires a proven singleton identity")
  }

}
