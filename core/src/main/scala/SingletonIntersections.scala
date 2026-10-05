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
      context: ResolutionContext,
      resolver: Resolver
  ): Resolved = syntax match {
    case Syntax.Reference(ref)    => singleton(ref, context, resolver)
    case Syntax.Meet(left, right) => intersection(left, right, context, resolver)
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
      context: ResolutionContext,
      resolver: Resolver
  ): Resolved = {
    val name = ref.syntax.stripPrefix("_root_.")
    val key = s"singleton:${context.frame.id}:$name"
    if (context.visiting(key) || context.visiting.size >= resolver.limits.maxTypeDepth)
      Left(s"recursive or exhausted singleton resolution: $name")
    else stableReference(ref, context.enter(key), resolver)
  }

  private def stableReference(
      ref: Term.Ref,
      context: ResolutionContext,
      resolver: Resolver
  ): Resolved = ref match {
    case Term.Name(name) =>
      context.frame.chain.iterator
        .map(owner => local(name, owner, context, resolver))
        .collectFirst { case Some(result) => result }
        .getOrElse(module(name, context.frame, resolver))
    case _: Term.Select => module(ref.syntax.stripPrefix("_root_."), context.frame, resolver)
    case _ => Left(s"unsupported stable singleton path: ${ref.syntax.stripPrefix("_root_.")}")
  }

  private def local(
      name: String,
      owner: Frame,
      context: ResolutionContext,
      resolver: Resolver
  ): Option[Resolved] = {
    val binding = BindingScope(name, owner, context)
    owner.params
      .find(_.name.value == name)
      .map(binding.parameter(_, resolver))
      .orElse {
        owner.stats.iterator
          .filter(stat => bindingPatterns(stat).exists(namesBinding(_, name)))
          .nextOption()
          .map(binding.stat(_, resolver))
      }
  }

  private def bindingPatterns(stat: Stat): List[Pat] = stat match {
    case v: Decl.Val => v.pats
    case v: Defn.Val => v.pats
    case v: Defn.Var => v.pats
    case _           => Nil
  }

  private def namesBinding(pattern: Pat, name: String): Boolean = pattern match {
    case Pat.Var(n) => n.value == name
    case _          => false
  }

  /** Lookup checks use the caller; binding types and initializers use the declaration scope. */
  private case class BindingScope(name: String, owner: Frame, caller: ResolutionContext) {

    private def declaration(resolver: Resolver): ResolutionContext =
      caller.copy(frame = owner, variables = resolver.typeParameters(owner))

    private def value(tpe: Type, resolver: Resolver): Resolved =
      declaration(resolver)
        .read(tpe, resolver)
        .map(shape => Shape.Singleton(s"${owner.id}:$name", Some(shape)))

    def parameter(p: Term.Param, resolver: Resolver): Resolved =
      if (p.mods.exists(_.is[Mod.VarParam])) Left(s"mutable singleton path: $name")
      else p.decltpe.toRight(s"missing singleton path type: $name").flatMap(value(_, resolver))

    def stat(stat: Stat, resolver: Resolver): Resolved = stat match {
      case v: Decl.Val => accessible(v, resolver).flatMap(_ => value(v.decltpe, resolver))
      case v: Defn.Val => accessible(v, resolver).flatMap(_ => capturedValue(v, resolver))
      case _           => Left(s"mutable singleton path: $name")
    }

    private def accessible(stat: Stat, resolver: Resolver): Either[String, Unit] =
      Either.cond(
        resolver.accessible(stat, owner, caller.frame),
        (),
        s"inaccessible singleton path: $name"
      )

    private def capturedValue(v: Defn.Val, resolver: Resolver): Resolved =
      if (!enclosingCapture(v, owner, caller.frame))
        Left(s"singleton path is not an enclosing capture: $name")
      else initializer(v.rhs, resolver)

    private def initializer(rhs: Term, resolver: Resolver): Resolved = rhs match {
      case ref: Term.Ref => singleton(ref, declaration(resolver), resolver)
      case _             => Left(s"stable singleton binding identity not resolved: $name")
    }

  }

  private def enclosingCapture(node: Tree, owner: Frame, from: Frame): Boolean =
    !owner.local || node.pos.start < from.tree.fold(Int.MaxValue)(_.pos.start)

  private def enclosingLocalScope(owner: Frame, from: Frame): Boolean =
    !owner.chain.exists(_.local) || from.chain.exists(_.id == owner.id)

  private def accessibleModuleScope(scope: Frame, from: Frame, resolver: Resolver): Boolean =
    scope.parent.forall(parent =>
      scope.tree.forall(resolver.accessible(_, parent, from)) &&
        enclosingLocalScope(parent, from) &&
        scope.tree.forall(enclosingCapture(_, parent, from))
    )

  private def eligibleModule(owner: Frame, from: Frame, resolver: Resolver): Boolean =
    owner.chain.forall(accessibleModuleScope(_, from, resolver))

  private def moduleEnvironment(owner: Frame, name: String): Resolved =
    if (owner.parents.nonEmpty || owner.stats.exists(stat => !stat.is[Defn.Type]))
      Left(s"singleton module member environment not resolved: $name")
    else Right(Shape.Singleton(s"module:${owner.id}", None))

  private def module(name: String, from: Frame, resolver: Resolver): Resolved = {
    val paths = from.chain.map(f => (f.path :+ name).mkString(".")) :+ name
    val found = paths.iterator
      .map(path => resolver.modules.filter(_.path.mkString(".") == path))
      .find(_.nonEmpty)
      .getOrElse(Nil)
    found match {
      case List(owner) if eligibleModule(owner, from, resolver) =>
        moduleEnvironment(owner, name)
      case Nil => Left(s"unresolved singleton path: $name")
      case _   => Left(s"inaccessible or ambiguous singleton path: $name")
    }
  }

  def intersection(
      left: Type,
      right: Type,
      context: ResolutionContext,
      resolver: Resolver
  ): Resolved = {
    if (left.structure == right.structure) context.read(left, resolver)
    else if (marker(left, context, resolver)) context.read(right, resolver).flatMap(singletonOnly)
    else if (marker(right, context, resolver)) context.read(left, resolver).flatMap(singletonOnly)
    else
      for {
        a <- context.read(left, resolver)
        b <- context.read(right, resolver)
        result <- provedIntersection(a, b)
          .toRight(s"unsupported intersection relationship: ${left.syntax} & ${right.syntax}")
      } yield result
  }

  private def marker(tpe: Type, context: ResolutionContext, resolver: Resolver): Boolean =
    tpe.syntax match {
      case "scala.Singleton" => resolver.lookup("scala.Singleton", context.frame).isEmpty
      case "Singleton"       =>
        resolver.lookup("Singleton", context.frame).isEmpty && unshadowedMarker(context)
      case _ => false
    }

  private def unshadowedMarker(context: ResolutionContext): Boolean =
    !context.variables.contains("Singleton") &&
      !context.frame.chain.exists(_.stats.exists(markerImportOrExport))

  private def markerImportOrExport(stat: Stat): Boolean = stat match {
    case i: Import => i.syntax.split("[^\\p{L}\\p{N}_$]+").contains("Singleton")
    case _: Export => true
    case _         => false
  }

  private def provedIntersection(left: Shape, right: Shape): Option[Shape] =
    (left, right) match {
      case (Shape.Sum(Nil), _) | (_, Shape.Sum(Nil)) => Some(Shape.Sum(Nil))
      case (a: Shape.Singleton, b: Shape.Singleton)  => singletonRelationship(a, b)
      case _                                         => binderRelationship(left, right)
    }

  private def singletonRelationship(a: Shape.Singleton, b: Shape.Singleton): Option[Shape] =
    if (a.id == b.id) Some(a)
    // Distinct module objects cannot be the same value. Ordinary stable values can alias
    // at runtime; different provenance alone does not prove their intersection empty.
    else if (a.underlying.isEmpty && b.underlying.isEmpty) Some(Shape.Sum(Nil))
    else None

  private def binderRelationship(left: Shape, right: Shape): Option[Shape] =
    (left, right) match {
      case (s: Shape.Singleton, a: Shape.Atom) => declaredBinder(s, a)
      case (a: Shape.Atom, s: Shape.Singleton) => declaredBinder(s, a)
      case _                                   => None
    }

  // A free atom has genuine binder identity, unlike two nominal types that happen to
  // share the same product representation. A path is a subtype of its declared binder.
  private def declaredBinder(singleton: Shape.Singleton, atom: Shape.Atom): Option[Shape] =
    singleton.underlying.collect { case declared: Shape.Atom if declared == atom => singleton }

  private def singletonOnly(shape: Shape): Resolved = shape match {
    case _: Shape.Singleton => Right(shape)
    case Shape.Sum(Nil)     => Right(shape)
    case _ => Left("intersection with Singleton requires a proven singleton identity")
  }

}
