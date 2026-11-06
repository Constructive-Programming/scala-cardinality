package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.model.ContextualEvidence.{Lineage, Nominal}
import cardinality.analysis.model.Operator.Closure
import cardinality.analysis.model.ResolutionContext
import cardinality.analysis.{BuiltinTypes, MethodAnalysis, Resolver}
import cardinality.types.Traversals
import cardinality.types.Trees.hasOpaque

import MethodAnalysis.{Frame, TypeEntry}
import OverrideIdentity.{aliasFrame, depthLens, known, lexical, Bindings, Key, Side, Spelled}

/** A type's identity as the declarations its names resolve to, for the questions a structural shape
  * cannot answer: `final case class P1[A](a: A)` and `P2[A](a: A)` share a shape and are unrelated
  * types, and two classes spelled `P` in two objects are two types. Two readings share one
  * classification of a name (`Denotation`): the nominal identity string (`of`, `nominal`, ...) and
  * the key override identity compares (`Overrides`).
  */
private[cardinality] object TypeIdentities {

  /** What a resolved type name denotes as an identity. */
  private enum Denotation {

    /** A supplied class, trait, enum or other declaration: its own identity. */
    case Source(entry: TypeEntry)

    /** An opaque type: its own nominal identity, never its representation. */
    case Opaque(entry: TypeEntry)

    /** A transparent alias: its body, read where it is declared. */
    case Alias(entry: TypeEntry, declaration: Defn.Type)

    /** An abstract type member: not known. */
    case Abstract

    /** A name nothing supplied binds: the canonical builtin it spells. */
    case Builtin(canonical: String)

    /** A name an import binds to a path outside the sources. */
    case Foreign(path: String)
  }

  // A blocked name, or one nothing binds and no builtin spells, denotes nothing known.
  private def denotation(found: Found, name: String): Option[Denotation] = found match {
    case Found.Entry(entry) =>
      entry.tree match {
        case d: Defn.Type if d.mods.hasOpaque => Some(Denotation.Opaque(entry))
        case d: Defn.Type                     => Some(Denotation.Alias(entry, d))
        case _: Decl.Type                     => Some(Denotation.Abstract)
        case _                                => Some(Denotation.Source(entry))
      }
    case Found.Foreign(path, _) => Some(Denotation.Foreign(path))
    case Found.Absent           => BuiltinTypes.canonical(name).map(Denotation.Builtin(_))
    case _                      => None
  }

  // An alias body's scope: where it is declared, with `id` entered against a cycle.
  private def declarationScope(
      entry: TypeEntry,
      context: ResolutionContext,
      resolver: Resolver,
      id: String
  ): ResolutionContext =
    context.inScope(entry.owner, resolver.typeParameters(entry.owner)).enter(id)

  /** A canonical spelling of `tpe`: each name the declaration it resolves to, each binder its own
    * identity. None when a name is an abstract member, a parameterized alias or unresolved, or a
    * binder stands for an instantiation — an identity this reading does not know.
    */
  def of(written: WrittenType): Option[String] =
    Reader(written.resolver, closures = false).of(written.tpe, written.context)

  /** A parameter type's identity where an overriding member is matched against the one it may
    * override: as `of`, and also through a higher-kinded binder an `extends` clause binds to a
    * written constructor (`F` given `Either` is `Either`), with a by-name or repeated marker kept.
    */
  def parameter(written: WrittenType): Option[String] = {
    val WrittenType(tpe, context, resolver) = written
    val reader = Reader(resolver, closures = true)
    tpe match {
      case Type.ByName(inner)   => reader.of(inner, context).map(id => s"=> $id")
      case Type.Repeated(inner) => reader.of(inner, context).map(id => s"$id*")
      case _                    => reader.of(tpe, context)
    }
  }

  private case class Reader(resolver: Resolver, closures: Boolean) {

    def of(tpe: Type, context: ResolutionContext): Option[String] = tpe match {
      case n: Type.Name =>
        context.variables.get(n.value) match {
          case Some(Right(Shape.Atom(id)))                         => Some(s"binder $id")
          case Some(Right(constructor @ Shape.Constructor(token))) =>
            if (resolver.operators.isRigid(constructor)) Some(token) else closure(n, context)
          case Some(_) => None
          case None    => named(WrittenType(tpe, context, resolver), n.value)
        }
      case s: Type.Select     => named(WrittenType(tpe, context, resolver), s.syntax)
      case a: Type.Apply      => applied(a.tpe, a.argClause.values, context)
      case a: Type.ApplyInfix => applied(a.op, List(a.lhs, a.rhs), context)
      // Tuple and arrow syntax are `scala.TupleN` and `scala.FunctionN` applied: one spelling each.
      case Type.Tuple(args) => arguments(s"scala.Tuple${args.size}", args, context)
      case f: Type.Function =>
        val parameters = f.paramClause.values
        arguments(s"scala.Function${parameters.size}", parameters :+ f.res, context)
      case _ => None
    }

    // A constructor written where the binder was instantiated, read where it was written.
    private def closure(name: Type.Name, context: ResolutionContext): Option[String] =
      Option
        .when(closures)(resolver.operators.applicable(name, context).toOption)
        .flatten
        .collect { case Closure(written, scope) => (written, scope) }
        .flatMap((written, scope) => of(written, scope))

    private def applied(callee: Type, args: List[Type], context: ResolutionContext) =
      of(callee, context).flatMap(arguments(_, args, context))

    private def arguments(head: String, args: List[Type], context: ResolutionContext) =
      Traversals.defined(args.map(of(_, context))).map(_.mkString(s"$head[", ", ", "]"))

  }

  /** What implicit search can tell of a type beyond its shape (`ContextualEvidence`). */
  def nominal(written: WrittenType): Nominal =
    Nominal(of(written), finalClass(written), lineage(written))

  /** The declaration of a final concrete class a type applies, when it is one: no other type is a
    * subtype of it, so two distinct ones are unrelated whatever their arguments.
    */
  def finalClass(written: WrittenType): Option[String] =
    head(written).collect {
      case entry if entry.tree.is[Defn.Class] && FinalImplementations.isFinal(entry.tree) =>
        key(entry)
    }

  /** The class or trait a type applies, by frame, with its supplied base classes: a class type
    * conforms to another only through a base class, whatever the arguments.
    */
  def lineage(written: WrittenType): Option[Lineage] =
    head(written)
      .flatMap(classFrame)
      .map { frame =>
        val (bases, complete) = written.resolver.baseClasses(frame)
        Lineage(frame.id, bases, complete)
      }

  /** The frame of the class or trait a declaration is, if it is one. */
  def classFrame(entry: TypeEntry): Option[MethodAnalysis.Frame] =
    entry.self.filter(_ => entry.tree.is[Defn.Class] || entry.tree.is[Defn.Trait])

  // The source declaration a type's head names, where the head is a name or a path, not a binder.
  private def head(written: WrittenType): Option[TypeEntry] = {
    val WrittenType(tpe, context, resolver) = written
    val callee = tpe match {
      case a: Type.Apply => a.tpe
      case other         => other
    }
    callee match {
      case n: Type.Name if context.variables.contains(n.value) => None
      case _: (Type.Name | Type.Select)                        =>
        resolver.declaration(callee, callee.syntax, context).declared
      case _ => None
    }
  }

  private def named(written: WrittenType, name: String): Option[String] = {
    val WrittenType(_, context, resolver) = written
    denotation(resolver.declaration(written.tpe, name, context), name).flatMap {
      // A plain alias without parameters is the type it names, read where it is declared.
      case Denotation.Alias(entry, d) =>
        val k = key(entry)
        Option
          .when(d.tparamClause.values.isEmpty && !context.visiting(k))(
            declarationScope(entry, context, resolver, k)
          )
          .flatMap(scope => of(WrittenType(d.body, scope, resolver)))
      case Denotation.Source(entry) => Some(key(entry))
      case Denotation.Opaque(entry) => Some(key(entry))
      // One key per declaration: `Either` and `scala.util.Either` are the model's one entry.
      case Denotation.Builtin(canonical)               => Some(canonical)
      case Denotation.Abstract | Denotation.Foreign(_) => None
    }
  }

  /** A declaration's identity: its declaring scope and its name. */
  def key(entry: TypeEntry): String = s"${entry.owner.id}#${entry.name}"

  /** The nominal identity of a type as written in a member signature: what override identity
    * compares. A source name is keyed by the declaration the scope-correct lookup binds it to
    * (imports included), a name outside the sources by the path an import binds it to, a builtin by
    * its canonical declaration; transparent aliases expand, opaque ones and anything else this
    * keying cannot pin stay unknown (`None`). A key resting on a name outside the sources or on an
    * opaque type is *tentative*: its equalities are invisible here, so a differing key proves
    * nothing.
    */
  final class Overrides(resolver: Resolver) {
    private given Dialect = dialects.Scala3

    def key(tpe: Type, side: Side): Key = tpe match {
      case n: Type.Name  => simple(n, Nil, side)
      case t: Type.Apply =>
        t.tpe match {
          case n: Type.Name => simple(n, t.argClause.values, side)
          case s: Type.Select if !termQualifier(s, side.context.frame) =>
            named(s, t.argClause.values, side)
          case _ => application(key(t.tpe, side), t.argClause.values, side)
        }
      // Tuple and arrow syntax spell `scala.TupleN` and `scala.FunctionN` themselves: one key per
      // declaration, whichever way it is written (scalac accepts an override that respells them).
      case Type.Tuple(args) => application(known(s"builtin:scala.Tuple${args.size}"), args, side)
      case f: Type.Function =>
        val parameters = f.paramClause.values
        application(known(s"builtin:scala.Function${parameters.size}"), parameters :+ f.res, side)
      case other => decoratedKey(other, side)
    }

    /** The keys of several types, as one key: unknown when any is, tentative when any is. */
    def combine(keys: List[Key]): Key =
      Traversals
        .defined(keys)
        .map(all =>
          Spelled(
            all.map(k => s"${k.spelling.length}:${k.spelling}").mkString("[", ",", "]"),
            all.exists(_.tentative)
          )
        )

    // A name the template inherits is a member this keying does not follow: its identity is left
    // open rather than read through a same-spelled outer declaration.
    private def simple(name: Type.Name, args: List[Type], side: Side): Key =
      memberKey(name, args, side).getOrElse {
        if (resolver.inheritsName(name, name.value, side.context)) None
        else named(name, args, side)
      }

    // A type member keys as its path's: an alias as its body where it is declared, an abstract member
    // as its atom (members M2), however it is spelled (`Member`, `this.Member`, `p.Member`). Method
    // binders shadow members, so they are read first.
    private def memberKey(tpe: Type, args: List[Type], side: Side): Option[Key] =
      Option
        .unless(methodBinder(tpe, side.bindings))(
          resolver.unappliedMember(tpe, args, side.context.referencing(tpe))
        )
        .flatten
        .map(_.toOption.flatMap(_.fold(aliasKey(_, _, side.bindings), known, (_, _) => None)))

    private def methodBinder(tpe: Type, bindings: Bindings): Boolean = tpe match {
      case n: Type.Name => bindings.locals.contains(n.value)
      case _            => false
    }

    private def aliasKey(body: Type, scope: ResolutionContext, bindings: Bindings): Key =
      if (bindings.expansionDepth >= resolver.limits.maxTypeDepth) None
      else
        key(
          body,
          Side(
            scope,
            depthLens.replace(bindings.expansionDepth + 1)(lexical(scope.frame, bindings.scoped))
          )
        )

    private def decoratedKey(tpe: Type, side: Side): Key =
      tpe match {
        case selected: Type.Select =>
          memberKey(selected, Nil, side).getOrElse {
            if (termQualifier(selected, side.context.frame)) None
            else named(selected, Nil, side)
          }
        case Type.ByName(value) =>
          key(value, side).map(k => k.copy(spelling = s"byname:${k.spelling}"))
        case Type.Repeated(value) =>
          key(value, side).map(k => k.copy(spelling = s"repeated:${k.spelling}"))
        case _ => None
      }

    // A binder of the method or of an enclosing scope first — unless a nearer declaration or import
    // may rebind the name, or a qualifier selects it — then the declaration the name means where it
    // is written.
    private def named(written: Type, args: List[Type], side: Side): Key = {
      val scopes = side.context.frame.chain
      val name = spelled(written)
      val entries = resolver.lookup(name, side.context.frame)
      val enclosing = scopes
        .find(scope => side.bindings.scoped.contains(scope.id -> name))
        .filterNot { binder =>
          val nearer = scopes.takeWhile(_.id != binder.id)
          entries.exists(entry => nearer.exists(_.id == entry.owner.id)) || imports(name, nearer)
        }
        .map(scope => side.bindings.scoped(scope.id -> name))
      val bound =
        if (written.is[Type.Select]) None else side.bindings.locals.get(name).orElse(enclosing)
      bound match {
        case Some(binding) => applied(binding, args, side)
        case None          => declared(written, args, side)
      }
    }

    private def spelled(written: Type): String = written match {
      case n: Type.Name => n.value
      case other        => other.syntax.stripPrefix("_root_.")
    }

    // The declaration lookup binds the name to at its reference, imports included (C9 (a)/(b)): a
    // supplied one by its identity (an alias expanded), one an import binds outside the sources by
    // that path, and a name nothing binds by the builtin it spells — unless a channel to unsupplied
    // declarations may rebind that name there.
    private def declared(written: Type, args: List[Type], side: Side): Key = {
      val name = spelled(written)
      val at = side.context.referencing(written)
      denotation(resolver.resolveName(name, at), name) match {
        case Some(Denotation.Source(entry))      => applied(known(sourceKey(entry)), args, side)
        case Some(Denotation.Alias(entry, body)) => alias(entry, body, args, side)
        case Some(Denotation.Opaque(entry))      =>
          applied(Some(Spelled(s"opaque:${sourceKey(entry)}", tentative = true)), args, side)
        case Some(Denotation.Foreign(path)) =>
          applied(Some(Spelled(s"foreign:$path", tentative = true)), args, side)
        case Some(Denotation.Builtin(canonical)) if resolver.rootShadow(name, at).isEmpty =>
          applied(known(s"builtin:$canonical"), args, side)
        case _ => None
      }
    }

    private def sourceKey(entry: TypeEntry): String =
      s"source:${entry.owner.id}:${entry.tree.pos.start}"

    private def applied(constructor: Key, args: List[Type], side: Side): Key =
      if (args.isEmpty) constructor else application(constructor, args, side)

    // A transparent alias is its body under its parameters' keys. An opaque one is its own nominal
    // declaration (C9 (d)), never its representation: `declared` keys it.
    private def alias(
        entry: TypeEntry,
        declaration: Defn.Type,
        args: List[Type],
        side: Side
    ): Key = {
      val Side(context, bindings) = side
      val parameters = declaration.tparamClause.values
      val id = s"override-alias:${entry.owner.id}:${declaration.pos.start}"
      if (
        parameters.size != args.size ||
        parameters.exists(p =>
          p.tparamClause.values.nonEmpty || p.mods.nonEmpty ||
            p.bounds.lo.nonEmpty || p.bounds.hi.nonEmpty ||
            p.bounds.context.nonEmpty || p.bounds.view.nonEmpty
        ) ||
        context.visiting(id) || bindings.expansionDepth >= resolver.limits.maxTypeDepth
      ) None
      else {
        val depth = bindings.expansionDepth + 1
        val arguments = args.map(key(_, Side(context, depthLens.replace(depth)(bindings))))
        val lexicalBindings = lexical(entry.owner, bindings.scoped)
        val replacements = parameters.map(_.name.value).zip(arguments).toMap
        val scoped = declarationScope(entry, context, resolver, id)
        key(
          declaration.body,
          Side(scoped, aliasFrame.replace((replacements, depth))(lexicalBindings))
        )
      }
    }

    private def termQualifier(tpe: Type.Select, frame: Frame): Boolean = {
      val root = tpe.qual.syntax.takeWhile(_ != '.')
      frame.chain.exists { scope =>
        scope.params.exists(_.name.value == root) || scope.stats.exists {
          case v: (Defn.Val | Decl.Val) =>
            v.pats.exists(_.collect { case Pat.Var(n) => n.value }.contains(root))
          case _ => false
        }
      }
    }

    private def application(constructor: Key, args: List[Type], side: Side): Key =
      for {
        name <- constructor
        arguments <- combine(args.map(key(_, side)))
      } yield Spelled(
        s"apply:${name.spelling}:${arguments.spelling}",
        name.tentative || arguments.tentative
      )

    private def imports(name: String, scopes: List[Frame]): Boolean =
      scopes.flatMap(_.stats).exists {
        case i: Import =>
          val tokens = i.syntax.split("[^\\p{L}\\p{N}_$*]+").toSet
          name.split('.').exists(tokens) || tokens("*") || tokens("_")
        case _: Export => true
        case _         => false
      }

  }

}
