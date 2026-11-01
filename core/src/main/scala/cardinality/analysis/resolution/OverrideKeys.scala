package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.model.{ResolutionContext, ResolutionOptics}
import cardinality.analysis.{BuiltinTypes, MethodAnalysis, Resolver}
import cardinality.types.Traversals

import MethodAnalysis.{Frame, TypeEntry}
import OverrideIdentity.{aliasFrame, depthLens, known, lexical, Bindings, Key, Side, Spelled}

/** The nominal identity of a type as written in a member signature: what override identity
  * compares. A source name is keyed by the declaration the scope-correct lookup binds it to
  * (imports included), a name outside the sources by the path an import binds it to, a builtin by
  * its canonical declaration; transparent aliases expand, opaque ones and anything else this keying
  * cannot pin stay unknown (`None`). A key resting on a name outside the sources or on an opaque
  * type is *tentative*: its equalities are invisible here, so a differing key proves nothing.
  */
final private[cardinality] class OverrideKeys(resolver: Resolver) {
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
      .map(_.toOption.flatMap(_.fold(aliasKey(_, _, side.bindings), known)))

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
    resolver.resolveName(name, at) match {
      case Found.Entry(entry) =>
        ResolutionOptics.entryTypeDefinition.getOption(entry) match {
          case Some(declaration)                => alias(entry, declaration, args, side)
          case None if entry.tree.is[Decl.Type] => None
          case None                             => applied(known(sourceKey(entry)), args, side)
        }
      case Found.Foreign(path, _) =>
        applied(Some(Spelled(s"foreign:$path", tentative = true)), args, side)
      case Found.Absent if resolver.rootShadow(name, at).isEmpty =>
        applied(BuiltinTypes.canonical(name).flatMap(b => known(s"builtin:$b")), args, side)
      case _ => None
    }
  }

  private def sourceKey(entry: TypeEntry): String =
    s"source:${entry.owner.id}:${entry.tree.pos.start}"

  private def applied(constructor: Key, args: List[Type], side: Side): Key =
    if (args.isEmpty) constructor else application(constructor, args, side)

  // A transparent alias is its body under its parameters' keys; an opaque one is its own nominal
  // declaration (C9 (d)), never its representation.
  private def alias(entry: TypeEntry, declaration: Defn.Type, args: List[Type], side: Side): Key = {
    val Side(context, bindings) = side
    val parameters = declaration.tparamClause.values
    val id = s"override-alias:${entry.owner.id}:${declaration.pos.start}"
    if (declaration.mods.exists(_.is[Mod.Opaque]))
      applied(Some(Spelled(s"opaque:${sourceKey(entry)}", tentative = true)), args, side)
    else if (
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
      val scoped = context.inScope(entry.owner, resolver.typeParameters(entry.owner)).enter(id)
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
