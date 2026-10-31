package cardinality.analysis.resolution

import scala.annotation.tailrec
import scala.meta.*

import cardinality.analysis.MethodAnalysis.{Frame, TypeEntry}
import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.model.ResolutionContext
import cardinality.analysis.{BuiltinTypes, Resolver, SourceIndex}

/** A builtin name (`Int`, `Boolean`, `Option`) a reference may bind through a channel simple-name
  * lookup does not walk.
  *
  * The builtins come from the root import `scala.*`, the weakest binding Scala has, so any
  * declaration a reference reaches by its simple name shadows them (or is an ambiguity Scala
  * rejects). Lookup reads lexical declarations, package members and import clauses. It does not
  * read what an enclosing template inherits from its parents or its self type, an enum's cases
  * inside its body, what a wildcard-imported object inherits or exports, a package object's
  * inherited members, or export clauses. With `trait Base { type Int = Boolean }`, `Int` is
  * `Boolean` in `trait Sub { self: Base => … }`, after `object O extends Base; import O.*`, in the
  * package of `package object p extends Base` and through `export O.*`: Scala 3.9.0 rejects `val w:
  * Int = 3` in each.
  *
  * This reading walks those channels from every scope around the reference and refuses the builtin
  * when one reaches a supplied declaration of the name or an export that binds it. A step it cannot
  * follow is refused too when it may lead into the supplied sources: a parent, self type or alias
  * whose name resolves to nothing it reads, or a wildcard prefix headed by a supplied term. A
  * foreign namespace is the stdlib and foreign-names cluster's question. The walk runs only for a
  * name some supplied type, enum case or export spells; resolving the reference through the member
  * it reaches is the environment lane's.
  */
final private[cardinality] class ShadowedBuiltins(
    resolver: Resolver,
    types: List[TypeEntry],
    sources: List[Source],
    wildcards: (Frame, Option[(String, Int)]) => List[List[String]]
) {
  import ShadowedBuiltins.*

  private val cases: Set[(String, String)] = types.flatMap { entry =>
    (entry.tree, entry.self) match {
      case (d: Defn.Enum, Some(frame)) => caseNames(d).map(frame.id -> _)
      case _                           => Nil
    }
  }.toSet

  private val declarations = types.map(entry => entry.owner.id -> entry.name).toSet ++ cases

  private val typeNames = types.map(_.name).toSet

  private val spelled = typeNames ++ cases.map(_._2) ++ sources.flatMap(exportedNames)

  private val terms = sources.flatMap(declaredTerms).toSet

  private val enums = cases.map(_._1)

  private lazy val modules = resolver.modules.groupBy(_.path)

  private lazy val classes =
    types.filter(_.self.nonEmpty).groupBy(entry => entry.owner.path :+ entry.name)

  /** The builtin a simple name lookup did not find denotes, unless a channel lookup does not walk
    * may bind it.
    */
  def resolve(name: String, args: List[Shape], context: ResolutionContext): Either[String, Shape] =
    BuiltinTypes.resolve(name, args).flatMap { builtin =>
      Option.when(spelled(name))(new Walk(name).from(context)).flatten.toLeft(builtin)
    }

  // One name's search: each frame whose members it may reach, once, with the step from the
  // reference that first reached it.
  final private class Walk(name: String) {

    def from(context: ResolutionContext): Option[String] =
      search(context.frame.chain.flatMap(entrances(_, context.at)), Set.empty, None)

    // A member found wins over a step that could not be followed; that one refuses only when the
    // walk finds nothing else.
    @tailrec private def search(
        pending: List[Step],
        seen: Set[String],
        doubt: Option[String]
    ): Option[String] =
      pending match {
        case Nil                            => doubt
        case Step.Found(reason) :: _        => Some(reason)
        case Step.Doubt(reason) :: rest     => search(rest, seen, doubt.orElse(Some(reason)))
        case Step.Reach(frame, via) :: rest =>
          if (seen(frame.id)) search(rest, seen, doubt)
          else if (declarations(frame.id -> name)) Some(found(s"${qualified(frame)}.$name", via))
          else search(rest ++ members(frame, Some(via)), seen + frame.id, doubt)
      }

    // What one scope around the reference opens: an enum's cases and an enclosing template's
    // members beyond its own declarations, a package's objects and exports, and wildcard imports.
    private def entrances(scope: Frame, at: Option[(String, Int)]): List[Step] = {
      val own =
        if (!template(scope)) Nil
        else if (cases(scope.id -> name))
          List(Step.Found(found(s"${qualified(scope)}.$name", s"case of ${label(scope)}")))
        else members(scope, None)
      val packaged =
        if (resolver.isPackage(scope)) resolver.peers(scope.path).flatMap(members(_, None))
        else Nil
      val imported = wildcards(scope, at).flatMap { prefix =>
        targets(prefix, scope, s"wildcard import of ${prefix.mkString(".")}")
      }
      own ++ packaged ++ imported ++ unbound(scope)
    }

    // A wildcard clause whose prefix lookup does not bind (`import Outer.this.o.*`) opens a
    // supplied value's members; a root-anchored one, a package's.
    private def unbound(scope: Frame): List[Step] =
      scope.stats
        .collect { case clause: Import => clause.importers }
        .flatten
        .filter(importer =>
          SourceIndex.importPath(importer.ref).isEmpty &&
            importer.importees.exists(_.is[Importee.Wildcard])
        )
        .flatMap { importer =>
          val ref = spell(importer.ref)
          val via = s"wildcard import of $ref"
          // A root-anchored prefix is a package path read from the root alone.
          if (ref.startsWith("_root_."))
            targets(ref.stripPrefix("_root_.").split('.').toList, scope.chain.last, via)
          else List(doubt(ref, via))
        }

    // The frames one frame's members come from besides its own declarations: its parents, its self
    // type (read only from inside its body: the constructor's own frame holds no statements), and
    // what its export clauses forward. With no step yet, the frame's own channel names the step.
    private def members(frame: Frame, via: Option[String]): List[Step] = {
      def step(channel: String) = via.getOrElse(s"$channel of ${label(frame)}")
      val parents = parentClauses(frame).flatMap(init =>
        named(init.tpe, frame.parent.getOrElse(frame), step("parents"), 0)
      )
      val selves =
        if (frame.stats.isEmpty) Nil
        else selfTypes(frame).flatMap(named(_, frame, step("self type"), 0))
      parents ++ selves ++ exports(frame, step("export"))
    }

    private def exports(frame: Frame, via: String): List[Step] =
      frame.stats
        .collect { case clause: Export => clause.importers }
        .flatten
        .flatMap { importer =>
          val prefix = SourceIndex.importPath(importer.ref)
          importer.importees.flatMap {
            case _: Importee.Wildcard =>
              prefix.fold(List(doubt(s"export ${spell(importer)}", via)))(targets(_, frame, via))
            case selector if bound(selector).contains(name) =>
              List(Step.Found(found(s"export ${spell(importer)}", via)))
            case _ => Nil
          }
        }

    // The frames a dotted prefix may denote, anchored at every package around `from`: objects,
    // packages, and the companions of enums (their cases) and classes (nothing this reads).
    private def targets(prefix: List[String], from: Frame, via: String): List[Step] = {
      val paths = from.chain.flatMap(_.path.inits).distinct.map(_ ++ prefix)
      val frames = paths.flatMap(path => modules.getOrElse(path, Nil) ++ resolver.peers(path))
      val companions = paths.flatMap(classes.getOrElse(_, Nil)).flatMap(_.self)
      if (frames.nonEmpty || companions.nonEmpty)
        (frames ++ companions.filter(frame => enums(frame.id))).map(Step.Reach(_, via))
      else if (prefix.headOption.exists(terms)) List(doubt(prefix.mkString("."), via))
      else Nil
    }

    // A parent, self type or alias body naming a supplied template reaches it; one that resolves to
    // an alias is followed; a name lookup refuses, or one a supplied type spells but lookup does not
    // find, may be that type.
    private def named(tpe: Type, from: Frame, via: String, depth: Int): List[Step] =
      parentName(tpe) match {
        case None                       => List(doubt(spell(tpe), via))
        case Some(_) if depth > MaxHops => List(doubt(spell(tpe), via))
        case Some(parent)               =>
          val at = Option.when(tpe.pos.start >= 0)(from.input -> tpe.pos.start)
          resolver.resolveName(
            parent,
            ResolutionContext(from, Map.empty, Set.empty, at = at)
          ) match {
            case Found.Entry(entry) =>
              (entry.self, entry.tree) match {
                case (Some(frame), _)         => List(Step.Reach(frame, via))
                case (None, alias: Defn.Type) => named(alias.body, entry.owner, via, depth + 1)
                case _                        => List(doubt(parent, via))
              }
            case Found.Blocked(_) => List(doubt(parent, via))
            case Found.Absent     =>
              if (typeNames(parent.split('.').last)) List(doubt(parent, via)) else Nil
          }
      }

    private def found(declaration: String, via: String): String =
      s"builtin name $name may be shadowed by $declaration ($via)"

    private def doubt(unread: String, via: String): Step =
      Step.Doubt(
        s"builtin name $name may be shadowed through $unread, which is not resolved ($via)"
      )

  }

}

private[cardinality] object ShadowedBuiltins {

  // Alias hops a parent name is followed through before the walk gives up on it.
  private val MaxHops = 16

  private enum Step {
    case Reach(frame: Frame, via: String)
    case Found(reason: String)
    case Doubt(reason: String)
  }

  /** Whether a resolution's reason is this refusal: a builtin reading that may stand for a
    * different declaration, so no other reading of the same name may be trusted in its place.
    */
  def shadows(reason: String): Boolean =
    reason.startsWith("builtin name ") && reason.contains(" may be shadowed ")

  private def template(frame: Frame): Boolean = frame.tree.exists {
    case _: Defn.Class | _: Defn.Trait | _: Defn.Object | _: Defn.Enum | _: Defn.Given |
        _: Pkg.Object =>
      true
    case _ => false
  }

  private def templateOf(frame: Frame): Option[Template] = frame.tree.collect {
    case d: Defn.Class  => d.templ
    case d: Defn.Trait  => d.templ
    case d: Defn.Object => d.templ
    case d: Defn.Enum   => d.templ
    case d: Defn.Given  => d.templ
    case d: Pkg.Object  => d.templ
  }

  // The constructor's own frame drops its parents: its parameters are read outside the template's
  // members (Scala 3.9.0 types `class Sub(x: Int) extends Base` with `scala.Int`).
  private def parentClauses(frame: Frame): List[Init] = frame.tree match {
    case Some(d: Pkg.Object) => d.templ.inits
    case _                   => frame.parents
  }

  private def selfTypes(frame: Frame): List[Type] =
    templateOf(frame).flatMap(_.body.selfOpt).flatMap(_.decltpe).toList.flatMap(components)

  private def components(tpe: Type): List[Type] =
    sides(tpe).fold(List(tpe))((left, right) => components(left) ++ components(right))

  // The two operands of an intersection, in either spelling.
  private def sides(tpe: Type): Option[(Type, Type)] = tpe match {
    case Type.And(left, right)                               => Some(left -> right)
    case Type.With(left, right)                              => Some(left -> right)
    case Type.ApplyInfix(left, op, right) if op.value == "&" => Some(left -> right)
    case _                                                   => None
  }

  private def parentName(tpe: Type): Option[String] = tpe match {
    case Type.Name(value)      => Some(value)
    case selected: Type.Select => Some(selected.syntax.stripPrefix("_root_."))
    case applied: Type.Apply   => parentName(applied.tpe)
    case _                     => None
  }

  private def caseNames(d: Defn.Enum): List[String] = d.templ.body.stats.flatMap {
    case c: Defn.EnumCase         => List(c.name.value)
    case r: Defn.RepeatedEnumCase => r.cases.map(_.value)
    case _                        => Nil
  }

  private def qualified(frame: Frame): String = frame.path.mkString(".")

  // A tree as one line of a reason.
  private def spell(tree: Tree): String = tree.syntax.replaceAll("\\s+", " ")

  private def label(frame: Frame): String = frame.tree match {
    case Some(_: Pkg.Object)        => s"package object ${qualified(frame)}"
    case None if frame.path.isEmpty => "the empty package"
    case None                       => s"package ${qualified(frame)}"
    case _                          => qualified(frame)
  }

  // The name an explicit selector binds.
  private def bound(selector: Importee): Option[String] = selector match {
    case Importee.Name(name)      => Some(name.value)
    case Importee.Rename(_, name) => Some(name.value)
    case _                        => None
  }

  // The names export selectors bind: what a forwarder may put behind a builtin's spelling.
  private def exportedNames(source: Source): List[String] =
    source
      .collect { case clause: Export => clause.importers.flatMap(_.importees) }
      .flatten
      .flatMap(bound)

  // The term names the sources declare (pattern variables of any kind included): a wildcard prefix
  // headed by one of them may open supplied members; any other head names a foreign namespace.
  private def declaredTerms(source: Source): List[String] = source.collect {
    case d: Defn.Object => d.name.value
    case d: Pkg.Object  => d.name.value
    case d: Defn.Def    => d.name.value
    case d: Decl.Def    => d.name.value
    case d: Defn.Given  => d.name.value
    case p: Term.Param  => p.name.value
    case Pat.Var(name)  => name.value
  }

}
