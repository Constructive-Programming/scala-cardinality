package cardinality.analysis.resolution

import scala.annotation.tailrec
import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.model.ResolutionContext
import cardinality.types.{Kinds, Spelling, Traversals, TypeApplications}

import MethodAnalysis.{Frame, Resolved}

/** The supplied parents a template inherits from, each read with the type arguments its whole
  * `extends` chain gives it.
  *
  * Scala substitutes type arguments through every level of inheritance: in `trait B[X] extends
  * A[X]; trait E[Y] extends B[Option[Y]]`, `A`'s parameter is `Option[E.Y]`, not `B.X`. So a
  * parent's arguments are resolved under the environment already computed for the level that names
  * it, and that environment travels down to the next level. Within one receiver's linearization
  * each parent is read once — a diamond reaches the same declarations through either path — and a
  * parent reached twice with different arguments is refused rather than read under whichever path
  * came first (Scala gives a covariant one the intersection, which neither path describes). Two
  * receivers — an inner template and the one enclosing it — each have their own copy of a parent
  * both extend.
  */
private[cardinality] object InheritedScopes {

  /** One inherited level: the parent's own frame, the clause that named it, the frame that clause
    * sits in, the parent's type parameters as that clause instantiates them, and the root template
    * whose member the level's declarations are.
    */
  final case class Step(
      declared: Frame,
      parent: Init,
      from: Frame,
      env: Map[String, Resolved],
      root: Frame
  )

  /** The levels a walk reached from one root, nearest first. */
  def of(steps: List[Step], root: Frame): List[Step] = steps.filter(_.root.id == root.id)

  /** The frames of those levels. */
  def levels(steps: List[Step], root: Frame): List[Frame] = of(steps, root).map(_.declared)

  /** A template's supplied ancestors, nearest first, read from the template itself; or why the
    * first parent not read is not. `Resolver.ancestors` keeps them per target.
    */
  def ancestors(frame: Frame, resolver: Resolver): Either[String, List[Frame]] =
    Traversals
      .sequence(walk(List(frame -> resolver.typeParameters(frame)), resolver, frame))
      .map(_.map(_.declared))

  /** The step that reaches `level` from `root`, if `level` is one of its parents. */
  def at(steps: List[Step], root: Frame, level: Frame): Option[Step] =
    of(steps, root).find(_.declared.id == level.id)

  /** Every supplied parent of each root, breadth first, each root with the environment its own
    * clauses are read under. A parent outside the source set is skipped (nothing is guessed about
    * it); one the lookup cannot pin down is a diagnostic.
    */
  def walk(
      roots: List[(Frame, Map[String, Resolved])],
      resolver: Resolver,
      observer: Frame
  ): List[Either[String, Step]] = {
    val read = Levels(resolver, observer)
    roots.flatMap { (root, env) =>
      breadthFirst(
        root.parents.map(p => (root, p, env)),
        Map.empty[String, Map[String, Resolved]]
      ) {
        case ((from, parent, level), seen) =>
          read.level(from, parent, level, root) match {
            case Left(reason)      => (List(Left(reason)), Nil, seen)
            case Right(None)       => (Nil, Nil, seen)
            case Right(Some(step)) =>
              seen.get(step.declared.id) match {
                case None =>
                  (
                    List(Right(step)),
                    step.declared.parents.map(p => (step.declared, p, step.env)),
                    seen.updated(step.declared.id, step.env)
                  )
                case Some(first) if first == step.env => (Nil, Nil, seen)
                case Some(_)                          =>
                  (
                    List(Left(s"inherited parent instantiated twice: ${Spelling.of(parent.tpe)}")),
                    Nil,
                    seen
                  )
              }
          }
      }
    }
  }

  /** One step of a walk over a parent graph: a pending edge and the state so far give the step's
    * output, the edges it opens and the next state.
    */
  type Visit[E, S, O] = (E, S) => (List[O], List[E], S)

  /** A queue over a parent graph: nearer parents come first. */
  def breadthFirst[E, S, O](pending: List[E], state: S)(visit: Visit[E, S, O]): List[O] =
    queue(pending, state, Nil, visit)

  @tailrec
  private def queue[E, S, O](
      pending: List[E],
      state: S,
      out: List[O],
      visit: Visit[E, S, O]
  ): List[O] =
    pending match {
      case Nil          => out.reverse
      case edge :: rest =>
        val (found, next, after) = visit(edge, state)
        queue(rest ++ next, after, found.reverse ::: out, visit)
    }

  /** One walk's reading of a level: the resolver and the use site stay fixed across it. */
  private case class Levels(resolver: Resolver, observer: Frame) {

    def level(
        from: Frame,
        parent: Init,
        env: Map[String, Resolved],
        root: Frame
    ): Either[String, Option[Step]] =
      resolver.parent(parent, from).flatMap {
        case None                 => Right(None)
        case Some((entry, scope)) =>
          entry.self
            .toRight(s"inherited parent declaration not resolved: ${Spelling.of(parent.tpe)}")
            .map { declared =>
              val own = declared.typeParams.map(_.name.value).toSet
              Some(
                Step(
                  declared,
                  parent,
                  from,
                  resolver.typeParameters(declared) ++ scope.filter((name, _) => !own(name)) ++
                    arguments(declared, parent, from, env),
                  root
                )
              )
            }
      }

    // The parent's type arguments, resolved where the parent is named and under that level's own
    // environment: `extends Base[A]` reads an inherited `Base[A].seed` as this scope's A, not as a
    // second binder that happens to share the name.
    private def arguments(
        declared: Frame,
        parent: Init,
        from: Frame,
        env: Map[String, Resolved]
    ): Map[String, Resolved] =
      TypeApplications.arguments.getOption(parent.tpe) match {
        case None       => Map.empty
        case Some(args) =>
          declared.typeParams
            .zip(args)
            .map((parameter, arg) =>
              parameter.name.value -> resolver.operators.argument(
                Kinds.of(parameter),
                arg,
                ResolutionContext(from, env, Set.empty, Some(observer))
              )
            )
            .toMap
      }

  }

  /** The name an `extends` clause spells: qualification kept, a `_root_` anchor included, so an
    * external `Outer.Base` is not a same-spelled lexical `Base`. `Init.name` is anonymous for a
    * plain type parent, so the type is the source of the name.
    */
  def typeName(tpe: Type): String = tpe match {
    case Type.Name(name)                   => name
    case selected: Type.Select             => selected.syntax
    case Type.Apply.After_4_6_0(callee, _) => typeName(callee)
    case other                             => other.syntax
  }

}
