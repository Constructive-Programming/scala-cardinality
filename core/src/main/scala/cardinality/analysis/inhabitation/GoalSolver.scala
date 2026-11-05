package cardinality.analysis.inhabitation

import scala.collection.mutable

import dev.constructive.eo.optics.Getter

import Inhabitation.{Binding, Count, Shape}
import Shape.*

/** The proof-counting search behind `Inhabitation.count`: goals are memoised environments and
  * targets, each with the rules that build it, counted over the rules a productive root reaches.
  */
final private[inhabitation] class GoalSolver(
    maxStates: Int,
    inspect: String => Unit,
    maxDepth: Int
) {
  import GoalSolver.*

  private val nodes = mutable.ArrayBuffer.empty[List[Rule]]
  private val memo = mutable.Map.empty[(List[Value], Shape), Int]
  private val absurd = mutable.Map.empty[Int, Int]
  private val witnesses = mutable.Map.empty[(List[String], Int), Atom]
  private val reservedAtoms = mutable.Set.empty[String]
  // A neutral's place in a path: its structure, numbered once per solver run.
  private val neutrals = mutable.Map.empty[NeutralSaturation.Neutral, Int]
  // The source name of each binding the measurement named, by id.
  private val labels = mutable.Map.empty[String, String]
  private var witnessNumber = 0
  private var work = 0
  private var depth = 0

  def run(bindings: List[Binding], result: Shape): Count = {
    reservedAtoms ++= bindings.flatMap(binding => ExistentialInputs.names(binding.shape))
    reservedAtoms ++= ExistentialInputs.names(result)
    labels ++= bindings.flatMap(binding => binding.label.map(binding.id -> _))
    val env = bindings.distinct.map(binding => Value(List("input", binding.id), binding.shape))
    val root = goal(env, result)
    val productive = leastProductive(allowUnresolved = true)
    // A root with no productive rule has no implementation at all, not an unknown one.
    if (!productive(root)) Count.Finite(0)
    else {
      val proved = leastProductive(allowUnresolved = false)
      val live = nodes.indices.map { id =>
        // An inhabited empty type proves the context impossible. All expressions under that
        // assumption are observationally equal (the unique map out of the empty type). In
        // particular, two different ways to call an A => Nothing are not two inhabitants.
        if (absurd.get(id).exists(proved)) List(Rule(Nil))
        else nodes(id).filter(_.children.forall(productive))
      }.toVector
      new Live(live, inspect, maxDepth).result(root)
    }
  }

  // The least set of goals that can be built from nothing: a rule counts once every child does.
  private def leastProductive(allowUnresolved: Boolean): mutable.Set[Int] = {
    val productive = mutable.Set.empty[Int]
    var changed = true
    while (changed) {
      changed = false
      nodes.indices.foreach { id =>
        inspect("solver productivity")
        val available = nodes(id).filter(r => allowUnresolved || r.reason.isEmpty)
        if (!productive(id) && available.exists(_.children.forall(productive))) {
          productive += id
          changed = true
        }
      }
    }
    productive
  }

  // A goal is the environment it can read plus the shape it must produce, memoised so a cycle
  // becomes a re-visited node rather than another pass.
  private def goal(raw: List[Value], target: Shape): Int = {
    inspect("solver goal")
    if (depth >= maxDepth) throw new Inhabitation.Limit("solver depth budget exhausted")
    depth += 1
    try readGoal(raw, target)
    finally depth -= 1
  }

  private def readGoal(raw: List[Value], target: Shape): Int = {
    val flat = flatten(raw)
    val evidence = EvidenceAnalysis
      .context(live(flat).map(_.shape))
      .fold(reason => throw new Inhabitation.Unsupported(reason), identity)
    def rewrite(shape: Shape): Shape =
      evidence(shape).fold(reason => throw new Inhabitation.Unsupported(reason), identity)
    // An available proof stays in the environment, so a value a later split or binder brings is
    // read under it too.
    val env = flat.map {
      case value @ (Value(_, _: Evidence, _) | Value(_, _, Reading.Spent)) => value
      case value => value.copy(shape = rewrite(value.shape))
    }.distinct
    val normalized = rewrite(target)
    memo.get((env, normalized)) match {
      case Some(id) => id
      case None     => record(Recorded(env, normalized, nodes.size))
    }
  }

  private def record(recorded: Recorded): Int = {
    inspect("solver state")
    tick()
    val id = recorded.id
    nodes += Nil
    memo((recorded.env, recorded.target)) = id
    val ordinary = rules(recorded)
    nodes(id) = ordinary ++ absurdRule(recorded.env, recorded.target, id)
    id
  }

  private def absurdRule(env: List[Value], shape: Shape, id: Int): List[Rule] =
    shape match {
      case Product(Nil) => Nil // Unit already has its unique construction.
      case Sum(Nil)     =>
        absurd(id) = id
        Nil
      case _ =>
        val impossible = goal(env, Sum(Nil))
        absurd(id) = impossible
        List(Rule(List(impossible)))
    }

  // Context sums split first; then the new neutrals of eliminable callables join the context to be
  // split in turn (capabilities C10). The forwarding certificate reads a context nothing was
  // eliminated in yet.
  private def rules(recorded: Recorded): List[Rule] =
    split(recorded.env, recorded.target).map(List(_)).getOrElse(saturated(recorded))

  private def saturated(recorded: Recorded): List[Rule] = {
    val plan = saturation(recorded)
    val spent = recorded.env.collect { case Value(path, _, Reading.Spent) => path }.toSet
    val fresh = plan.neutrals.filterNot((neutral, _) => spent(spentPath(neutral)))
    if (fresh.nonEmpty) {
      val added = fresh.flatMap { (neutral, output) =>
        tick()
        List(Value(neutralPath(neutral), output), Value(spentPath(neutral), output, Reading.Spent))
      }
      List(Rule(List(goal(recorded.env ++ added, recorded.target))))
    } else
      Option
        .when(spent.isEmpty)(OpaqueSumForwarding.count(data(recorded.env), recorded.target))
        .flatten
        .fold(ordinaryRules(recorded, plan.refused))(count => List.fill(count)(Rule(Nil)))
  }

  // The name a reason gives a callable: the source name of the binding it was read from — an
  // elimination's suffix kept (`xs#1`, an element of `xs`) — and the fields of it that lead to the
  // callable (`inner#0`: the first field of the record `inner`).
  private def name(path: List[String]): String = path match {
    case "input" :: id :: rest =>
      (source(id) :: rest.collect { case s"field:$index" => index }).mkString("#")
    case _            => "a function the goal binds"
  }

  private def source(id: String): String = labels.getOrElse(id, id)

  private def neutralPath(neutral: NeutralSaturation.Neutral): List[String] =
    List("neutral", neutrals.getOrElseUpdate(neutral, neutrals.size).toString)

  private def spentPath(neutral: NeutralSaturation.Neutral): List[String] =
    "spent" :: neutralPath(neutral).tail

  // A goal with one construction whatever the context holds needs no case analysis.
  private def saturation(recorded: Recorded): NeutralSaturation.Plan =
    recorded.target match {
      case Product(Nil) | _: Singleton | _: Existential => NeutralSaturation.Plan.empty
      case _                                            =>
        val env = live(recorded.env)
        NeutralSaturation.plan(
          env.collect {
            case Value(path, shape, _) if !higherOrderValue(shape) => path -> shape
          },
          env.flatMap { value =>
            callees(value).map { (callee, producer) =>
              NeutralSaturation.Path(callee, name(value.path), producer.parameters, producer.output)
            }
          }
        )
    }

  private def ordinaryRules(recorded: Recorded, refused: Refusals): List[Rule] = {
    import recorded.{env, id, target}
    target match {
      case _: (Existential | Singleton) =>
        throw new Inhabitation.Unsupported("an existential or singleton goal")
      case Product(fields) =>
        List(Rule(fields.map(goal(env, _))))
      case Function(parameters, result) =>
        List(Rule(List(goal(env ++ binders(parameters, id), result))))
      // A polymorphic goal is built for binders no one chooses: fresh rigid atoms (skolems).
      case Poly(bound, body) =>
        val skolems = bound.zipWithIndex.map { (binder, index) =>
          binder.id -> witnesses.getOrElseUpdate(
            (List("skolem", id.toString), index),
            freshWitness()
          )
        }.toMap
        List(
          Rule(List(goal(env, ExistentialInputs.mapAtoms(body)(a => skolems.getOrElse(a.id, a)))))
        )
      case _ =>
        constructors(env, target) ++ producers(recorded, refused)
    }
  }

  // An empty context alternative has a unique absurd eliminator, and the branch's own value
  // joins the environment.
  private def split(env: List[Value], target: Shape): Option[Rule] =
    env.zipWithIndex
      .collectFirst {
        case (Value(path, Sum(alternatives), reading), at) if reading != Reading.Spent =>
          Rule(alternatives.zipWithIndex.map {
            case (shape, branch) =>
              goal(env.patch(at, List(Value(path :+ s"case:$branch", shape)), 1), target)
          })
      }
      .orElse(closedSplit(env, target))

  // Closed values are split uniformly: every branch is the same goal without them, once per
  // combination of their values, so the one memoised branch is raised to their joint space. An
  // empty closed value (a function into Nothing from an inhabited domain) makes the context absurd.
  private def closedSplit(env: List[Value], target: Shape): Option[Rule] = {
    val (closed, open) =
      env.partition(value => value.reading != Reading.Spent && ClosedSpaces.closed(value.shape))
    Option.when(closed.nonEmpty) {
      val exponent =
        closed.map(value => ClosedSpaces.space(value.shape)).foldLeft(Card.one)(_ * _)
      if (exponent.isZero) Rule(Nil) else Rule(List(goal(open, target)), exponent = exponent)
    }
  }

  private def binders(parameters: List[Shape], id: Int): List[Value] =
    parameters.zipWithIndex.map {
      case (shape, index) =>
        Value(List("bound", id.toString, index.toString), shape)
    }

  private def constructors(env: List[Value], target: Shape): List[Rule] = target match {
    case Sum(alternatives) => alternatives.map(shape => Rule(List(goal(env, shape))))
    // Every literal, and nothing the environment adds: its transparent values were split away
    // and a producer of a ground result is eliminated (or carries a reason).
    case Ground(_, size) => List(Rule(Nil, weight = size))
    // Nil is a base case. Cons is productive only when an element can be constructed; the
    // ordinary productive-cycle proof then establishes infinitely many distinct lengths.
    case Repeated(element) =>
      List(Rule(Nil), Rule(List(goal(env, element), goal(env, target))))
    case _ => Nil
  }

  private def producers(recorded: Recorded, refused: Refusals): List[Rule] =
    live(recorded.env).flatMap { value =>
      value.shape match {
        case atom: Atom if atom == recorded.target          => List(Rule(Nil))
        case applied: Applied if applied == recorded.target => List(Rule(Nil))
        case _: Function                                    =>
          callees(value).flatMap { (callee, producer) =>
            application(recorded.env, recorded.target, producer, refused.get(callee))
          }
        case poly: Poly => polymorphic(recorded, poly)
        case _          => Nil
      }
    }

  // A polymorphic input's paths: each is instantiated at what the goal fixes (PB2), irrelevant here
  // (PB3) or refused (PB4).
  private def polymorphic(recorded: Recorded, poly: Poly): List[Rule] = {
    import recorded.{env, target}
    producerPaths.get(poly.body)(Nil).flatMap { producer =>
      PolymorphicMembers.offer(poly.binders, producer, target) match {
        case PolymorphicMembers.Offer.Irrelevant      => Nil
        case PolymorphicMembers.Offer.Refused(reason) => List(Rule(Nil, Some(reason)))
        case PolymorphicMembers.Offer.Instantiated(instance, quotient) =>
          application(env, target, instance, None).map(_.copy(quotient = Some(quotient)))
      }
    }
  }

  // Functional arguments are synthesized only when the complete environment proves that no
  // higher-order result can feed argument synthesis. Otherwise retain the diagnostic rule. An
  // eliminable result is a neutral the context splits (C10), unless `refusal` says why not.
  private def application(
      env: List[Value],
      target: Shape,
      value: Producer,
      refusal: Option[String]
  ): List[Rule] = {
    // Empty-result callables are negations, not opaque tagged choices. A proved call to one
    // is handled by absurd elimination above; only nonempty sum results need case analysis.
    val eliminable = NeutralSaturation.eliminable(value.output)
    val opaque = if (eliminable) refusal else None
    // A call with no reason is a rule only where it is the goal: an eliminable result is saturated
    // (every case of it is in the context already). Case analysis of a ground result has every
    // literal's branch in the same environment, and a sequence result may be empty, so neither
    // proves the context absurd: no rule toward the empty goal.
    val idle = opaque.isEmpty && (eliminable || value.output != target)
    if (idle) Nil
    else {
      val certified = TerminalHigherOrder.certified(data(env))
      val dependencies =
        value.parameters.filter(p => certified || !higherOrder(p)).map(goal(env, _))
      List(Rule(dependencies, opaque.orElse(unsupported(value, certified))))
    }
  }

  private def unsupported(value: Producer, certified: Boolean): Option[String] =
    Option.when(!certified && value.parameters.exists(higherOrder))(
      "Higher-order application is unsupported"
    )

  // The values the certificates read: a proof is no datum, only the unit it was discharged to.
  private def data(env: List[Value]): List[Shape] = live(env).map {
    case Value(_, _: Evidence, _) => Product(Nil)
    case value                    => value.shape
  }

  private def tick(): Unit = {
    work += 1
    if (work > maxStates)
      throw new Inhabitation.Limit(s"Inhabitation state budget exceeded ($maxStates)")
  }

  private def flatten(values: List[Value]): List[Value] =
    values.flatMap {
      case spent @ Value(_, _, Reading.Spent) => List(spent)
      case Value(_, _: (Existential | Singleton), _) =>
        throw new Inhabitation.Unsupported("an existential or singleton input")
      case Value(path, Product(fields), _) =>
        flatten(fields.zipWithIndex.map {
          case (shape, index) =>
            Value(path :+ s"field:$index", shape)
        })
      case value => List(value)
    }.distinct

  private def freshWitness(): Atom = {
    val (name, next) = Shape.freshName("existential:", witnessNumber + 1)(reservedAtoms)
    witnessNumber = next - 1
    reservedAtoms += name
    Atom(name)
  }

}

private[inhabitation] object GoalSolver {

  // `Spent` marks a neutral eliminated on this path (capabilities C10): no value, only the reason
  // the neutral is not cased on again. A mark of its own, not a step of the path: a path is
  // provenance, and its first steps are binding ids, which any spelling may start.
  enum Reading {
    case Unread, Spent
  }

  final case class Value(path: List[String], shape: Shape, reading: Reading = Reading.Unread)

  // A goal being recorded: what it reads, what it builds, and its node.
  final case class Recorded(env: List[Value], target: Shape, id: Int)

  final case class Producer(parameters: List[Shape], output: Shape)

  // Why each eliminable producer path a goal does not saturate keeps a reason.
  type Refusals = Map[NeutralSaturation.Callee, String]

  val producerPaths: Getter[Shape, List[Shape] => List[Producer]] =
    Shape.cata[List[Shape] => List[Producer]] { (node, kids) =>
      node match {
        case Function(parameters, _) => args => kids.last(args ++ parameters)
        case Product(_)              => args => kids.flatMap(_(args)).toList
        case output                  => args => List(Producer(args, output))
      }
    }

  // What every rule reads: a marker of an eliminated neutral is no value.
  def live(env: List[Value]): List[Value] = env.filterNot(_.reading == Reading.Spent)

  // A value's producer paths, each with who it is: the one key saturation and application share. A
  // polymorphic member's paths that mention none of its binders are ordinary producers (PB0).
  def callees(value: Value): List[(NeutralSaturation.Callee, Producer)] = value.shape match {
    case function: Function  => indexed(value.path, producerPaths.get(function)(Nil))
    case Poly(binders, body) =>
      indexed(value.path, producerPaths.get(body)(Nil)).filter((_, producer) =>
        PolymorphicMembers.monomorphic(binders, producer)
      )
    case _ => Nil
  }

  private def indexed(path: List[String], producers: List[Producer]) =
    producers.zipWithIndex.map((producer, index) =>
      NeutralSaturation.Callee(path, index) -> producer
    )

  // A value saturation enumerates as a variable: neither a callable nor a polymorphic member.
  private def higherOrderValue(shape: Shape): Boolean = shape match {
    case _: Function | _: Poly => true
    case _                     => false
  }

  def higherOrder(shape: Shape): Boolean =
    Shape.exists(shape) {
      case _: Function | _: Existential | _: Poly => true
      case _                                      => false
    }

}
