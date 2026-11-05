package cardinality.capacity

import scala.meta.*

import cardinality.analysis.StdlibModel
import cardinality.types.{Trees, TypeName}

/** What a reading met whose size is not whether a value exists (`ValueRows`): a function from an
  * empty type (the algebra's 0, though the absurd function is a value), an intersection or
  * refinement (an upper bound, maybe over an empty type: `Int & String`), or a value that is not
  * known to exist — a template whose construction is not read (`Builds`), a singleton type.
  */
private[cardinality] enum Overcount {
  case EmptyDomain
  case Narrowing
  case Unread(what: String)
}

// What a top-level value holds (docs/research/pure-code.md §2, register D30): one value when its
// declared type has one, none when that type is empty — `min(1, |T|)` read as inhabitation, not as
// the algebra's capacity, which counts a function from an empty domain 0 (it never runs,
// docs/type-arithmetic.md §3) though the absurd function is a value, an intersection or a
// refinement by an upper bound, and a class by what its constructor stores, whether or not an
// instance is ever built. A witness decides inhabitation without the rest of the type: a result
// type with a value, an empty parameter type, a standard-library type the library gives a value of
// whatever its arguments, an enum's parameterless case or a sealed family's module that is a member
// of every instantiation and is built. Anything else reads the algebra (`capacity`). The
// initializer is never read.
private[capacity] object ValueRows {

  // Inhabited (true) or empty (false); or the reasons neither is decided.
  private type Verdict = Either[List[Definition.Unbound], Boolean]

  /** What a value holds: 1, 0, or — with the reasons noted — at most 1. With no declared type, 1.
    */
  def value(scope: Scope)(declared: Option[Type]): Size =
    declared.fold(UnitSize) { tpe =>
      inhabited(scope, tpe) match {
        case Right(true)   => UnitSize
        case Right(false)  => NothingSize
        case Left(reasons) =>
          reasons.foreach(scope.note)
          UnitSize
      }
    }

  private def inhabited(scope: Scope, tpe: Type): Verdict = tpe match {
    case f: Type.FunctionType => function(scope, f.paramClause.values, f.res)
    case Type.ByName(inner)   => inhabited(scope, inner)
    case Type.Tuple(args)     => all(args.map(arg => inhabited(scope, Counter.tupleElem(arg))))
    case Type.ApplyInfix(l, Type.Name("*:"), r) =>
      all(List(inhabited(scope, l), inhabited(scope, r)))
    case Type.ApplyInfix(l, Type.Name("|"), r) =>
      any(List(inhabited(scope, l), inhabited(scope, r)))
    case Type.Singleton(ref) => singleton(scope, ref)
    case _ if standard(scope, Trees.callee(tpe)) || witnessed(scope, Trees.callee(tpe)) =>
      Right(true)
    case _ => capacity(scope, tpe)
  }

  // A function has a value when its result type has one, or when a parameter type is empty (the
  // absurd function); none when every parameter type has a value and the result type none.
  private def function(scope: Scope, params: List[Type], result: Type): Verdict = {
    val domain = params.map(p => inhabited(scope, Counter.tupleElem(p)))
    inhabited(scope, result) match {
      case Right(true)                                     => Right(true)
      case _ if domain.contains(Right(false))              => Right(true)
      case Right(false) if domain.forall(_ == Right(true)) => Right(false)
      case codomain                                        => Left(reasons(codomain :: domain))
    }
  }

  // A product has a value when every factor has one, and none when a factor has none.
  private def all(factors: List[Verdict]): Verdict =
    if (factors.contains(Right(false))) Right(false)
    else if (factors.forall(_ == Right(true))) Right(true)
    else Left(reasons(factors))

  // A union has a value when a branch has one, and none when every branch has none.
  private def any(branches: List[Verdict]): Verdict =
    if (branches.contains(Right(true))) Right(true)
    else if (branches.forall(_ == Right(false))) Right(false)
    else Left(reasons(branches))

  private def reasons(verdicts: List[Verdict]): List[Definition.Unbound] =
    verdicts.flatMap(_.left.toOption).flatten.distinct

  private def undecided(what: String): Verdict = {
    if (what.contains("evaluates a throw")) {
      val frames = new Exception("trace").getStackTrace.take(12).map(_.toString).mkString("\n  ")
      println(s"DBG undecided: $what\n  at:\n  $frames")
    }
    Left(List(Definition.Unbound.Refusal(s"value not decided: $what")))
  }

  // `p.type` holds the value `p` denotes, if it denotes one: an object that inherits nothing and
  // whose body evaluates no throw is built; a standard-library object is. A value's declared type
  // is not read here (it reads where the value is declared, which a path does not carry), so its
  // singleton is undecided — `val n: Nothing; val sn: n.type` holds no value, not the 1 a
  // singleton's capacity is (`ValueRowsSpec`'s `singletonValues`).
  private def singleton(scope: Scope, ref: Term.Ref): Verdict = {
    val written = s"${ref.syntax}.type"
    Namespaces.path(ref).map(Namespaces.target(scope, _)) match {
      case Some(Namespaces.Target.Obj(_, _, exposure)) if exposure.inherits =>
        undecided(s"$written, whose object's parents are not read")
      case Some(Namespaces.Target.Obj(_, _, exposure)) =>
        if (Builds.throwing(exposure.stats)) undecided(s"$written, whose object's body throws")
        else Right(true)
      case Some(_: Namespaces.Target.Stdlib) => Right(true)
      case Some(_: Namespaces.Target.Value)  => undecided(s"$written, a value's singleton type")
      case _                                 => undecided(s"$written, whose path is not read")
    }
  }

  // The algebra's reading in a record of its own: with no reason, its 0 is emptiness and any other
  // number inhabitation — unless it read a function from an empty domain (a 0 with a value), or an
  // intersection or refinement, or a value not known to exist (a number with maybe none).
  private def capacity(scope: Scope, tpe: Type): Verdict = {
    val probe = scope.measuredValue
    // A declared type whose construction provably builds nothing holds no value, whatever it
    // stores: the row reads 0, and a function from it the absurd value. Any other unread
    // construction step may hide a value built where this reading does not look, and stays with
    // the overcount reasons below.
    val built = Trees.callee(tpe) match {
      case name @ (Type.Name(_) | Type.Select(_, _)) =>
        Namespaces.lookup(probe, name)((in, key, _) => in.definition(key)).flatMap(_.built(probe))
      case _ => None
    }
    if (built.exists(Builds.buildsNothing)) Right(false)
    else {
      val size = Counter.typeIn(probe)(tpe)
      val read = probe.overcounts
      if (probe.unbound.nonEmpty) Left(probe.unbound)
      else if (size.isZero) {
        if (read.contains(Overcount.EmptyDomain))
          undecided(s"${tpe.syntax} holds a function from an empty type")
        else Right(false)
      } else
        read
          .collectFirst {
            case Overcount.Narrowing    => "holds an intersection or refinement"
            case Overcount.Unread(what) => s"holds $what"
          }
          .fold(Right(true): Verdict)(why => undecided(s"${tpe.syntax} $why"))
    }
  }

  // A source definition every instantiation of which has a value (`Named.witnessed`).
  private def witnessed(scope: Scope, callee: Type): Boolean =
    Namespaces
      .lookup(scope, callee)((in, key, _) => in.definition(key).map(_.witnessed(in)))
      .contains(true)

  // A standard-library type with a value the library gives for every argument, spelled where no
  // source binds the name: through the root imports, or in full where no source namespace has the
  // path. The registry is the stdlib model's (`StdlibModel.witnesses`), one table for both readers.
  private def standard(scope: Scope, callee: Type): Boolean = callee match {
    case Type.Name(name) => StdlibModel.witnessName(name) && !scope.binds(TypeName.of(name))
    case select @ Type.Select(q, Type.Name(name)) =>
      StdlibModel.witnessQualified(select.syntax.stripPrefix("_root_.")) &&
      Namespaces.locate(scope, q, TypeName.of(name)).isEmpty
    case _ => false
  }

}
