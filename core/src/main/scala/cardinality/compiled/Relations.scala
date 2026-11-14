package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.resolution.LiteralTypes
import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Types.*

/** How types and classes relate where a reading needs it (`Shapes`): a union read as a tagged sum,
  * an intersection as its narrowest part, and a capability record's member that may be the measured
  * definition itself.
  */
final private[compiled] class Relations(world: World, of: Relations.Read)(using Context) {
  import Relations.{arity, maximal, runtimeClass, top}
  import Shapes.Env
  import Unread.Refused

  // A union read as the tagged sum of its alternatives, where one exists, as the old front end
  // reads it (`UnionSums`). Type equalities first, by the compiler's subtyping: an alternative
  // below another is that one (one class spelled twice, `Null` below a reference type), and a
  // top-like one (`AnyRef`, `Matchable` are `Any`) takes them all. `Nothing` and `Null` are empty
  // (null-free model). What remains is a sum where its values are told apart: distinct constants,
  // or alternatives of runtime classes no instance has two of (the closed world,
  // `ClosedProgram.mixes`; a primitive is its box).
  def union(written: List[Type], env: Env): Either[Unread, Shape] = {
    val kept = maximal(written.map(t => if (top(t)) defn.AnyType else t))(
      _.isSubType(_)
    )
    Traversals.sequence(kept.map(of(_, env))).flatMap { shapes =>
      val (types, inhabited) = kept.zip(shapes).filter(_._2 != Shape.Sum(Nil)).unzip
      val classes = types.map(runtimeClass)
      val apart = inhabited.sizeIs < 2 || inhabited.forall(LiteralTypes.literal) ||
        classes.forall(_.nonEmpty) &&
        classes.flatten.combinations(2).forall(pair => !world.mixes(pair.head, pair.last))
      Either.cond(
        apart,
        inhabited match {
          case List(single) => single
          case several      => Shape.Sum(several)
        },
        Refused(
          "union alternatives require an overlap and discrimination proof: " +
            written.map(_.showBasic).mkString(" | ")
        )
      )
    }
  }

  // An intersection, as the old front end reads it (`SingletonIntersections.intersection`): the
  // narrowest parts are kept (`Relations.narrowest`); `Singleton` keeps only a singleton or the
  // empty type; distinct static objects or enum value cases share no value. Classes are met in the
  // closed world (`World.meet`): the sum of the classes whose instances are of every part, none
  // where two parts share no instance. Any other meet is refused. Each part kept is read first, so
  // that what a part needs is the row's reason, as there.
  def intersection(written: List[Type], env: Env): Either[Unread, Shape] = {
    val (marked, parts) = written.partition(World.classNamed(_).contains(defn.SingletonClass))
    val narrowest = Relations.narrowest(parts)
    val values = narrowest.flatMap(_.dealias match {
      case path: TermRef => path.optSymbol.filter(s => s.isModuleVal || s.isEnumCase)
      case _             => None
    })
    val apart = values.sizeIs == narrowest.size && values.distinct == values &&
      values.forall(World.inObjects)
    Relations
      .narrowed(narrowest)(of(_, env)) { several =>
        Traversals.sequence(several.map(of(_, env))).flatMap { _ =>
          if (apart) Right(Shape.Sum(Nil))
          else
            // An object is one value, as a family's object case is (`Sums`).
            Relations
              .met(world, several)(of(_, env), Shape.Product(Nil), Refused(_))
              .map {
                case List(single) => single
                case several      => Shape.Sum(several)
              }
        }
      }
      .filterOrElse(
        shape => marked.isEmpty || shape.isInstanceOf[Shape.Singleton] || shape == Shape.Sum(Nil),
        Refused("intersection with Singleton requires a proven singleton identity")
      )
  }

  // A member that varies across instances (abstract, or overridden by a class of the program),
  // own or inherited, of the measured definition's name and arity, where one instance may be of
  // the record's class and of the class the definition is a member of: on such a value it may
  // be the definition itself, whose calls re-enter it (the old front end's `selfView`,
  // capabilities C6), which the model does not split: refused, as there. A value built before
  // the receiver (`preceding`) is never it. A constructor row and a local definition measure no
  // member.
  def clash(c: ClassSymbol, env: Env): Boolean = (env.site, env.site.owner) match {
    case (site: TermSymbol, owner: ClassSymbol)
        if !World
          .isConstructor(site) && !env.preceding && (owner == c || world.mixes(owner, c)) =>
      (c :: World.ancestors(c)).flatMap(_.declarations).exists {
        case declared: TermSymbol =>
          // An inherited declaration as the record's class sees it: implemented there or not.
          val member = declared.overridingSymbol(c).getOrElse(declared)
          !member.isSynthetic && member.name == site.name &&
          arity(member) == arity(site) &&
          (member.isAbstractMember ||
            world.overriding(c, member) == ClosedProgram.Overriding.Supplied)
        case _ => false
      }
    case _ => false
  }

}

private[compiled] object Relations {

  /** How a part of a union or an intersection is read: `Shapes.of`. */
  type Read = (Type, Shapes.Env) => Either[Unread, Shape]

  /** How many value parameters a member takes, clause after clause; none for a value or a type. */
  def arity(member: Symbol): Int = {
    def taken(tpe: TypeOrMethodic): Int = tpe match {
      case method: MethodType => method.paramNames.size + taken(method.resultType)
      case poly: PolyType     => taken(poly.resultType)
      case _                  => 0
    }
    member match {
      case term: TermSymbol => taken(term.declaredType)
      case _                => 0
    }
  }

  /** The alternatives of a union, through the aliases they name. */
  def alternatives(tpe: Type)(using Context): List[Type] = tpe.dealias match {
    case or: OrType => alternatives(or.first) ++ alternatives(or.second)
    case other      => List(other)
  }

  /** The parts of an intersection, through the aliases they name. */
  def conjuncts(tpe: Type)(using Context): List[Type] = tpe.dealias match {
    case and: AndType => conjuncts(and.first) ++ conjuncts(and.second)
    case other        => List(other)
  }

  /** The types no other one lies above (`below(a, b)`: `a` lies below `b`), in order; of two equal
    * ones, the first. With `below` turned around, the narrowest.
    */
  def maximal(types: List[Type])(below: (Type, Type) => Boolean): List[Type] =
    types.foldLeft(List.empty[Type]) { (kept, t) =>
      if (kept.exists(below(t, _))) kept else kept.filterNot(below(_, t)) :+ t
    }

  /** The parts of an intersection that bound it, by the compiler's subtyping: no top (`T & Any` is
    * `T`), and none above another (`x.type & A` is `x.type` for an `x: A`).
    */
  def narrowest(parts: List[Type])(using Context): List[Type] =
    maximal(parts.filterNot(top))((a, b) => b.isSubType(a))

  /** An intersection of its narrowest parts: `Any` for none, the one alone, `several` otherwise. */
  def narrowed[E, A](parts: List[Type])(read: Type => Either[E, A])(
      several: List[Type] => Either[E, A]
  )(using Context): Either[E, A] = parts match {
    case Nil         => read(defn.AnyType)
    case List(alone) => read(alone)
    case more        => several(more)
  }

  /** The values of each class an intersection's parts meet in (`World.meet`), an object `one`. */
  def met[E, A](world: World, parts: List[Type])(
      read: Type => Either[E, A],
      one: A,
      refused: String => E
  ): Either[E, List[A]] =
    world.meet(parts).left.map(refused).flatMap { leaves =>
      Traversals.sequence(leaves.map {
        case _: TermRef => Right(one)
        case leaf       => read(leaf)
      })
    }

  /** Whether a type is `Any` or reads as it (`AnyRef`, which is `Object`, and `Matchable`;
    * AGENTS.md). A refinement of one (`AnyRef { def k: Int }`) is not.
    */
  def top(tpe: Type)(using Context): Boolean = !tpe.dealias.isInstanceOf[RefinedType] &&
    World.classNamed(tpe).exists(Set(defn.AnyClass, defn.ObjectClass, defn.MatchableClass))

  /** The class a value of a union's alternative carries at runtime: a class's (a primitive's box),
    * an object's, a literal's; none for any other type.
    */
  def runtimeClass(tpe: Type)(using Context): Option[ClassSymbol] = (tpe.dealias match {
    case constant: ConstantType => World.classNamed(constant.underlying)
    case other                  => World.instanceClass(other)
  }).map(c => Boxes.get(c.displayFullName).fold(c)(summon[Context].findStaticClass))

  private val Boxes = Map(
    "scala.Unit" -> "scala.runtime.BoxedUnit",
    "scala.Boolean" -> "java.lang.Boolean",
    "scala.Char" -> "java.lang.Character",
    "scala.Byte" -> "java.lang.Byte",
    "scala.Short" -> "java.lang.Short",
    "scala.Int" -> "java.lang.Integer",
    "scala.Long" -> "java.lang.Long",
    "scala.Float" -> "java.lang.Float",
    "scala.Double" -> "java.lang.Double"
  )

}
