package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Shape
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*

/** How the site observes a final class's constructor parameters (rule B, v1.md §2), for the
  * capability record `Shapes` reads: a parameter it observes as itself is a field of the record.
  */
final private[compiled] class Observers(
    of: Shapes.Reading,
    // The constructor parameters the site observes on a value it is given (`Factories`).
    read: (ClassDef, TermSymbol) => List[ValDef]
)(using Context) {
  import Shapes.Env

  /** Whether the site observes a final class's constructor parameter as itself (rule B: only the
    * fields the site observes count).
    */
  def exposed(template: ClassDef, parameter: Symbol, site: TermSymbol, env: Env): Boolean =
    new Exposure(template, parameter, site, env).observed(template, parameter)

  // What the site observes of the template's parameter `asked`, read in `env`.
  final private class Exposure(template: ClassDef, asked: Symbol, site: TermSymbol, env: Env) {
    private val c = template.symbol
    private lazy val shape = field(template, asked).flatMap(seen(_, c, env))

    // Whether the site observes the parameter of `level` as itself: a field of `level` it may
    // select (one of the template, or one of `shape` as `c` sees it), a member of `level` it may
    // call of `shape` whose body is the field (an alias) or applies it to the member's own
    // parameters (an η-alias), or the argument a parent's primary constructor takes verbatim and
    // observes in turn. `shape` is the template's parameter's, so a wider observer is none. An
    // observer a class between `c` and `level` declares again is overridden there.
    def observed(level: ClassDef, parameter: Symbol): Boolean =
      field(level, parameter).exists { field =>
        selected(level, field, parameter) || aliased(level, field) ||
        passed(level, parameter).exists((parent, p) => observed(parent, p))
      }

    private def unoverridden(level: ClassDef, member: TermSymbol) =
      c.linearization.takeWhile(_ != level.symbol).forall(member.overridingSymbol(_).isEmpty)

    private def typed(member: TermSymbol) = shape.exists(seen(member, c, env).contains)

    private def reached(level: ClassDef, member: TermSymbol) =
      World.callable(member, site) && unoverridden(level, member)

    private def selected(level: ClassDef, field: TermSymbol, parameter: Symbol) =
      unoverridden(level, field) && (level.symbol == c || typed(field)) &&
        (World.selectable(field, site) || readOnThis(level, field, parameter))

    // Of `c`'s own field, one selected on `this` alone (a plain parameter, `protected[this]`)
    // only where a member the site may select reads it, or in `c`'s own constructor, whose
    // position keeps the stored side's estimate (`Constructors`).
    private def readOnThis(level: ClassDef, field: TermSymbol, parameter: Symbol) =
      level.symbol == c && World.callable(field, site) &&
        (World.isConstructor(site) && site.owner == c ||
          read(level, site).exists(_.symbol == parameter))

    private def aliased(level: ClassDef, field: TermSymbol) = level.rhs.body.exists {
      case alias: ValOrDefDef if candidate(alias, field) =>
        aliases(alias, field) && reached(level, alias.symbol) &&
        World.selectable(alias.symbol, site) && typed(alias.symbol)
      case _ => false
    }

  }

  // A member other than the field that may alias it: written, and no field itself.
  private def candidate(alias: ValOrDefDef, field: TermSymbol): Boolean =
    alias.symbol != field && !alias.symbol.isParamAccessor && !alias.symbol.isSynthetic

  // Whether a member's body is the field, or applies it to the member's own parameters.
  private def aliases(alias: ValOrDefDef, field: TermSymbol): Boolean =
    World.rhs(alias).exists(body => Shapes.named(body).contains(field) || eta(alias, body, field))

  // The field of a constructor parameter of `level`.
  private def field(level: ClassDef, parameter: Symbol): Option[TermSymbol] =
    level.symbol.declarations.collectFirst {
      case field: TermSymbol if field.isParamAccessor && field.name == parameter.name => field
    }

  // A method of one parameter clause whose body applies the field to those parameters in order.
  private def eta(alias: ValOrDefDef, body: TermTree, field: TermSymbol): Boolean =
    (alias, body) match {
      case (method: DefDef, Apply(select @ Select(function, _), arguments)) =>
        method.paramLists match {
          case List(Left(parameters)) =>
            World.named(select).exists(_.name.toString == "apply") &&
            Shapes.named(function).contains(field) &&
            arguments.map(World.named) == parameters.map(p => Some(p.symbol))
          case _ => false
        }
      case _ => false
    }

  // A member's type as `c` sees it: a method of one plain clause is the function of it.
  private def seen(member: TermSymbol, c: ClassSymbol, env: Env): Option[Shape] =
    member.typeAsSeenFrom(c.thisType) match {
      case method: MethodType if !method.isImplicitOrContextual =>
        method.resultType match {
          case result: Type =>
            val inputs = method.paramTypes.flatMap(of(_, env).toOption)
            of(result, env).toOption
              .filter(_ => inputs.sizeIs == method.paramTypes.size)
              .map(Shape.Function(inputs, _))
          case _ => None
        }
      case tpe: Type => of(tpe, env).toOption
      case _         => None
    }

  // The parents of `level` whose primary constructor takes `parameter` verbatim, each with the
  // parameter that takes it: arguments in order only, as one call of its clauses.
  private def passed(level: ClassDef, parameter: Symbol): List[(ClassDef, Symbol)] =
    level.rhs.parents.flatMap {
      case call: Apply => taking(call, level, parameter)
      case _           => Nil
    }

  // The parameters of the parent constructor `call` runs that take `parameter` verbatim.
  private def taking(call: Apply, level: ClassDef, parameter: Symbol): List[(ClassDef, Symbol)] = {
    val taken = Bodies.passed(call)
    parentTemplate(call.fun).toList.flatMap { parent =>
      val parameters = Shapes.parameters(parent.rhs.constr)
      if (
        !constructor(call.fun).contains(parent.rhs.constr.symbol) || parameters.sizeIs != taken.size
      ) Nil
      else
        taken.zip(parameters).collect {
          case (argument, p) if named(argument, parameter, level) => parent -> p.symbol
        }
    }
  }

  // Whether an argument names `parameter`, or the field of `level` it is.
  private def named(argument: TermTree, parameter: Symbol, level: ClassDef): Boolean =
    argument match {
      case ident: Ident =>
        World.named(ident).exists { s =>
          s == parameter || s.isParamAccessor && s.owner == level.symbol && s.name == parameter.name
        }
      case _ => false
    }

  // The template of the class whose constructor a parent call runs.
  private def parentTemplate(fun: TermTree): Option[ClassDef] =
    constructor(fun).flatMap(_.owner match {
      case parent: ClassSymbol => parent.tree
      case _                   => None
    })

  private def constructor(fun: TermTree): Option[TermSymbol] = fun match {
    case Apply(inner, _)     => constructor(inner)
    case TypeApply(inner, _) => constructor(inner)
    case other               => World.named(other)
  }

}
