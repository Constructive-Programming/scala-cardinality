package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.model.*
import cardinality.analysis.{MethodAnalysis, Resolver}
import cardinality.types.TypeSubstitution.{Meet, Union}
import cardinality.types.{NamedTuples, TypeApplications}

import MethodAnalysis.Target

/** What the call site of a `transparent inline` method observes.
  *
  * The declared result is only an upper bound: the call site types the expansion, so it may see a
  * subclass of a class the result names, with members of its own. Scala 3.9.0 compiles
  * `transparent inline def mk(b: Boolean): Foo = new Sub(b)` beside `class Sub(val n: Boolean)
  * extends Foo()` and reads `mk(true).n`, so `new Sub(true)` and `new Sub(false)` are two
  * implementations the declared `Foo` cannot tell apart. The declared type counts what callers
  * observe only when no class it names, its type arguments included, admits such a subclass: a
  * final class, a module, an enum, a sealed family of those, or a modelled builtin that is one. A
  * top type, a function type and an open class may all be narrowed. An anonymous class adds nothing
  * the call site sees: Scala 3 types `new T { val n = 1 }` as `T`.
  */
final private[cardinality] class TransparentResults(resolver: Resolver, classes: RuntimeClasses) {
  import TransparentResults.closedBuiltins

  /** Why the call site may observe more than the declared result `tpe`, if it may. */
  def narrowed(tpe: Type, context: ResolutionContext): Option[String] =
    open(tpe, context).map(name =>
      "transparent inline result: the call site observes the expansion's type, " +
        s"not the declared $name"
    )

  // The first class `tpe` names that a subclass may extend, as spelled.
  private def open(tpe: Type, context: ResolutionContext): Option[String] = tpe match {
    case named @ (_: Type.Name | _: Type.Select) => openClass(named, context)
    case a: Type.Apply                           => first(a.tpe :: a.argClause.values, context)
    case Union(left, right)                      => first(List(left, right), context)
    case Meet(left, right)                       => first(List(left, right), context)
    case i: Type.ApplyInfix                      => first(List(i.op, i.lhs, i.rhs), context)
    case NamedTuples(fields)                     => first(fields.map(_._2), context)
    case Type.Tuple(args)                        => first(args, context)
    case a: Type.Annotate                        => open(a.tpe, context)
    case _: Type.Singleton | _: Lit              => None
    case other                                   => Some(other.syntax)
  }

  private def first(types: List[Type], context: ResolutionContext): Option[String] =
    types.iterator.flatMap(open(_, context)).nextOption()

  // A binder is the caller's own type; an alias is its body; a declared class must be closed.
  private def openClass(named: Type, context: ResolutionContext): Option[String] = {
    val name = TypeApplications.name(named)
    if (context.variables.contains(name)) None
    else
      resolver.declaration(named, name, context) match {
        case Found.Entry(entry) =>
          entry.tree match {
            case alias: Defn.Type =>
              context
                .enterAlias(entry, resolver.typeParameters(entry.owner), AliasReading.Transparent)
                .flatMap(open(alias.body, _))
            case _: Defn.Class | _: Defn.Trait | _: Defn.Enum =>
              classes.closed(entry).left.toOption.map(_ => name)
            case _ => Some(name)
          }
        case Found.Absent => Option.unless(closedBuiltins(name.stripPrefix("scala.")))(name)
        case _            => Some(name)
      }
  }

}

private[cardinality] object TransparentResults {

  /** Whether the target is a `transparent inline` method, whose call site types the expansion. */
  def transparent(target: Target): Boolean = modifiers(target).exists(_.is[Mod.Transparent])

  val InlineReason: String =
    "inline method: each expansion may inspect its call site's argument types, constants and " +
      "givens (compile-time dispatch)"

  val InsideInline: String =
    "definition inside an inline method: it is expanded with the method at each call site, whose " +
      "argument types, constants and givens it may inspect"

  /** Why an `inline` target's implementations are not counted, or those of a definition inside an
    * inline method, which every expansion copies with it: an expansion is typed at its call site,
    * where `inline x match`, `erasedValue` and `summonFrom` read an argument's static type or
    * constant, a type argument or a given (fixtures il, review-A5 il/loc), so the parametric count
    * does not bound the implementations. A goal with one value keeps its one implementation: the
    * one-valued rule answers before this reason is read. A constructor's value space is the same in
    * every copy.
    */
  def inlineRefusal(target: Target): Option[String] =
    if (inlined(target.tree)) Some(InlineReason)
    else
      Option.when(!target.constructor && target.frame.chain.drop(1).exists(_.tree.exists(inlined)))(
        InsideInline
      )

  private def inlined(tree: Tree): Boolean = modifiers(tree).exists(_.is[Mod.Inline])

  private def modifiers(target: Target): List[Mod] = modifiers(target.tree)

  // No frame is an `inline given`'s yet: lane B5 adds the rows of definitions inside one, and with
  // them the regression for its arm.
  private def modifiers(tree: Tree): List[Mod] = tree match {
    case d: Defn.Def        => d.mods
    case d: Decl.Def        => d.mods
    case g: Defn.GivenAlias => g.mods
    case _                  => Nil
  }

  // Builtins no class extends: the classes this reading models (final scalar and string classes,
  // `Unit`, and `Option` / `Either`, sealed in the standard library with final cases) and the
  // bottom types. The top types, `Function1` and the evidence classes are not listed.
  private val closedBuiltins: Set[String] = RuntimeClasses.builtinNames ++ Set("Nothing", "Null")

}
