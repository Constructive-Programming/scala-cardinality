package cardinality.compiled

import scala.util.Try

import tastyquery.Constants.Constant
import tastyquery.Contexts.*
import tastyquery.Names.termName
import tastyquery.SourcePosition
import tastyquery.Symbols.{ClassSymbol, TypeSymbolWithBounds}
import tastyquery.Trees.*
import tastyquery.Types.*

/** The calls the compiler inserts where the tree does not hold them, written as calls, so that a
  * search over the module's trees (`Reentry`) follows them.
  *
  *   - A match, on its scrutinee (Scala 3's `PatternMatcher`): each extractor applied to what it is
  *     matched against (its using clause after), then, on the result, `_1`… where it is a `Product`
  *     of as many as the patterns, nothing more where it is a `Boolean`, else `isEmpty` and `get`
  *     and, for more than one pattern, `_1`… of `get`. A sequence extractor's result (or `get`) is
  *     read by `lengthCompare`, `drop`, `toSeq` and `apply`, or, where it is a `Product`, by `_1`…
  *     and the last of those so. A stable pattern runs `equals` (and an `==` overload the scrutinee
  *     fits) of its value, given the scrutinee. A nested pattern is matched against what its part
  *     yields.
  *   - `==` and `!=` of `Any` run the receiver's `equals`; `##`, its `hashCode`.
  *
  * Each tree's calls are built once: `Reentry` keys what a tree makes by its identity (`Fixed`), so
  * every search and visit must meet the same trees.
  */
final private[compiled] class Matches(using Context) {
  private val built = new java.util.IdentityHashMap[Tree, List[Tree]]
  private val bindings = new java.util.IdentityHashMap[Bind, List[TermTree]]

  /** What a match's variable is bound to: each value its pattern is matched against, once the
    * match's calls are built (`apply`).
    */
  def boundTo(variable: Bind): List[TermTree] =
    Option(bindings.get(variable)).getOrElse(Nil)

  def apply(tree: Tree): List[Tree] = tree match {
    case _: Match | Apply(Select(_, _), _) | Select(_, _) =>
      Option(built.get(tree)).getOrElse(build(tree))
    case _ => Nil
  }

  private def build(tree: Tree): List[Tree] = {
    val calls = Try(inserted(tree)(using tree.pos)).getOrElse(Nil)
    if (calls.nonEmpty) built.put(tree, calls): Unit
    calls
  }

  private def inserted(tree: Tree)(using SourcePosition): List[Tree] = tree match {
    case Match(scrutinee, cases) => cases.flatMap(c => matched(c.pattern, scrutinee))
    case Apply(function @ Select(receiver, _), arguments) if ofAny(function, "==", "!=") =>
      equality(receiver).map(Apply(_, arguments)(tree.pos))
    case hash @ Select(receiver, _) if ofAny(hash, "##") =>
      select(receiver, "hashCode").map(Apply(_, Nil)(tree.pos))
    case _ => Nil
  }

  // Whether a selection names one of these members of `Any` (the name read first: it is cheap).
  private def ofAny(selection: TermReferenceTree, names: String*): Boolean =
    names.contains(Reentry.written(selection)) &&
      World.named(selection).exists(_.owner.displayFullName == "scala.Any")

  private def matched(pattern: PatternTree, scrutinee: TermTree)(using
      at: SourcePosition
  ): List[Tree] = pattern match {
    case variable @ Bind(_, body, _) =>
      bindings.put(variable, scrutinee :: boundTo(variable)): Unit
      matched(body, scrutinee)
    case TypeTest(body, _)  => matched(body, scrutinee)
    case Alternative(trees) => trees.flatMap(matched(_, scrutinee))
    case ExprPattern(value) =>
      val typed = Try(scrutinee.tpe.widenTermRef).toOption.collect { case tpe: Type => tpe }
      (equality(value) ++ select(value, "==").filter(m =>
        !ofAny(m, "==") && typed.exists(takes(m, _))
      ))
        .map(Apply(_, List(scrutinee))(at))
    case Unapply(fun, implicits, patterns) => extracted(fun, implicits, patterns, scrutinee)
    case _                                 => Nil
  }

  private def extracted(
      fun: TermTree,
      implicits: List[TermTree],
      patterns: List[PatternTree],
      scrutinee: TermTree
  )(using at: SourcePosition): List[Tree] = {
    val applied = Apply(fun, List(scrutinee))(at)
    val result = if (implicits.isEmpty) applied else Apply(applied, implicits)(at)
    val sequence =
      World.named(Reentry.unapplied(fun)._1).exists(_.name.toString == "unapplySeq")
    val product = products(result)
    // What the match selects, and what each pattern is matched against.
    val (selected, values) =
      if (!sequence && tupled(product, patterns)) (product, product.map(List(_)))
      else optional(result, patterns.size, sequence)
    result :: selected ++ values.flatten ++
      patterns.lazyZip(values).flatMap((inner, value) => value.flatMap(matched(inner, _)))
  }

  // Whether an extractor's result is a `Product` of as many as the patterns.
  private def tupled(product: List[TermTree], patterns: List[PatternTree]): Boolean =
    product.sizeIs == patterns.size && patterns.nonEmpty

  // An extractor's result read through `isEmpty` and `get`, for `count` patterns: what is selected,
  // and what each pattern is matched against.
  private def optional(result: TermTree, count: Int, sequence: Boolean)(using
      SourcePosition
  ): (List[TermTree], List[List[TermTree]]) = {
    val got = select(result, "get", parameterless = true)
    val base = got.headOption.getOrElse(result)
    val (read, yielded) =
      if (sequence) elements(base, count)
      else if (got.isEmpty || count == 1) (Nil, List.fill(count)(got))
      else (selectors(base), selectors(base).map(List(_)))
    (
      Option
        .when(got.nonEmpty)(select(result, "isEmpty", parameterless = true))
        .toList
        .flatten
        ++ got ++ read,
      yielded
    )
  }

  // A sequence read for `count` patterns: what is selected, and what each pattern is matched against.
  private def elements(base: TermTree, count: Int)(using
      at: SourcePosition
  ): (List[TermTree], List[List[TermTree]]) = {
    // A `Product` is read as one only where it is no sequence itself.
    val fields = if (select(base, "lengthCompare").nonEmpty) Nil else products(base)
    val sequence = fields.lastOption.getOrElse(base)
    val element = select(sequence, "apply").map(Apply(_, List(Literal(Constant(0))(at)))(at))
    val read = List("lengthCompare", "drop", "toSeq").flatMap(select(sequence, _))
    val leading = fields.dropRight(1).map(List(_))
    (fields ++ read ++ element, leading ++ List.fill(count - leading.size)(element))
  }

  // `_1`, `_2`… of a term, as long as they are declared; `products`, only of a `Product`.
  private def selectors(term: TermTree)(using SourcePosition): List[TermTree] =
    Iterator
      .from(1)
      .map(i => select(term, s"_$i", parameterless = true))
      .takeWhile(_.nonEmpty)
      .flatten
      .toList

  private def products(term: TermTree)(using SourcePosition): List[TermTree] =
    if (classesOf(term).exists(_.isSubClass(defn.ProductClass))) selectors(term) else Nil

  // A selection of each member of that name each class of the term's type declares or inherits
  // (every overload of the nearest class declaring one; with `parameterless`, those without a value
  // clause). A private one is kept: a match written in its class selects it.
  private def select(qualifier: TermTree, name: String, parameterless: Boolean = false)(using
      at: SourcePosition
  ): List[Select] = classesOf(qualifier).flatMap { k =>
    k.linearization.iterator
      .map(_.getAllOverloadedDecls(termName(name)).filter { m =>
        !parameterless || m.paramSymss.forall(_.isRight)
      })
      .find(_.nonEmpty)
      .getOrElse(Nil)
      .map(m => Select(qualifier, m.signedName)(None)(at))
  }

  // The `equals` that `==` and a stable pattern run: the one taking any value, not an overload.
  private def equality(receiver: TermTree)(using SourcePosition): List[Select] =
    select(receiver, "equals").filter(takes(_, defn.AnyType))

  // Whether a selected method's one parameter takes a value of this type.
  private def takes(method: Select, argument: Type): Boolean = Try(World.named(method).exists {
    _.declaredType match {
      case tpe: MethodType => tpe.paramTypes.sizeIs == 1 && argument.isSubType(tpe.paramTypes.head)
      case _               => false
    }
  }).getOrElse(true)

  // The classes a term's type names: each side of an intersection or a union, an abstract type's
  // upper bound.
  private def classesOf(term: TermTree): List[ClassSymbol] = {
    def of(tpe: Type): List[ClassSymbol] = tpe.dealias match {
      case byName: ByNameType => of(byName.resultType)
      // `A & B` and `A | B` as written are applications of the aliases `&` and `|`.
      case World.Named(ref, List(first: Type, second: Type))
          if ref.optSymbol
            .exists(s => s.owner == defn.scalaPackage && Set("&", "|")(s.name.toString)) =>
        of(first) ++ of(second)
      case self: ThisType           => List(self.cls)
      case and: AndType             => of(and.first) ++ of(and.second)
      case or: OrType               => of(or.first) ++ of(or.second)
      case annotated: AnnotatedType => of(annotated.typ)
      case other                    =>
        World
          .classNamed(other)
          .map(List(_))
          .getOrElse(other.dealias match {
            case ref: TypeRef =>
              ref.optSymbol
                .collect { case b: TypeSymbolWithBounds => of(b.declaredBounds.high) }
                .getOrElse(Nil)
            case _ => Nil
          })
    }
    Try(term.tpe.widenTermRef match {
      case tpe: Type => of(tpe).distinct
      case _         => Nil
    }).getOrElse(Nil)
  }

}

private[compiled] object Matches {

  /** What a pattern may call, by name, that `Reentry`'s `referrers` notes as referenced. */
  def selected(pattern: PatternTree): List[String] = pattern match {
    case Unapply(_, _, patterns) =>
      List("isEmpty", "get", "lengthCompare", "apply", "drop", "toSeq") ++
        (1 to patterns.size + 1).map(n => s"_$n")
    case _ => List("equals", "==")
  }

}
