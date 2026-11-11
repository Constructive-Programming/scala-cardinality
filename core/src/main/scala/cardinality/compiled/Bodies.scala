package cardinality.compiled

import tastyquery.Contexts.*
import tastyquery.Modifiers.TermSymbolKind
import tastyquery.Trees.*

/** What the counting model reads of a template's construction, over typed trees (register D26):
  * whether its constructor always throws, and whether it holds a pattern definition, whose values
  * are not read yet.
  */
private[compiled] object Bodies {

  /** Whether building an instance of a template always throws, by what the template itself
    * evaluates: the arguments it passes to its parents' constructors, each strict value's
    * initializer and each statement of its body, but `excluded` (the measured member, whose
    * initializer is the choice being counted).
    */
  def alwaysThrows(template: ClassDef, excluded: Option[Tree] = None)(using Context): Boolean =
    (arguments(template) ++ evaluated(template, excluded)).exists(throws)

  private def evaluated(template: ClassDef, excluded: Option[Tree]): List[TermTree] =
    template.rhs.body.filterNot(stat => excluded.exists(_ eq stat)).flatMap {
      case v: ValDef if v.symbol.kind != TermSymbolKind.LazyVal => v.rhs.toList
      case term: TermTree                                       => List(term)
      case _                                                    => Nil
    }

  // The arguments a template passes to its parents' constructors: named ones out of order are
  // values of a block around the call, one in order keeps its name, and repeated ones are a
  // sequence literal. A by-name argument is taken as evaluated.
  private def arguments(template: ClassDef): List[TermTree] =
    template.rhs.parents
      .flatMap {
        case call: Apply               => passed(call)
        case Block(stats, call: Apply) =>
          stats.collect { case v: ValDef => v.rhs }.flatten ++ passed(call)
        case _ => Nil
      }
      .flatMap(each)

  private def passed(call: Apply): List[TermTree] = call.fun match {
    case inner: Apply => passed(inner) ++ call.args
    case _            => call.args
  }

  private def each(argument: TermTree): List[TermTree] = argument match {
    case NamedArg(_, value)             => each(value)
    case Typed(sequence: SeqLiteral, _) => each(sequence)
    case SeqLiteral(elems, _)           => elems.flatMap(each)
    case plain                          => List(plain)
  }

  // Whether evaluating a term always throws: `throw`, the library's `???` and `sys.error`, one of
  // them ascribed, a block whose result throws, a conditional or match every branch of which
  // throws.
  private def throws(term: TermTree)(using Context): Boolean = term match {
    case _: Throw                         => true
    case ref: TermReferenceTree           => ref.symbol.displayFullName == "scala.Predef$.???"
    case Apply(fun: TermReferenceTree, _) =>
      fun.symbol.displayFullName == "scala.sys.package$.error"
    case Typed(inner, _)  => throws(inner)
    case Block(_, result) => throws(result)
    case If(_, yes, no)   => throws(yes) && throws(no)
    case Match(_, cases)  => cases.nonEmpty && cases.forall(c => throws(c.body))
    case _                => false
  }

  /** Whether a template holds a pattern or multi-name value definition: one statement of the source
    * and several values of the tree. `val a, b = …` is a value per name, all starting where the
    * definition does; `val (a, b) = …` a value per name, each positioned at its name alone, with a
    * holder the compiler made or none; `val Some(x) = e` and `val _ = e` a match on `e` ascribed
    * (so is a match written on an ascribed term). A parameter, an enum's value case, its `$values`
    * and an object are no such definition.
    */
  def destructures(template: ClassDef): Boolean = {
    val body = template.rhs.body
    val values = body.collect {
      case v: ValDef
          if !(v.symbol.isModuleVal || v.symbol.isParamAccessor || v.symbol.isEnumCase) =>
        v
    }
    val (made, written) = values.partition(_.symbol.isSynthetic)
    // A value the compiler places without extent is its own (a `derives` clause's instance).
    val placed = written.filterNot(v => v.pos.isUnknown || v.pos.isZeroExtent)
    val terms = values.flatMap(_.rhs) ++ body.collect { case term: TermTree => term }
    made.exists(_.name.toString != "$values") ||
    terms.exists {
      case Match(_: Typed, _) => true
      case _                  => false
    } || placed.exists(v => v.pos.startOffset == v.pos.pointOffset) ||
    placed.groupBy(_.pos.startOffset).exists(_._2.sizeIs > 1)
  }

}
