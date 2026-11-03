package cardinality.analysis.expansion

import scala.collection.mutable
import scala.meta.*
import scala.util.control.NonFatal

import cardinality.analysis.MethodAnalysis.{Frame, Target}
import cardinality.analysis.Resolver
import cardinality.analysis.model.MemberSyntax
import cardinality.types.Spelling

/** Which inline methods count as methods (register D27).
  *
  * An expansion differs from the method's own body only through what Scala resolves at the call
  * site: `inline if` and `inline match` reduce on the arguments' static types and constants,
  * `scala.compiletime` reads types, constants, givens and code, a quote or splice runs a macro, and
  * a call of an inline method brings its body along to be expanded there — including an inline
  * override the call site's own classes select on a parameter or on `this` (fixtures review-A6 h3,
  * h7, h8). Everything else in the body — names, overloads, the givens it passes — is typed once,
  * where the method is written (fixture a6 b1: an overload picked in the body is the same at every
  * call site). An `inline` parameter is its argument substituted by name; with no side effects that
  * is its value. So a body free of those operations, and calling only methods whose bodies are,
  * makes every expansion the same function of the values its signature names — up to the members
  * the model excludes, which boxing lets an expansion read differently (`getClass`, `==` on a type
  * parameter's values: fixture review-A6 h1): it counts as a `def`. Every other inline method is
  * refused with what failed, and so is every definition inside one, which each expansion copies; a
  * constructor's value space is the same in every copy.
  */
final private[cardinality] class InlineBodies(resolver: Resolver) {

  private val terms = new BodyTerms(resolver)
  private val conversions = new Conversions(resolver, terms)
  private val walk = new BodyWalk(terms, verdict, fallback, resolver)
  private val restated = new RestatedExtensions(terms, resolver)

  // Verdicts by definition. A call back into a definition still being read (a cycle of inline
  // calls) adds nothing the walk does not reach anyway, so it passes for now; the cycle is one
  // strongly connected component, decided with the definition of it opened first. If that one
  // passes, every verdict read inside the cycle holds and is kept; if it fails, only its own is,
  // and the others are read again from it. `lowest` is the depth of the earliest open definition
  // the current reading met, saved and restored around each reading.
  private val verdicts = mutable.Map.empty[String, Option[String]]
  private val open = mutable.Map.empty[String, Int]
  private val pending = mutable.ListBuffer.empty[(String, Option[String])]
  private var lowest = Int.MaxValue

  /** Why a target is not counted as a method: it is an inline method whose body fails, or a
    * definition inside one.
    */
  def refusal(target: Target): Option[String] =
    if (MemberSyntax.inline(target.tree))
      verdict(Definition(target.tree, target.frame)).map(what => s"${InlineBodies.Prefix}: $what")
    else if (target.constructor) None
    else
      target.frame.chain
        .drop(1)
        .iterator
        .flatMap(frame => frame.tree.filter(MemberSyntax.inline).map(Definition(_, frame)))
        .flatMap(owner => verdict(owner).map(owner.name -> _))
        .nextOption()
        .map((owner, what) =>
          s"${InlineBodies.Inside}: $owner $what, and every expansion copies this definition"
        )

  /** The first reason an inline definition's expansions differ, as a phrase; None when its body
    * passes.
    */
  def verdict(definition: Definition): Option[String] = {
    val key = definition.frame.id
    (verdicts.get(key), open.get(key)) match {
      case (Some(known), _)    => known
      case (None, Some(depth)) =>
        lowest = lowest.min(depth)
        None
      case (None, None) => read(key, definition)
    }
  }

  private def read(key: String, definition: Definition): Option[String] = {
    val depth = open.size
    val outer = lowest
    val mark = pending.size
    lowest = Int.MaxValue
    open.update(key, depth)
    val found =
      try judged(definition)
      catch {
        // A reading cut short (an exhausted budget) decides nothing: no verdict is kept for the
        // definition or for any pending on it, so whoever asks next reads them again rather than
        // meeting a definition left open, which would pass.
        case NonFatal(failure) =>
          open.remove(key)
          pending.remove(mark, pending.size - mark)
          lowest = outer
          throw failure
      }
    open.remove(key)
    if (lowest >= depth) {
      val members = pending.drop(mark).toList
      pending.remove(mark, members.size)
      if (found.isEmpty) members.foreach(verdicts.update)
      verdicts.update(key, found)
      lowest = outer
    } else {
      pending += key -> found
      lowest = outer.min(lowest)
    }
    found
  }

  private def judged(definition: Definition): Option[String] =
    // The body an inline method or given alias expands; an abstract declaration has none.
    MemberSyntax.rightHandSide(definition.tree) match {
      case None       => Some("is an abstract inline declaration, with no body to check")
      case Some(body) =>
        implemented(definition)
          .orElse(contextResult(definition.tree))
          .orElse(walk.body(body, definition.frame, defaults(definition.tree)))
          .orElse(inserted(definition))
    }

  // A body typed against a result that holds a context function may bind a lambda's parameter
  // to one, which implicit search then applies where it is read.
  private def contextResult(tree: Tree): Option[String] =
    MemberSyntax
      .result(tree)
      .filter(walk.contextual)
      .map(tpe =>
        s"declares the result ${Spelling.of(tpe)}, which holds a context function type implicit " +
          "search may apply"
      )

  // The compiler may insert an implicit conversion wherever a body's expression does not conform
  // to its expected type, which this reading does not type: one the sources declare inline, or
  // one a channel to declarations outside them may bring, would be expanded at each call site.
  private def inserted(definition: Definition): Option[String] =
    if (Unconverted.returnsOwnParameter(definition, terms)) None
    else
      conversions
        .inlineConversion(definition.frame)
        .map(c => s"may expand the inline conversion ${c.label}, which this check does not place")
        .orElse(
          resolver
            .unreadChannel(terms.at(definition.frame, definition.tree.pos.start))
            .map(channel =>
              s"may expand an implicit conversion from outside the sources ($channel)"
            )
        )

  /** Why an applied selection of `name` read in `frame` may run code the member reading did not
    * resolve: Scala tries an extension or a conversion when no member of the name accepts the
    * arguments, which this reading does not type — unless every extension restates the member
    * (`RestatedExtensions`).
    */
  private def fallback(applied: RestatedExtensions.Applied, frame: Frame): Option[String] = {
    val name = applied.name
    conversions
      .conversion(frame)
      .map(c =>
        s"applies $name, whose arguments this reading does not type: if no member accepts them, " +
          s"the implicit conversion ${c.label} may answer"
      )
      .orElse {
        val extensions = conversions.extensionsOf(name, frame)
        Option.when(extensions.any && !applied.call.exists(restated.answered(_, extensions)))(
          s"applies $name, which an extension of that name may answer when no member applies"
        )
      }
  }

  private def defaults(tree: Tree): List[Term] =
    MemberSyntax.parameters(MemberSyntax.groups(tree)).flatMap(_.default)

  // An inline method implementing an abstract inline declaration of a supplied ancestor is read
  // through that declaration, which has no body; the reading stays with the declaration.
  private def implemented(definition: Definition): Option[String] = {
    val name = definition.name
    val templates = definition.frame.parent.filter(_.isTemplate).iterator
    templates
      .flatMap(terms.levels(_).toOption.iterator.flatten)
      .flatMap(level =>
        level.stats.iterator.collect {
          case declared: Decl.Def if declared.name.value == name && MemberSyntax.inline(declared) =>
            s"implements the abstract inline declaration ${level.path.mkString(".")}.$name, " +
              "which has no body to check"
        }
      )
      .nextOption()
  }

}

private[cardinality] object InlineBodies {

  val Prefix = "inline method"

  val Inside = "definition inside an inline method"

  /** Whether a definition is expanded where it is called: an inline method or given alias, or a
    * definition inside one, which every expansion copies.
    */
  def expanded(target: Target): Boolean =
    !target.constructor &&
      (MemberSyntax.inline(target.tree) ||
        target.frame.chain.drop(1).exists(_.tree.exists(MemberSyntax.inline)))

}
