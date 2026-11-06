package cardinality.analysis.expansion

import scala.collection.mutable
import scala.meta.*
import scala.util.control.NonFatal

import cardinality.analysis.MethodAnalysis.{Frame, Target}
import cardinality.analysis.Resolver
import cardinality.analysis.model.MemberSyntax
import cardinality.types.{Spelling, TypeApplications}

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

  private val terms = new BodyTerms(resolver, failing)
  private val conversions = new Conversions(resolver, terms)
  private val walk = new BodyWalk(terms, verdict, fallback, resolver)
  private val restated = new RestatedExtensions(terms, resolver)
  private val reachable = new Reachable(resolver, conversions)

  // The conversion checks of each body under way that wait for its reachable types: an applied
  // selection whose qualifier's implicit scope is not read (`fallback`).
  private val deferred =
    mutable.Stack.empty[mutable.ListBuffer[Option[Set[String]] => Option[String]]]

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
    // An abstract declaration has none: it counts as an abstract `def` does, and an override
    // implementing it is read through its own body.
    MemberSyntax.rightHandSide(definition.tree).flatMap { body =>
      contextResult(definition.tree).orElse {
        deferred.push(mutable.ListBuffer.empty)
        val ((found, heard), waiting) =
          try (walk.read(body, definition.frame, defaults(definition.tree)), deferred.head.toList)
          finally deferred.pop(): Unit
        lazy val anchors = reachable.scopes(definition, heard)
        found
          .orElse(waiting.iterator.flatMap(_(anchors)).nextOption())
          .orElse(inserted(definition, anchors))
      }
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
  // to its expected type, which this reading does not type. The closed world supplies every
  // conversion, and one is typed where the body is written, as any call is: only an inline one
  // whose own expansion fails (or which this check cannot read) differs at each call site, and
  // so does one a channel to declarations outside the sources may bring.
  // A conversion is a candidate where it is in the body's lexical scope or in the implicit scope of
  // a type the body can reach (`anchors`, `Reachable`).
  private def inserted(definition: Definition, anchors: => Option[Set[String]]): Option[String] =
    if (Unconverted.returnsOwnParameter(definition, terms)) None
    else
      conversions
        .inlineConversions(definition.frame, anchors)
        .flatMap(c =>
          expands(c).map(why => s"may expand the inline conversion ${c.label}, which $why")
        )
        .nextOption()
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
  private def fallback(
      applied: RestatedExtensions.Applied,
      frame: Frame,
      qualifier: Option[Meaning]
  ): Option[String] = {
    val name = applied.name
    val anchors = conversions.anchored(qualifier)
    val extended = {
      val extensions = conversions.extensionsOf(name, frame, anchors)
      Option.when(extensions.any && !applied.call.exists(restated.answered(_, extensions)))(
        s"applies $name, which an extension of that name may answer when no member applies"
      )
    }
    // A qualifier whose implicit scope is not read converts through the body's reachable types,
    // read once the walk has resolved every name of the body.
    if (anchors.isEmpty && deferred.nonEmpty) {
      deferred.head += (reached => convertedApply(name, frame, reached).orElse(extended))
      None
    } else convertedApply(name, frame, anchors).orElse(extended)
  }

  private def convertedApply(
      name: String,
      frame: Frame,
      anchors: Option[Set[String]]
  ): Option[String] =
    conversions
      .conversion(frame, anchors)
      .flatMap(c => converted(name, frame, anchors).map((label, why) => (c, label, why)))
      .map((c, label, why) =>
        s"applies $name, whose arguments this reading does not type: if no member accepts them, " +
          s"the implicit conversion ${c.label} may answer, and $label$why"
      )

  private def defaults(tree: Tree): List[Term] =
    MemberSyntax.parameters(MemberSyntax.groups(tree)).flatMap(_.default)

  // Why an inline conversion's expansion differs at each call site: its own body fails, or it is
  // an instance whose inline `apply` this check does not place.
  private def expands(c: conversions.Candidate): Option[String] =
    c.expands.fold(Some("this check does not place"))(verdict)

  // Why a supplied inline method of the name differs at each call site, if one does (the closed
  // world: an override an expansion may select is one of them). An override of a member of
  // `below` (a supplied template, or a library class by its key) is a member of a template that
  // may extend it: an extension method or a member of an unrelated template is never selected for
  // it.
  private def failing(name: String, below: Option[Either[String, Frame]]): Option[String] =
    conversions
      .inlineMethods(name)
      .iterator
      .filter(d => below.forall(overrides(d, _)))
      .flatMap(d => verdict(d).map(why => s"inline method ${d.frame.path.mkString(".")} $why"))
      .nextOption()

  private def overrides(d: Definition, below: Either[String, Frame]): Boolean =
    d.frame.parent.exists(owner =>
      owner.isTemplate && !owner.tree.exists(_.is[Defn.ExtensionGroup]) &&
        below.fold(libraryParent(owner, _), resolver.mayExtend(owner, _))
    )

  // Whether a template may extend a library class (a function class, by its key): some class on
  // its ancestry is outside the sources, or a supplied one names a function type as a parent.
  private def libraryParent(owner: Frame, key: String): Boolean = {
    val (bases, complete) = resolver.baseClasses(owner)
    !complete || bases.exists(functionParents.getOrElse(_, Set.empty)(key.stripPrefix("scala.")))
  }

  // The library classes each supplied template names as a parent: a function type as its class.
  private lazy val functionParents: Map[String, Set[String]] =
    resolver.memberScopes
      .map(f =>
        f.id -> f.parents
          .map(init =>
            init.tpe match {
              case t: Type.Function        => s"Function${t.paramClause.values.size}"
              case t: Type.ContextFunction => s"Function${t.paramClause.values.size}"
              case _                       => TypeApplications.parentName(init)
            }
          )
          .toSet
      )
      .toMap

  // What a conversion's result may run under an applied name whose expansion differs at each call
  // site (the closed world: a supplied inline method of the name), or a failing inline conversion
  // that may be inserted: none, and the call is typed where the body is written.
  private def converted(
      name: String,
      frame: Frame,
      anchors: Option[Set[String]]
  ): Option[(String, String)] =
    conversions
      .inlineConversions(frame, anchors)
      .flatMap(c => expands(c).map(why => s"the inline conversion ${c.label} " -> why))
      .nextOption()
      .orElse(failing(name, None).map("" -> _))

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
