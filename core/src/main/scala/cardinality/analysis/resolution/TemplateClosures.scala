package cardinality.analysis.resolution

import scala.meta.{Defn, Mod, Name, Pkg, Stat, Term, Tree, XtensionClassifiable}

import cardinality.analysis.MethodAnalysis.*
import cardinality.analysis.SealedReceivers
import cardinality.types.TypeApplications

/** Whether a subclass written anywhere may extend a template and pick its abstract members, which a
  * member atom's parity with a type parameter needs (members M1). Where only some scope may extend
  * it, that scope's subclasses are the only choices, and none is enumerated.
  */
private[cardinality] object TemplateClosures {

  /** Why no subclass written anywhere may extend a template and pick its members. */
  enum Closure {
    case Final
    case PrivateConstructor(ancestor: Option[String])
    case Sealed
    case Hidden(why: String)
    case Unsupplied(parent: String)
  }

  /** The closures of a template, given its supplied linearization (itself first), in order: no
    * subclass at all (a final class, abstract or not, a value class, an object, a given), one only
    * where a private constructor (its own or an ancestor class's) is reachable, one only in its
    * file (sealed), one only inside a private or local scope.
    */
  def of(template: Frame, linearization: List[Frame]): List[Closure] = {
    val own = template.tree.toList.flatMap { tree =>
      List(
        Option.when(neverExtended(tree))(Closure.Final),
        Option.when(privateConstructor(tree))(Closure.PrivateConstructor(None)),
        Option.when(SealedReceivers.sealedTree(tree))(Closure.Sealed)
      ).flatten
    }
    val ancestor = linearization.drop(1).collectFirst {
      case frame if frame.tree.exists(privateConstructor) =>
        Closure.PrivateConstructor(frame.path.lastOption)
    }
    own ++ ancestor ++ hiddenScope(template)
  }

  // A template no subclass extends: a final class (a `final abstract` one has no instance at all; a
  // value class is final), an object or a given.
  private def neverExtended(tree: Tree): Boolean =
    FinalImplementations.isFinal(tree) || tree.is[Pkg.Object] || (tree match {
      case c: Defn.Class => TypeApplications.valueClass(c) || Representations.finalAbstract(c)
      case _             => false
    })

  // A class whose constructor is private: only its own body and companion can extend it.
  private def privateConstructor(tree: Tree): Boolean = tree match {
    case c: Defn.Class => c.ctor.mods.exists(_.is[Mod.Private])
    case _             => false
  }

  /** Whether only code of the template's own file may extend it: it is local, it or an enclosing
    * template is private to a template, or its own constructor is (its companion is in the file). A
    * top-level `private` template is visible in its whole package, an ancestor's private
    * constructor closes no public subclass, and a `private[p]` constructor is reached package-wide.
    */
  def fileScoped(template: Frame): Boolean =
    template.chain.exists(frame =>
      frame.local || (frame.parent.exists(_.isTemplate) && frame.tree.exists(privateTo(_, frame)))
    ) || template.tree.exists {
      case c: Defn.Class => confining(c.ctor.mods, template.chain)
      case _             => false
    }

  private def hiddenScope(template: Frame): Option[Closure] =
    template.chain.collectFirst {
      case frame if frame.local => Closure.Hidden(s"${template.path.last} is local")
      case frame if frame.tree.exists(privateTo(_, frame)) =>
        Closure.Hidden(s"${frame.path.last} is private")
    }

  // A private modifier that confines a template to a template's scope: unqualified, `this`, or
  // naming an enclosing template rather than a package.
  private def privateTo(tree: Tree, frame: Frame): Boolean = tree match {
    case d: Stat.WithMods => confining(d.mods, frame.chain.drop(1))
    case _                => false
  }

  // A `private` among `mods` whose qualifier, if any, names one of the `scopes` templates.
  private def confining(mods: List[Mod], scopes: List[Frame]): Boolean =
    mods.collectFirst { case m: Mod.Private => m.within }.exists {
      case _: Name.Anonymous | _: Term.This => true
      case name: Name => scopes.exists(f => f.path.lastOption.contains(name.value) && f.isTemplate)
      case _          => false
    }

}
