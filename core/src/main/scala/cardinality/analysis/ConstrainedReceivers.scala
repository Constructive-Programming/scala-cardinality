package cardinality.analysis

import scala.meta.*

import cardinality.analysis.model.MemberSyntax

import MethodAnalysis.Frame

/** A member reads its receiver's fields as any values of their types. That holds only where any
  * program may build a receiver from any field values: a class only its own code may instantiate —
  * its constructor or the class itself non-public, nested in a non-public template, or local to a
  * block — is built by that code alone (`NonEmpty.of`), which may keep a field from holding some
  * values (`xs` is never `Nil`). Counting every field value would answer for receivers no program
  * builds, so such fields are an unreadable environment until those factories are read.
  */
private[cardinality] object ConstrainedReceivers {

  def reason(owner: Frame): Option[String] =
    owner.tree
      .filter(tree => owner.params.nonEmpty && (tree.is[Defn.Class] || tree.is[Defn.Trait]))
      .flatMap(tree => builder(owner, tree))
      .map(why => s"receiver fields constrained by $why: ${owner.path.mkString(".")}")

  // Why only some code may build the template, nearest scope first; packages bound the search
  // (a qualified-private class is visible only inside its package, which the class itself says).
  private def builder(owner: Frame, tree: Tree): Option[String] = {
    val kind = if (tree.is[Defn.Trait]) "trait" else "class"
    val constructor = tree match {
      case d: Defn.Class => d.ctor.mods.exists(MemberSyntax.restricted)
      case _             => false
    }
    Option
      .when(constructor)("a non-public constructor")
      .orElse(Option.when(modifiers(tree).exists(MemberSyntax.restricted))(s"a non-public $kind"))
      .orElse(owner.chain.tail.takeWhile(_.tree.nonEmpty).collectFirst {
        case scope if scope.tree.exists(modifiers(_).exists(MemberSyntax.restricted)) =>
          s"a $kind nested in non-public ${scope.path.mkString(".")}"
      })
  }

  private def modifiers(tree: Tree): List[Mod] = tree match {
    case d: Defn.Class  => d.mods
    case d: Defn.Object => d.mods
    case _              => Nil
  }

}
