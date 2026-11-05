package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.MethodAnalysis
import cardinality.analysis.model.MemberSyntax
import cardinality.types.Spelling

import MethodAnalysis.Target

/** What the call site of a `transparent inline` method observes (rule X): the expansion typed by
  * what its body returns — a subclass, a sealed case, an opaque type's representation (`opaque type
  * T = A` read as `A`), a literal's or a stable path's singleton type — which an overload tells
  * apart from equal values of the declared type (fixtures: docs/research/top-types-unions.md).
  */
private[cardinality] object TransparentResults {

  /** Whether the target is a `transparent inline` method, whose call site types the expansion. */
  def transparent(target: Target): Boolean =
    MemberSyntax.methodModifiers(target.tree).exists(_.is[Mod.Transparent])

  /** Why the call site may observe more than the declared result `tpe`. */
  def narrowed(tpe: Type): Option[String] =
    Some(
      "transparent inline result: the call site observes the expansion's type, " +
        s"not the declared ${Spelling.of(tpe)}"
    )

}
