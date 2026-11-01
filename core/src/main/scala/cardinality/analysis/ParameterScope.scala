package cardinality.analysis

import cardinality.types.TypeApplications

import MethodAnalysis.Frame

/** Where a template's parameters are typed: outside its body and its parents, in the frame its
  * constructor reads them in (Scala 3.9.0 reads `class C(val a: Any) extends X` with `scala.Any`
  * beside X's `type Any`) — also where a member reads them as captures, or as evidence.
  */
private[cardinality] object ParameterScope {

  def of(owner: Frame): Frame =
    if (owner.tree.exists(TypeApplications.template(_).nonEmpty))
      owner.copy(stats = Nil, parents = Nil)
    else owner

}
