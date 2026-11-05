package cardinality.analysis.model

import scala.meta.*

/** The name Scala 3 gives an anonymous given: `given_` and the names its type spells. A member of
  * that name is the given, so it implements, overrides or is implemented by a declaration of it
  * like a named one (`given A = a` in a class is `given_A`, the declaration `given given_A: A` of a
  * parent).
  *
  * The type contributes its simple name, an applied type its constructor's and those of its
  * arguments when they are plain names (`Ord[Int]` is `given_Ord_Int`); a structural given joins
  * its parents'. Any other type is not spelled here, and its given's name is not known (`unknown`).
  * Every rule is 3.9.0's, read off `-Xprint:typer` (compiler fixtures `$S/probes/b5-fix2/gname`).
  */
private[cardinality] object GivenNames {

  /** A given definition of any form. */
  type Given = Defn.GivenAlias | Decl.Given | Defn.Given

  /** The synthesized name of an anonymous given; none for a named one, or for one whose name is not
    * known (`unknown`).
    */
  def of(definition: Given): Option[String] =
    if (definition.name.value.nonEmpty) None else spelled(definition)

  private def spelled(definition: Given): Option[String] = definition match {
    case g: Defn.GivenAlias => top(g.decltpe).map("given_" + _)
    case g: Defn.Given      =>
      g.templ.inits
        .foldRight(Option(List.empty[String]))((init, acc) => top(init.tpe).zip(acc).map(_ :: _))
        .map(_.mkString("given_", "_", ""))
    case _ => None
  }

  /** Whether an anonymous given's name is not known: its type is not one spelled here. Such a given
    * may be any member `given_…`, and is a member of the name `Unknown`, which no identifier spells
    * unquoted.
    */
  def unknown(definition: Tree): Boolean = definition match {
    case g: Given if g.name.value.isEmpty => spelled(g).isEmpty
    case _                                => false
  }

  val Unknown = "<given>"

  /** Whether a member of a name may be the given whose name is not known. */
  def mayBe(definition: Tree, name: String): Boolean =
    unknown(definition) && name.startsWith("given_")

  private def top(tpe: Type): Option[String] = tpe match {
    case a: Type.Apply if a.argClause.values.nonEmpty =>
      val args = a.argClause.values.map(name)
      Option
        .when(args.forall(_.nonEmpty))(args.flatten)
        .flatMap(spelled => name(a.tpe).map(c => (c :: spelled).mkString("_")))
    case other => name(other)
  }

  private def name(tpe: Type): Option[String] = tpe match {
    case Type.Name("?" | "_") => None
    case n: Type.Name         => Some(n.value)
    case _                    => None
  }

}
