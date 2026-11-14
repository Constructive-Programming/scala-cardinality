package cardinality

import scala.meta.*

/** The overriders that open a fixture's members. A member is open in the closed world exactly where
  * a supplied template overrides it (AGENTS.md), so a fixture about an open member supplies the
  * overrider: beside each class and trait that is not final, `opened` writes a template that
  * extends it and overrides each of its concrete methods that is not final, by calling it.
  *
  * `trait T { def target(b: Boolean): Boolean; def m: Boolean = target(true) }` gets `trait T__O
  * extends T { override def m : Boolean = super.m }`. A template nested in a class is overridden
  * inside the class's own overrider; one nested in an object beside the object.
  *
  * It reads the fixture's text, and writes only what that text is enough for. A fixture writes for
  * itself the overrider of a `val`, of a method written without a result type, of an inherited
  * member and of a template in a final class. Templates in a package clause, repeated and default
  * parameters, context parameters of a constructor and modifiers other than `final` are not
  * handled: a fixture with one fails to compile or keeps the member final, and its example fails.
  */
object Overriders {

  /** The fixture with its overriders, each written at the end of the source it overrides. Withheld
    * support is left as it is.
    */
  def opened(inputs: List[Compiled.Input]): List[Compiled.Input] =
    inputs.map { input =>
      templates(dialects.Scala3(input.text).parse[Source].get.stats, "") match {
        case written if written.nonEmpty && !input.path.startsWith(Compiled.Withheld) =>
          input.copy(text = input.text + written.mkString("\n", "\n", "\n"))
        case _ => input
      }
    }

  // The overriders of the templates among `stats`, which other code reaches as `prefix` + name.
  private def templates(stats: List[Stat], prefix: String): List[String] = stats.flatMap {
    case o: Defn.Object => templates(o.templ.body.stats, s"$prefix${o.name.value}.")
    case c: Defn.Class if !c.mods.exists(_.is[Mod.Final]) =>
      val clauses = c.ctor.paramClauses.toList
      val kind = if (c.mods.exists(_.is[Mod.Abstract])) "abstract class" else "class"
      // The constructor's parameters without what makes them members, and passed on.
      val declared = clauses.map(_.values.map(p => s"${p.name.value}: ${p.decltpe.get.syntax}"))
      val overrider = s"$kind ${c.name.value}__O${c.tparamClause.syntax}" +
        declared.map(_.mkString("(", ", ", ")")).mkString
      // A class is open where a template extends it, whatever that overrides.
      template(
        overrider,
        parent(prefix, c.name.value, c.tparamClause, clauses.map(arguments)),
        c.templ
      )
        .map(_._1)
    case t: Defn.Trait =>
      val overrider = s"trait ${t.name.value}__O${t.tparamClause.syntax}"
      // A trait is open whatever extends it: one with nothing to override gets no overrider.
      template(overrider, parent(prefix, t.name.value, t.tparamClause, Nil), t.templ).collect {
        case (written, true) => written
      }
    case _ => Nil
  }

  // The template an overrider extends, as written from where it stands.
  private def parent(prefix: String, name: String, tparams: Type.ParamClause, args: List[String]) =
    s"$prefix$name${typeArguments(tparams)}${args.mkString}"

  // One overrider, and whether it overrides anything: its head, the template it extends (`parent`),
  // and the members it overrides.
  private def template(
      overrider: String,
      parent: String,
      templ: Template
  ): List[(String, Boolean)] = {
    val members = templ.body.stats.collect {
      case d: Defn.Def if !d.mods.exists(_.is[Mod.Final]) && d.decltpe.nonEmpty =>
        val groups = d.paramClauseGroups
        val declared =
          groups.map(g => g.tparamClause.syntax + g.paramClauses.map(_.syntax).mkString)
        val applied =
          groups.map(g => typeArguments(g.tparamClause) + g.paramClauses.map(arguments).mkString)
        // A symbolic name is kept apart from the colon after it.
        s"override def ${d.name.syntax}${declared.mkString} : ${d.decltpe.get.syntax} = " +
          s"super.${d.name.syntax}${applied.mkString}"
    }
    // A nested template is a member its overrider inherits: it is named as it is written.
    val nested = templates(templ.body.stats, "")
    List(
      (
        s"$overrider extends $parent " +
          (members ++ nested).mkString("{ ", "; ", " }"),
        members.nonEmpty || nested.nonEmpty
      )
    )
  }

  private def typeArguments(clause: Type.ParamClause): String =
    if (clause.values.isEmpty) "" else clause.values.map(_.name.value).mkString("[", ", ", "]")

  // A clause's parameters passed on, a context one by `using`.
  private def arguments(params: Term.ParamClause): String = {
    val using = if (params.mod.exists(_.is[Mod.Using])) "using " else ""
    params.values.map(_.name.value).mkString(s"($using", ", ", ")")
  }

}
