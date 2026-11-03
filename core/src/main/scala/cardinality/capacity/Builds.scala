package cardinality.capacity

import scala.meta.*

import cardinality.analysis.model.{MemberSyntax, Throws}

// Whether an instance of a template is ever built (registers D26 and D30; CROSSCHECK X6). A class
// row counts what a constructor stores; a value row asks whether a value exists, and building one
// runs the primary constructor of the template and of every ancestor: the arguments of its
// `extends` clause, the defaults its parents fall back to, and the strict members and statements of
// its body. A term that only throws supplies nothing (`Throws`, D-pure), so `final case class K()
// extends Base(???)` has no instance, whatever its row says it stores (1); any other term yields a
// value of its type, as concrete code obeys the model (D-env-concrete) — the method side's
// `Construction` reads the same steps. This reading resolves no term name, so a term that throws if
// its names are the root imports' leaves the construction unread rather than empty, as does a
// private or protected constructor (`final class P private (i: Int)`: no caller is read), a context
// argument implicit search supplies, an enum's companion whose construction is not read (it builds
// the singleton cases), and a parent the estimate does not read (`ValueRowsSpec`'s
// `unbuiltValues`).
private[capacity] object Builds {

  /** Why no instance of `d`, written where `around` reads, is known to be built; None when every
    * step of its construction is read.
    */
  def unread(d: Defn, around: Scope): Option[String] = {
    val name = Walk.named(d).value
    access(d)
      .map(restricted => s"$name, whose constructor is $restricted")
      .orElse(level(d, around, name, Nil))
      .orElse(companion(d).flatMap(level(_, around, name, Nil)))
  }

  private def access(d: Defn): Option[String] = d match {
    case c: Defn.Class =>
      c.ctor.mods.collectFirst {
        case _: Mod.Private   => "private"
        case _: Mod.Protected => "protected"
      }
    case _ => None
  }

  // The companion of an enum with a singleton case: the cases are its values, built with it.
  private def companion(d: Defn): Option[Defn.Object] = d match {
    case e: Defn.Enum if EnumCompanions.singletons(e) => EnumCompanions.companion(e)
    case _                                            => None
  }

  /** Whether a body, built, evaluates a term that throws if its names are the root imports'. */
  def throwing(stats: List[Stat]): Boolean = Throws.evaluated(stats, None).exists(throws)

  private def throws(term: Term): Boolean = Throws.only(term, _ => true)

  // One level of the construction, then the ancestors its parents name, read where it is written.
  private def level(d: Defn, around: Scope, leaf: String, seen: List[Defn]): Option[String] =
    Walk.templateOf(d).flatMap { t =>
      if (Throws.arguments(t.inits ++ cases(d)).exists(throws) || throwing(t.body.stats))
        Some(thrown(leaf))
      else t.inits.iterator.flatMap(parent(_, around, leaf, d :: seen)).nextOption()
    }

  private def thrown(leaf: String): String = s"$leaf, whose construction evaluates a throw"

  // An enum's cases with their own `extends` clause pass its constructor their arguments.
  private def cases(d: Defn): List[Init] = d match {
    case e: Defn.Enum => e.templ.body.stats.collect { case c: Defn.EnumCase => c.inits }.flatten
    case _            => Nil
  }

  private def parent(init: Init, around: Scope, leaf: String, seen: List[Defn]): Option[String] =
    Inheritance.resolve(around, init.tpe) match {
      case Left(channel) => Some(s"$leaf, built on $channel")
      case Right(None)   => None
      case Right(Some(members)) if seen.exists(_ eq members.defn) => None
      case Right(Some(members))                                   =>
        omitted(init, members.defn, leaf)
          .orElse(level(members.defn, members.outside, leaf, seen))
    }

  // The parameters a parent's clause does not write: each takes its default, which is evaluated,
  // or — a trait's — another clause's argument; a context clause left out is filled by implicit
  // search, which is not read.
  private def omitted(init: Init, parent: Defn, leaf: String): Option[String] = {
    val clauses = parent match {
      case c: (Defn.Class | Defn.Trait) => c.ctor.paramClauses.toList
      case _                            => Nil
    }
    clauses.zipWithIndex.iterator
      .flatMap { (clause, i) =>
        init.argClauses.lift(i) match {
          case None if MemberSyntax.contextual(clause) =>
            Some(s"$leaf, whose parent ${init.tpe.syntax} takes context arguments not written")
          case written =>
            val args = written.fold(List.empty[Term])(_.values)
            val named = args.collect { case Term.Assign(Term.Name(n), _) => n }.toSet
            val positional = args.count(!_.is[Term.Assign])
            clause.values
              .drop(positional)
              .filterNot(p => named(p.name.value))
              .flatMap(_.default)
              .find(throws)
              .map(_ => thrown(leaf))
        }
      }
      .nextOption()
  }

}
