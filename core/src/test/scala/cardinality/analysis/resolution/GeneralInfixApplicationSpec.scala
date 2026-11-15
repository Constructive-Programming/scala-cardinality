package cardinality.analysis.resolution

import cardinality.Pending
import cardinality.analysis.inhabitation.*
import cardinality.reporting.Report
import org.specs2.Specification

class GeneralInfixApplicationSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    General infix type application
      resolves aliases with the same operand order as prefix syntax       $aliases
      resolves source products in methods and constructor fields          $products
      respects left and right associative parser nesting                  $associativity
      resolves nested qualified and root-qualified prefix operands        $qualified
      enforces the same source constructor arity                          ${Pending.rejectedFixture(
      arity
    )}
      reports missing external effect dependencies rather than syntax     ${Pending.knownLoss(
      externalEffects
    )}
      reads an infix source trait as the record prefix syntax reads        $effectRepresentation
  """

  private def entries(code: String): List[Report.Entry] =
    cardinality.Compiled.analyze(cardinality.Compiled.inputs("Infix.scala" -> code))

  private def count(definitions: String, result: String, support: String = ""): Count =
    entries(s"$definitions\ndef pick[A](x: A, y: A): $result = ???$support")
      .find(_.name == "pick")
      .get
      .count

  def aliases = {
    val definitions = "type First[A, B] = A"
    val prefix = count(definitions, "First[A, Unit]")
    (prefix === Count.Finite(2))
      .and(count(definitions, "A First Unit") === prefix)
      .and(count(definitions, "A `First` Unit") === prefix)
      .and(count(definitions, "`First`[A, Unit]") === prefix)
  }

  def products = {
    val definitions = "case class Pair[A, B](first: A, second: B)"
    val prefix = count(definitions, "Pair[A, A]")
    val constructorCounts = entries(
      definitions + "\ncase class InfixBox[A](value: A Pair Unit)" +
        "\ncase class PrefixBox[A](value: Pair[A, Unit])"
    ).filter(e => e.name == "InfixBox.<init>" || e.name == "PrefixBox.<init>").map(_.count)
    (prefix === Count.Finite(4))
      .and(count(definitions, "A Pair A") === prefix)
      .and(constructorCounts === List(Count.Finite(1), Count.Finite(1)))
  }

  def associativity = {
    // Duplicating the right field makes regrouping observable: 16 vs 4 implementations.
    val definitions = "type Nest[A, B] = (A, B, B)\ntype :*:[A, B] = (A, B, B)"
    val left = count(definitions, "Nest[Nest[Unit, Unit], Boolean]")
    val right = count(definitions, ":*:[Unit, :*:[Unit, Boolean]]")
    (left === Count.Finite(4))
      .and(right === Count.Finite(16))
      .and(count(definitions, "Unit Nest Unit Nest Boolean") === left)
      .and(count(definitions, "Unit :*: Unit :*: Boolean") === right)
      .and(count(definitions, "(Unit :*: Unit) :*: Boolean") === left)
  }

  // `Join[Item[A], Option[A]]` is `(A, Option[A])`: x or y, beside None, Some(x) or Some(y): 6.
  // `_root_` reaches a package's members, not the empty package's, so Models sits in a package.
  def qualified = {
    val definitions =
      "package m { object Models { type Item[A] = A; type Join[A, B] = (A, B) } }\n" +
        "type Join[A, B] = m.Models.Join[A, B]"
    val prefix = count(definitions, "_root_.m.Models.Join[m.Models.Item[A], scala.Option[A]]")
    (prefix === Count.Finite(6))
      .and(count(definitions, "m.Models.Item[A] Join scala.Option[A]") === prefix)
      .and(count(definitions, "Join[m.Models.Item[A], _root_.scala.Option[A]]") === prefix)
  }

  def arity = {
    val definitions = "type Unary[A] = A"
    val prefix = count(definitions, "Unary[A, Unit]")
    (prefix === Count.Unresolved(List("type argument arity: Unary")))
      .and(count(definitions, "A Unary Unit") === prefix)
  }

  def externalEffects = {
    val pairs = List(
      "A < Env[A]" -> "<[A, Env[A]]",
      "Unit < Var[A]" -> "<[Unit, Var[A]]",
      "Option[A] < Var[A]" -> "<[Option[A], Var[A]]",
      "A < Unit" -> "<[A, Unit]"
    )
    // The callee is read first: with `<` unsupplied, nothing says whether its arguments are types
    // or constructors, so the missing callee is the one true reason, whatever its arguments name.
    // The fixtures are compiled against these declarations, which the analysis is not given.
    val effects = cardinality.Compiled.withheld("class <[A, S]\nclass Env[A]\nclass Var[A]")
    pairs
      .map { (infix, prefix) =>
        (count("", infix, effects) === count("", prefix, effects))
          .and(
            count("", infix, effects) === Count.Unresolved(
              List(
                s"declaration not supplied: < (${"not declared in the sources or stdlib-model/1"})"
              )
            )
          )
      }
      .reduce(_.and(_))
  }

  // The trait is the record of its one member (capabilities C4): `run` returns x or y — 2.
  def effectRepresentation = {
    val definitions = "trait <[A, S] { def run: A }"
    val prefix = count(definitions, "<[A, Unit]")
    (prefix === Count.Finite(2)).and(count(definitions, "A < Unit") === prefix)
  }

}
