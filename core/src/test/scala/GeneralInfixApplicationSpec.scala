package cardinality

import scala.meta.*

import org.specs2.Specification

class GeneralInfixApplicationSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    General infix type application
      resolves aliases with the same operand order as prefix syntax       $aliases
      resolves source products in methods and constructor fields          $products
      respects left and right associative parser nesting                  $associativity
      resolves nested qualified and root-qualified prefix operands        $qualified
      enforces the same source constructor arity                          $arity
      reports missing external effect dependencies rather than syntax     $externalEffects
      does not erase an unavailable effect constructor representation      $effectRepresentation
  """

  private def entries(code: String): List[MethodAnalysis.Entry] =
    MethodAnalysis.analyze(
      List(MethodAnalysis.Input("Infix.scala", dialects.Scala3(code).parse[Source].get))
    )

  private def count(definitions: String, result: String): Count =
    entries(s"$definitions\ndef pick[A](x: A, y: A): $result = ???")
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

  def qualified = {
    val definitions =
      "object Models { type Item[A] = A; type Join[A, B] = (A, B) }\n" +
        "type Join[A, B] = Models.Join[A, B]"
    val prefix = count(definitions, "_root_.Models.Join[Models.Item[A], scala.Option[A]]")
    (prefix === Count.Finite(6))
      .and(count(definitions, "Models.Item[A] Join scala.Option[A]") === prefix)
      .and(count(definitions, "Join[Models.Item[A], _root_.scala.Option[A]]") === prefix)
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
    pairs
      .map { (infix, prefix) =>
        val expected =
          if (infix.contains("Env")) "Env" else if (infix.contains("Var")) "Var" else "<"
        (count("", infix) === count("", prefix))
          .and(count("", infix) === Count.Unresolved(List(s"unresolved type: $expected")))
      }
      .reduce(_.and(_))
  }

  def effectRepresentation = {
    val definitions = "trait <[A, S]"
    val prefix = count(definitions, "<[A, Unit]")
    (prefix === Count.Unresolved(List("abstract type or method-valued representation: <")))
      .and(count(definitions, "A < Unit") === prefix)
  }

}
