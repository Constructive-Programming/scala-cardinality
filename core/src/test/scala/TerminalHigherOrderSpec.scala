package cardinality

import scala.meta.*

import org.specs2.Specification

class TerminalHigherOrderSpec extends Specification {
  import Inhabitation.{Binding, Count, Shape}
  import Shape.*

  private val a = Atom("A")
  private val b = Atom("B")
  private val s = Atom("S")
  private val terminalAtom = Atom("T")
  private val r = Atom("R")
  private val argument = Function(List(a), b)
  private val modify = Function(List(argument), Function(List(s), terminalAtom))

  def is = s2"""
    Terminal higher-order application
      synthesizes a constant functional argument for replacement           $replacement
      forwards a functional binder modulo eta equivalence                 $forwarding
      constructs an identity functional argument                           $identityArgument
      proves an empty functional argument space                            $emptyArgument
      distinguishes independent constant argument choices                  $constantChoices
      distinguishes independent higher-order heads                         $headChoices
      preserves provenance aliases                                         $aliases
      retains first-order productive argument families                     $productiveArgument
      accepts data products and ordinary currying in arguments             $dataArguments
      distinguishes projections of functional product parameters          $parameterProjections
      distinguishes projections of first-order producer results            $resultProjections
      preserves projected higher-order head provenance                      $projectedHeads
      combines independent functional arguments                             $multipleArguments
      multiplies environment sum branches                                  $sumBranches
      normalizes Unit functional domains                                    $unitArgument
      retains unique absurd elimination                                    $bottomContext
      rejects a callable product containing an unsupported function         $mixedResult
      retains a direct terminal value when the application is unavailable  $directValue
      rejects functional-codomain feedback                                 $feedback
      rejects ordinary consumers of higher-order results                   $consumer
      rejects feedback into ordinary curried parameters                    $parameterFeedback
      checks all heads for cross-head feedback                              $crossHead
      rejects nested functional parameters                                $nested
      retains unsupported capability obligations                           $unknown
      leaves opaque sum interactions unresolved                            $opaqueSum
      rejects functions inside product parameters                          $productArgument
      rejects higher-order product results                                 $productResult
      keeps Unit's unique observational inhabitant                          $unitResult
      counts real-style replacement and constructor source rows            $sourceRows
      executes compiled replacement and forwarding witnesses               $compiled
  """

  private def count(environment: List[Shape], target: Shape): Count =
    Inhabitation.count(
      environment.zipWithIndex.map((shape, index) => Binding(index.toString, shape)),
      target
    )

  def replacement =
    count(List(modify, b), Function(List(s), terminalAtom)) === Count.Finite(1)

  def forwarding =
    count(List(modify), modify) === Count.Finite(1)

  def identityArgument =
    count(List(Function(List(Function(List(a), a)), terminalAtom)), terminalAtom) === Count.Finite(
      1
    )

  def emptyArgument =
    count(List(modify), Function(List(s), terminalAtom)) === Count.Finite(0)

  def constantChoices =
    count(List(modify, b, b), Function(List(s), terminalAtom)) === Count.Finite(2)

  def headChoices =
    count(List(modify, modify, b), Function(List(s), terminalAtom)) === Count.Finite(2)

  def aliases = {
    val binding = Binding("same", modify)
    Inhabitation.count(List(binding, binding), modify) === Count.Finite(1)
  }

  def productiveArgument =
    count(
      List(modify, b, Function(List(b), b)),
      Function(List(s), terminalAtom)
    ) === Count.Countable

  def dataArguments = {
    val product = Function(List(Function(List(Product(List(a, a))), b)), terminalAtom)
    val curried = Function(List(Function(List(a), Function(List(s), b))), terminalAtom)
    (count(List(product, b), terminalAtom) === Count.Finite(1))
      .and(count(List(curried, b), terminalAtom) === Count.Finite(1))
  }

  def parameterProjections =
    count(
      List(Function(List(Function(List(Product(List(a, a))), a)), terminalAtom)),
      terminalAtom
    ) === Count.Finite(2)

  def resultProjections =
    count(
      List(Function(List(argument), terminalAtom), Function(List(a), Product(List(b, b)))),
      terminalAtom
    ) === Count.Finite(2)

  def projectedHeads = {
    val head = Function(List(argument), terminalAtom)
    count(List(Product(List(head, head)), b), terminalAtom) === Count.Finite(2)
  }

  def multipleArguments =
    count(List(Function(List(argument, argument), terminalAtom), b, b), terminalAtom) ===
      Count.Finite(4)

  def sumBranches =
    count(
      List(Function(List(argument), terminalAtom), Sum(List(Product(List(b, b)), b))),
      terminalAtom
    ) === Count.Finite(2)

  def unitArgument =
    count(
      List(Function(List(Function(List(Product(Nil)), b)), terminalAtom), b),
      terminalAtom
    ) === Count.Finite(1)

  def bottomContext =
    count(List(modify, Sum(Nil)), terminalAtom) === Count.Finite(1)

  def mixedResult =
    count(
      List(
        Function(List(argument), terminalAtom),
        Function(List(a), Product(List(b, argument)))
      ),
      terminalAtom
    ) must beAnInstanceOf[Count.Unresolved]

  def directValue =
    count(List(modify, terminalAtom), terminalAtom) === Count.Finite(1)

  def feedback =
    count(List(Function(List(argument), b), b), b) must beLike {
      case Count.Unresolved(reasons) =>
        reasons.contains("Higher-order application is unsupported") must beTrue
    }

  def consumer =
    count(List(modify, b, s, Function(List(terminalAtom), b)), terminalAtom) must
      beAnInstanceOf[Count.Unresolved]

  def parameterFeedback =
    count(List(modify, b, s, Function(List(terminalAtom), s)), terminalAtom) must
      beAnInstanceOf[Count.Unresolved]

  def crossHead =
    count(List(modify, b, s, Function(List(Function(List(a), terminalAtom)), r)), terminalAtom) must
      beAnInstanceOf[Count.Unresolved]

  def nested =
    count(List(Function(List(Function(List(argument), b)), terminalAtom)), terminalAtom) must
      beAnInstanceOf[Count.Unresolved]

  def unknown =
    count(List(modify, b, s, Repeated(a)), terminalAtom) must
      beAnInstanceOf[Count.Unresolved]

  def opaqueSum =
    count(List(modify, b, s, Function(List(s), Sum(List(b, r)))), terminalAtom) must
      beAnInstanceOf[Count.Unresolved]

  def productArgument =
    count(List(Function(List(Product(List(argument))), terminalAtom)), terminalAtom) must
      beAnInstanceOf[Count.Unresolved]

  def productResult =
    count(
      List(Function(List(argument), Product(List(terminalAtom, terminalAtom))), b),
      terminalAtom
    ) must
      beAnInstanceOf[Count.Unresolved]

  def unitResult =
    count(List(Function(List(argument), b)), Product(Nil)) === Count.Finite(1)

  def sourceRows = {
    val code = """trait CanModifyP[S, T, A, B] {
      def modify(f: A => B): S => T
      def replace(b: B): S => T = modify(_ => b)
    }
    final class Modify[S, T, A, B](val modifyFn: (A => B) => S => T)
    """
    val input = MethodAnalysis.Input("Higher.scala", dialects.Scala3(code).parse[Source].get)
    val entries = MethodAnalysis.analyze(List(input))
    (entries.find(_.name == "CanModifyP.replace").get.count === Count.Finite(1))
      .and(entries.find(_.name == "Modify.<init>").get.count === Count.Finite(1))
  }

  def compiled = {
    import terminalexamples.{CanModifyP, Modify}
    val receiver = new CanModifyP[Int, String, Boolean, String] {
      def modify(f: Boolean => String): Int => String = n => s"$n:${f(true)}"
    }
    val original: (Boolean => String) => Int => String = receiver.modify
    val stored = new Modify(original)
    (receiver.replace("constant")(7) === "7:constant")
      .and(stored.modifyFn(flag => if (flag) "yes" else "no")(3) === "3:yes")
  }

}
