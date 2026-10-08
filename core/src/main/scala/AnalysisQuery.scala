package cardinality

import scala.meta.*

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest

import MethodAnalysis.{Entry, Input, Limits}

/** Selected method analysis over a complete supplied source set, without persistent cache reuse. */
object AnalysisQuery {

  final case class Budget(
      maxIndexWork: Long = 1000000,
      maxTargets: Int = 1000,
      maxWorkPerTarget: Long = 10000,
      maxRequestWork: Long = 1000000,
      maxTreeDepth: Int = 256
  ) {
    require(
      maxIndexWork > 0 && maxTargets > 0 && maxWorkPerTarget > 0 && maxRequestWork > 0 &&
        maxTreeDepth > 0
    )
  }

  final case class Request(
      inputs: List[Input],
      targetPaths: Set[String],
      targetNames: Set[String] = Set.empty,
      limits: Limits = Limits(),
      budget: Budget = Budget()
  )

  final case class Usage(path: String, name: String, quota: Long, consumed: Long)

  final case class Result(
      entries: List[Entry],
      key: String,
      counters: Map[String, Long],
      usage: List[Usage],
      errors: List[String]
  )

  def run(request: Request): Result = {
    require(request.limits.maxTypeDepth > 0 && request.limits.maxStates > 0)
    var key = ""
    val work = new AnalysisWork(request.budget.maxIndexWork, request.limits.maxTypeDepth)
    if (request.inputs.map(_.path).distinct.size != request.inputs.size)
      Result(Nil, key, Map.empty, Nil, List("duplicate source origin"))
    else if (!request.targetPaths.subsetOf(request.inputs.map(_.path).toSet))
      Result(Nil, key, Map.empty, Nil, List("selected source origin not supplied"))
    else
      try {
        inspectTrees(request, work)
        key = fingerprint(request)
        val index = new MethodAnalysis.Index(request.inputs, request.limits, Some(work))
        val targets = index.selected(request.targetPaths, request.targetNames)
        val missing = request.targetNames -- targets.map(_.name)
        if (missing.nonEmpty)
          Result(
            Nil,
            key,
            work.counters,
            Nil,
            List(s"selected targets not found: ${missing.toList.sorted.mkString(", ")}")
          )
        else if (targets.size > request.budget.maxTargets)
          Result(Nil, key, work.counters, Nil, List("selected target limit exceeded"))
        else {
          val quota = math.min(
            request.budget.maxWorkPerTarget,
            request.budget.maxRequestWork / math.max(1, targets.size)
          )
          val measured = targets.map { target =>
            work.target(quota)
            val entry = index.measure(target)
            entry -> Usage(entry.path, entry.name, quota, work.consumed)
          }
          Result(measured.map(_._1), key, work.counters, measured.map(_._2), Nil)
        }
      } catch {
        case error: AnalysisWork.Exhausted =>
          Result(Nil, key, work.counters, Nil, List(error.getMessage))
      }
  }

  private def inspectTrees(request: Request, work: AnalysisWork): Unit =
    request.inputs.sortBy(_.path).foreach { input =>
      var pending = List((input.tree: scala.meta.Tree) -> 0)
      while (pending.nonEmpty) {
        val (tree, depth) = pending.head
        pending = pending.tail
        work.step("index node")
        if (depth > request.budget.maxTreeDepth)
          throw AnalysisWork.Exhausted("syntax depth", input.path)
        pending = tree.children.map(_ -> (depth + 1)) ::: pending
      }
    }

  // Full-environment keys deliberately invalidate on every supporting-source edit. A later cache
  // can narrow this only after recording negative lookup and producer-domain dependencies.
  private def fingerprint(request: Request): String = {
    val hash = MessageDigest.getInstance("SHA-256")
    def field(value: String): Unit = {
      val bytes = value.getBytes(UTF_8)
      hash.update(java.nio.ByteBuffer.allocate(4).putInt(bytes.length).array())
      hash.update(bytes)
    }
    field("analysis-query-v1;scalameta-4.17.4;scala3;allocation-v1")
    field(request.inputs.size.toString)
    request.inputs.sortBy(_.path).foreach { input =>
      field(input.path)
      field(input.tree.pos.input.text)
      field(input.tree.structure)
    }
    field(request.targetPaths.size.toString)
    request.targetPaths.toList.sorted.foreach(path => { field("path"); field(path) })
    field(request.targetNames.size.toString)
    request.targetNames.toList.sorted.foreach(name => { field("name"); field(name) })
    field(request.limits.toString)
    field(request.budget.toString)
    hash.digest().map(byte => f"${byte & 0xff}%02x").mkString
  }

}
