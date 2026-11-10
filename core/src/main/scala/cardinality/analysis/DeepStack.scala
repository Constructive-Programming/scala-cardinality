package cardinality.analysis

/** The stack a measurement runs on, whatever thread asked for it. The goal solver recurses once per
  * goal (up to the state budget when no depth limit applies) and each goal folds its shapes
  * recursively, so a deep target needs more than the JDK's 1 MiB default: a host JVM started
  * without `.jvmopts` (a Mill daemon, a bare launcher) would end the report with a
  * `StackOverflowError` instead of a budget refusal. Reserved, not committed: untouched pages cost
  * nothing.
  */
private[cardinality] object DeepStack {

  val Bytes: Long = 256L << 20

  /** `body` on a thread of its own with `Bytes` of stack; what it throws is thrown here. */
  def apply[A](body: => A): A = {
    @volatile var outcome: Option[Either[Throwable, A]] = None
    val thread = new Thread(
      null,
      () =>
        outcome = Some(
          try Right(body)
          catch { case error: Throwable => Left(error) }
        ),
      "cardinality-measurement",
      Bytes
    )
    thread.start()
    thread.join()
    outcome.get.fold(throw _, identity)
  }

}
