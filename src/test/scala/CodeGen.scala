import org.scalacheck.Gen

/** Property-test inputs: generated Scala source strings paired with the size a correct counter must
  * report for them.
  *
  * A snippet carries two expectations. `exactCount` is the fragment's true cardinality, computed
  * with `BigInt` arithmetic that never touches the `Size` algebra; it grounds the product property
  * in real arithmetic. `expected` is the `Size` the documented semantics assign, built directly
  * from the primitive sizes rather than by re-parsing the string; it pins the behaviour of whole
  * programs, where the lossy `Size` representation rounds sums up.
  *
  * The generated types are the integral primitives, whose cardinalities are exact powers of two, so
  * a product is representable without rounding. Statements that introduce a single definition
  * (`val`, `def`, `object`) count as one value; a type-only or import statement contributes
  * nothing.
  */
object CodeGen {

  /** A generated source string together with what a correct counter must report for it. */
  final case class Snippet(source: String, exactCount: BigInt, expected: Size)

  /** The `Size` a correct counter must report for an exactly known cardinality. */
  def canonical(count: BigInt): Size =
    if count <= 127 then TinySize(count.toByte) else FiniteSize(Size.bits(count))

  /** Exactly one class. The count is then a pure product with no rounding from an enclosing sum, so
    * the reported size must match [[canonical]] exactly. This is the generator behind the exactness
    * property.
    */
  def genClassOnly: Gen[Snippet] = genClass.map(part => Snippet(part.source, part.count, part.size))

  /** A whole compilation unit: any mix of classes, definitions, type-only statements and packages.
    * The sum over statements rounds up, so the expected size is built directly from the parts.
    */
  def genProgram: Gen[Snippet] = Gen.listOf(genStat(2)).map { stats =>
    Snippet(
      stats.map(_.source).mkString("\n"),
      stats.foldLeft(BigInt(0))((acc, part) => acc + part.count),
      stats.foldLeft(NothingSize: Size)((acc, part) => acc + part.size)
    )
  }

  /** A generated fragment with both of its expectations. */
  final private case class Part(source: String, count: BigInt, size: Size)

  private def genClass: Gen[Part] = for
    keyword <- Gen.oneOf("class", "case class")
    name <- Gen.oneOf("A", "B", "C", "D", "E")
    params <- Gen.listOf(genType)
  yield
    val rendered = params.zipWithIndex.map { case ((tpe, _, _), i) => s"p$i: $tpe" }.mkString(", ")
    Part(
      s"$keyword $name($rendered)",
      params.foldLeft(BigInt(1)) { case (acc, (_, count, _)) => acc * count },
      params.foldLeft(UnitSize: Size) { case (acc, (_, _, size)) => acc * size }
    )

  /** A statement that introduces exactly one definition, hence one value. */
  private def genValue: Gen[Part] = Gen
    .oneOf("val a = 1", "val b: Int = 2", "object O", "def f = 1")
    .map(body => Part(body, BigInt(1), UnitSize))

  /** A statement that introduces no value of its own. */
  private def genNoValue: Gen[Part] = Gen
    .oneOf("import a.b", "trait T")
    .map(body => Part(body, BigInt(0), NothingSize))

  private def genStat(depth: Int): Gen[Part] =
    if depth <= 0 then Gen.frequency(5 -> genClass, 2 -> genValue, 1 -> genNoValue)
    else Gen.frequency(5 -> genClass, 2 -> genValue, 1 -> genNoValue, 1 -> genPackage(depth))

  private def genPackage(depth: Int): Gen[Part] = for
    name <- Gen.oneOf("foo", "bar", "baz.qux")
    body <- Gen.nonEmptyListOf(genStat(depth - 1))
  yield
    val rendered = body.map(part => indent(part.source)).mkString("\n")
    Part(
      s"package $name {\n$rendered\n}",
      body.foldLeft(BigInt(0))((acc, part) => acc + part.count),
      body.foldLeft(NothingSize: Size)((acc, part) => acc + part.size)
    )

  private def genType: Gen[(String, BigInt, Size)] = Gen.oneOf(
    ("Boolean", BigInt(2), BooleanSize),
    ("Byte", BigInt(256), ByteSize),
    ("Short", BigInt(65536), ShortSize),
    ("Char", BigInt(65536), CharSize),
    ("Int", BigInt(1) << 32, IntSize),
    ("Long", BigInt(1) << 64, LongSize),
  )

  private def indent(source: String): String =
    source.linesIterator.map(line => s"  $line").mkString("\n")

}
