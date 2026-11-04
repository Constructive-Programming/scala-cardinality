package cardinality.analysis.inhabitation

/** Spellings the specs use for exact naturals: powers of plain integers, and a literal's value. */
object NaturalSyntax {

  def pow(base: BigInt, exponent: BigInt): Natural = Natural(base) ^ Natural(exponent)

  def pow(base: BigInt, exponent: Natural): Natural = Natural(base) ^ exponent

  extension (a: Natural) {

    def toBigInt: Option[BigInt] = a match {
      case Natural.Lit(value) => Some(value)
      case _                  => None
    }

  }

}
