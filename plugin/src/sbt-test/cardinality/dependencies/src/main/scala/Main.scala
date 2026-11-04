package example

import cats.kernel.Comparison

// `Comparison` is compiled into cats-kernel: the report reads its three cases from the jar's TASTy.
def pick(c: Comparison): Comparison = c
