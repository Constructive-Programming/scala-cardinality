import org.specs2.Specification

// The size algebra: `aε₀ + bω + n`, added componentwise and compared lexicographically, with
// deliberately coarse products and powers above the finite tier. `TinySize(n)` is the exact
// count `n`, `FiniteSize(bits)` a count in `(2^(bits-1), 2^bits]`, `Size.tiers(a, b)` a
// polynomial of whole tiers, and `EffectiveOmega` / `EffectiveEpsilon0` one tier each — so
// `EffectiveOmega + EffectiveOmega` is `2ω`.
class SizeSpec extends Specification {

  def is = s2"""
    TinySize equality compares cardinality     ${(TinySize(4) !== TinySize(2)).and(
      TinySize(4) === TinySize(4)
    )}
    equal sizes hash equally                   ${ShortSize.hashCode === FiniteSize(16).hashCode}
    Byte has 256 inhabitants                   ${ByteSize === FiniteSize(8)}
    bits rounds up                             ${Size.bits(3) === 2}
    tiny arithmetic spills into bits           ${TinySize(100) + TinySize(100) === FiniteSize(8)}
    tiny powers do not saturate                ${(BooleanSize ^ TinySize(100)) === FiniteSize(100)}
    equal finite sums terminate                ${IntSize + IntSize === FiniteSize(33)}
    sums keep the larger bit count             ${FloatSize + LongSize === FiniteSize(65)}
    Nothing absorbs products                   ${(NothingSize * IntSize === NothingSize).and(
      EffectiveOmega * NothingSize === NothingSize
    )}
    omega times epsilon-zero stays high        ${EffectiveOmega * EffectiveEpsilon0 === EffectiveEpsilon0}
    omega is larger than finite                ${EffectiveOmega
      .larger(IntSize)
      .and(!IntSize.larger(EffectiveOmega))}
    epsilon-zero is larger than omega          ${EffectiveEpsilon0
      .larger(EffectiveOmega)
      .and(!EffectiveOmega.larger(EffectiveEpsilon0))}
    larger is antisymmetric for lossy sizes    ${FloatSize
      .larger(LongSize)
      .and(!LongSize.larger(FloatSize))}
    tiny larger is irreflexive                 ${!TinySize(4).larger(TinySize(4))}
    finite larger orders by bits               ${IntSize
      .larger(ByteSize)
      .and(!ByteSize.larger(IntSize))
      .and(!IntSize.larger(IntSize))}
    finite is larger than tiny                 ${ByteSize.larger(BooleanSize)}
    lossy larger is strict and irreflexive     ${DoubleSize
      .larger(FloatSize)
      .and(!FloatSize.larger(DoubleSize))
      .and(!FloatSize.larger(FloatSize))}
    lossy is not larger than omega             ${!FloatSize.larger(EffectiveOmega)}
    lossy equality compares bits               ${(FloatSize === FloatSize)
      .and(FloatSize !== DoubleSize)}
    lossy never equals an exact width          ${(IntSize !== FloatSize)
      .and(ByteSize !== DoubleSize)}
    a tier is not larger than itself           ${!EffectiveEpsilon0.larger(EffectiveEpsilon0)}
    tiny prints its cardinality                ${TinySize(7).toString === "TinySize(7)"}
    finite prints its bit width                ${ByteSize.toString === "FiniteSize(8)"}
    tiny to a finite power terminates          ${(BooleanSize ^ IntSize) === FiniteSize(
      BigInt(1) << 32
    )}
    finite to an infinite power terminates     ${(IntSize ^ EffectiveOmega) === EffectiveOmega}
    omega to epsilon-zero terminates           ${(EffectiveOmega ^ EffectiveEpsilon0) === EffectiveEpsilon0}
    huge finite exponents become omega         ${(IntSize ^ FiniteSize(
      BigInt(Int.MaxValue) + 1
    )) === EffectiveOmega}
    tiny times finite rounds up                ${TinySize(3) * IntSize === FiniteSize(34)}

  Natural sums keep every contribution
    omega plus omega keeps its coefficient     ${(EffectiveOmega + EffectiveOmega) === Size.tiers(
      0,
      2
    )}
    sums of tiers commute                      ${(EffectiveEpsilon0 + EffectiveOmega + TinySize(
      3
    )) ===
      (TinySize(3) + EffectiveOmega + EffectiveEpsilon0)}
    zero is an identity for sums               ${(EffectiveOmega + NothingSize) === EffectiveOmega}
    a capacity sum absorbs zero                ${(FiniteSize(32) + NothingSize) === FiniteSize(32)}
    a lossy sum absorbs zero                   ${(FloatSize + NothingSize) === FloatSize}
    comparison reads epsilon-zero first        ${Size
      .tiers(2, 0)
      .larger(Size.tiers(1, 5))
      .and(Size.tiers(1, 5).larger(Size.tiers(1, 2) + TinySize(100)))}
    the finite part breaks a tie last          ${(Size.tiers(0, 2) + TinySize(3)).larger(
      Size.tiers(0, 2) + TinySize(2)
    )}
    ties are equal, not larger                 ${!Size.tiers(1, 1).larger(Size.tiers(1, 1))}
    min picks the smaller side                 ${(EffectiveEpsilon0.min(
      EffectiveOmega
    )) === EffectiveOmega}
    a unit coefficient is implicit             ${(EffectiveEpsilon0 + EffectiveOmega).toString === "ε₀ + ω"}
    a coefficient prints before its tier       ${(EffectiveOmega + EffectiveOmega).toString === "2ω"}
    every term of a polynomial prints          ${(Size
      .tiers(2, 1) + TinySize(3)).toString === "2ε₀ + ω + 3"}
    an exact finite part prints as digits      ${(EffectiveOmega + TinySize(
      3
    )).toString === "ω + 3"}
    a capacity prints as its marker            ${(EffectiveOmega + IntSize).toString === "ω + FiniteSize(32)"}
    a finite size keeps its own rendering      ${(TinySize(2) + TinySize(
      3
    )).toString === "TinySize(5)"}

  Products and powers stay coarse above the finite tier
    a finite factor keeps the tier             ${(TinySize(2) * EffectiveOmega) === EffectiveOmega}
    infinite times infinite stays in the tier  ${(EffectiveOmega * EffectiveOmega) === EffectiveOmega}
    a coarse product never demotes epsilon     ${(EffectiveEpsilon0 * EffectiveOmega) === EffectiveEpsilon0}
    zero annihilates any polynomial            ${(EffectiveEpsilon0 * NothingSize) === NothingSize}
    one preserves a whole polynomial           ${(Size.tiers(2, 1) * UnitSize) === Size.tiers(2, 1)}
    an infinite base keeps its tier            ${(EffectiveEpsilon0 ^ TinySize(
      3
    )) === EffectiveEpsilon0}
    a finite base over an infinite domain      ${(BooleanSize ^ EffectiveOmega) === EffectiveOmega}
    an infinite base over an infinite exponent ${(EffectiveOmega ^ EffectiveOmega) === EffectiveEpsilon0}
    a power of one preserves the polynomial    ${(Size.tiers(2, 0) ^ UnitSize) === Size.tiers(2, 0)}
    zero to the zeroeth is one                 ${(NothingSize ^ NothingSize) === UnitSize}
    zero over an infinite exponent stays empty ${(NothingSize ^ EffectiveOmega) === NothingSize}
    one over an infinite exponent stays one    ${(UnitSize ^ EffectiveOmega) === UnitSize}

  Widening is how the solver loses precision
    a bounded divergence widens to omega       ${TinySize(7).widen === EffectiveOmega}
    an empty estimate stays empty              ${NothingSize.widen === NothingSize}
    omega widens to a single omega             ${(EffectiveOmega + EffectiveOmega + TinySize(
      2
    )).widen === EffectiveOmega}
    a polynomial keeps its highest tier        ${(Size.tiers(3, 4) + TinySize(
      9
    )).widen === EffectiveEpsilon0}
  """

}
