import org.specs2.Specification

class SizeSpec extends Specification {

  def is = s2"""
    TinySize equality compares cardinality     ${(TinySize(4) !== TinySize(2)).and(
      TinySize(4) === TinySize(4)
    )}
    equal sizes hash equally                   ${ShortSize.hashCode === FiniteSize(16).hashCode}
    Byte has 256 inhabitants                   ${ByteSize === FiniteSize(8)}
    bits rounds up                             ${Size.bits(3) === 2}
    tiny arithmetic spills into bits           ${TinySize(100) + TinySize(100) === FiniteSize(8)}
    tiny powers don't saturate                 ${(BooleanSize ^ TinySize(100)) === FiniteSize(100)}
    equal finite sums terminate                ${IntSize + IntSize === FiniteSize(33)}
    sums keep the larger bit count             ${FloatSize + LongSize === FiniteSize(65)}
    Nothing absorbs products                   ${(NothingSize * IntSize === NothingSize).and(
      EffectiveOmega * NothingSize === NothingSize
    )}
    Omega times Tau is Tau                     ${EffectiveOmega * EffectiveTau === EffectiveTau}
    Omega is larger than finite                ${EffectiveOmega
      .larger(IntSize)
      .and(!IntSize.larger(EffectiveOmega))}
    Tau is larger than Omega                   ${EffectiveTau
      .larger(EffectiveOmega)
      .and(!EffectiveOmega.larger(EffectiveTau))}
    larger is antisymmetric for lossy sizes    ${FloatSize
      .larger(LongSize)
      .and(!LongSize.larger(FloatSize))}
    tiny larger is irreflexive                   ${!TinySize(4).larger(TinySize(4))}
    finite larger orders by bits                 ${IntSize
      .larger(ByteSize)
      .and(!ByteSize.larger(IntSize))
      .and(!IntSize.larger(IntSize))}
    finite is larger than tiny                   ${ByteSize.larger(BooleanSize)}
    lossy larger is strict and irreflexive       ${DoubleSize
      .larger(FloatSize)
      .and(!FloatSize.larger(DoubleSize))
      .and(!FloatSize.larger(FloatSize))}
    lossy is not larger than omega               ${!FloatSize.larger(EffectiveOmega)}
    lossy equality compares bits                 ${(FloatSize === FloatSize)
      .and(FloatSize !== DoubleSize)}
    tau larger is irreflexive                    ${!EffectiveTau.larger(EffectiveTau)}
    tiny prints its cardinality                  ${TinySize(7).toString === "TinySize(7)"}
    finite prints its bit width                  ${ByteSize.toString === "FiniteSize(8)"}
    tiny to a finite power terminates          ${(BooleanSize ^ IntSize) === FiniteSize(
      BigInt(1) << 32
    )}
    finite to an infinite power terminates     ${(IntSize ^ EffectiveOmega) === EffectiveOmega}
    Omega to the Tau terminates                ${(EffectiveOmega ^ EffectiveTau) === EffectiveTau}
    huge finite exponents become Omega         ${(IntSize ^ FiniteSize(
      BigInt(Int.MaxValue) + 1
    )) === EffectiveOmega}
    tiny times finite rounds up                ${TinySize(3) * IntSize === FiniteSize(34)}
  """

}
