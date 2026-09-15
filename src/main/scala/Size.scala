sealed trait Size { self =>
  def larger: Size => Boolean
  def add: Size => Size
  def mul: Size => Size
  def pow: Size => Size
  def exp: Size => Size = _.pow(self)
  def +(other: Size): Size = add(other)
  def *(other: Size): Size = mul(other)
  def ^(other: Size): Size = pow(other)

  def max(other: Size): Size = if (self.larger(other)) self else other
  def min(other: Size): Size = if (self.larger(other)) other else self
}

object Size {
  def bits(cardinality: BigInt): Int = (cardinality - 1).bitLength
}

sealed trait TinySize extends Size { self =>
  def repr: Byte

  def larger: Size => Boolean = {
    case t: TinySize => self.repr > t.repr
    case s           => !s.larger(self)
  }

  override def equals(obj: Any): Boolean = obj match {
    case t: TinySize => self.repr == t.repr
    case _           => false
  }

  override def hashCode(): Int = repr.hashCode
  override def toString() = s"TinySize($repr)"

  def add: Size => Size = {
    case t: TinySize => checkRepr(self.repr + t.repr)
    case s           => s.add(self)
  }

  def mul: Size => Size = {
    case t: TinySize => checkRepr(self.repr * t.repr)
    case s           => s.mul(self)
  }

  def pow: Size => Size = {
    case t: TinySize                        => checkRepr(BigInt(self.repr).pow(t.repr))
    case _ if self.repr <= 1                => self
    case f: FiniteSize if f.bits.isValidInt =>
      FiniteSize((BigInt(1) << f.bits.toInt) * Size.bits(BigInt(self.repr)))
    case _: FiniteSize => EffectiveOmega
    case _             => EffectiveTau
  }

  private def checkRepr(nrepr: BigInt): Size =
    if (nrepr.isValidByte) TinySize(nrepr.toByte)
    else FiniteSize(Size.bits(nrepr))

}

object TinySize {

  def apply(r: Byte) = new TinySize {
    val repr: Byte = r
  }

}

sealed trait FiniteSize extends Size { self =>
  def bits: BigInt

  def larger: Size => Boolean = {
    case _: TinySize          => true
    case _: LossyInfiniteSize => false
    case f: FiniteSize        => bits > f.bits
    case _                    => false
  }

  override def equals(obj: Any): Boolean = obj match {
    case _: LossyInfiniteSize => false
    case f: FiniteSize        => bits == f.bits
    case _                    => false
  }

  override def hashCode(): Int = bits.hashCode
  override def toString() = s"FiniteSize($bits)"

  def add: Size => Size = {
    case NothingSize   => self
    case t: TinySize   => FiniteSize(bits.max(Size.bits(BigInt(t.repr))) + 1)
    case f: FiniteSize => FiniteSize(bits.max(f.bits) + 1)
    case s             => s.add(self)
  }

  def mul: Size => Size = {
    case NothingSize   => NothingSize
    case t: TinySize   => FiniteSize(bits + Size.bits(BigInt(t.repr)))
    case f: FiniteSize => FiniteSize(bits + f.bits)
    case s             => s.mul(self)
  }

  def pow: Size => Size = {
    case t: TinySize                        => FiniteSize(bits * BigInt(t.repr))
    case f: FiniteSize if f.bits.isValidInt => FiniteSize(bits * (BigInt(1) << f.bits.toInt))
    case _: FiniteSize                      => EffectiveOmega
    case _                                  => EffectiveTau
  }

}

object FiniteSize {
  def apply(bs: BigInt): FiniteSize = new FiniteSize { val bits: BigInt = bs }
}

sealed trait LossyInfiniteSize extends FiniteSize { self =>

  override def larger: Size => Boolean = {
    case i: LossyInfiniteSize        => bits > i.bits
    case _: TinySize | _: FiniteSize => true
    case _                           => false
  }

  override def equals(obj: Any): Boolean = obj match {
    case i: LossyInfiniteSize => bits == i.bits
    case _                    => false
  }

}

object LossyInfiniteSize {
  def apply(bs: BigInt): LossyInfiniteSize = new LossyInfiniteSize { val bits: BigInt = bs }
}

case object NothingSize extends TinySize { val repr = 0 }
case object UnitSize extends TinySize { val repr = 1 }
case object BooleanSize extends TinySize { val repr = 2 }
case object ByteSize extends FiniteSize { val bits = 8 }
case object ShortSize extends FiniteSize { val bits = 16 }
case object CharSize extends FiniteSize { val bits = 16 }
case object IntSize extends FiniteSize { val bits = 32 }
case object LongSize extends FiniteSize { val bits = 64 }
case object FloatSize extends LossyInfiniteSize { val bits = 32 }
case object DoubleSize extends LossyInfiniteSize { val bits = 64 }

case object EffectiveOmega extends Size {

  def larger: Size => Boolean = {
    case _: TinySize | _: FiniteSize => true
    case _                           => false
  }

  def add: Size => Size = {
    case EffectiveTau => EffectiveTau
    case _            => EffectiveOmega
  }

  def mul: Size => Size = {
    case NothingSize  => NothingSize
    case EffectiveTau => EffectiveTau
    case _            => EffectiveOmega
  }

  def pow: Size => Size = {
    case NothingSize                 => UnitSize
    case _: TinySize | _: FiniteSize => EffectiveOmega
    case _                           => EffectiveTau
  }

}

case object EffectiveTau extends Size {

  def larger: Size => Boolean = {
    case EffectiveTau => false
    case _            => true
  }

  def add: Size => Size = _ => EffectiveTau

  def mul: Size => Size = {
    case NothingSize => NothingSize
    case _           => EffectiveTau
  }

  def pow: Size => Size = {
    case NothingSize => UnitSize
    case _           => EffectiveTau
  }

}
