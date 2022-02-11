sealed trait Size { self =>
  def larger: Size => Boolean
  def add: Size => Size
  def mul: Size => Size
  def pow: Size => Size
  def exp: Size => Size = _.pow(self)
  def +(other: Size): Size = add(other)
  def *(other: Size): Size = mul(other)
  def ^(other: Size): Size = pow(other)
}

object Size {
  def bits(cardinality: Long): Int =
    (Math.log10(cardinality) / Math.log10(2)).toInt
}

sealed trait TinySize extends Size { self =>
  def repr: Byte
  def larger: Size => Boolean = {
    case t: TinySize => self.repr > t.repr
    case s => !s.larger(self)
  }
  override def equals(obj: Any): Boolean = obj match {
    case t: TinySize => t.repr == t.repr
    case _ => false
  }
  override def toString() = s"TinySize($repr)"
  def add: Size => Size = {
    case t: TinySize => checkRepr(t, self.repr.toInt + t.repr)
    case s => s.add(self)
  }
  def mul: Size => Size = {
    case t: TinySize => checkRepr(t, self.repr.toInt * t.repr)
    case s => s.mul(self)
  }
  def pow: Size => Size = {
    case t: TinySize => checkRepr(t, Math.pow(self.repr, t.repr).toInt)
    case s => s.exp(self)
  }

  private def checkRepr(t: TinySize, nrepr: Int) = {
    if (nrepr < 256) TinySize(nrepr.toByte)
    else FiniteSize(Size.bits(nrepr))
  }

}

object TinySize {
  def apply(r: Byte) = new TinySize {
    val repr: Byte = r
  }
}

sealed trait FiniteSize extends Size { self =>
  def bits: BigInt
  def larger: Size => Boolean = {
    case _: TinySize => true
    case f: FiniteSize => bits > f.bits
    case _ => false
  }
  override def equals(obj: Any): Boolean = obj match {
    case _: LossyInfiniteSize => false
    case f: FiniteSize => bits == f.bits
    case _ => false
  }
  override def toString() = s"FiniteSize($bits)"
  def add: Size => Size = {
    case NothingSize => self
    case s if larger(s) => FiniteSize(bits + 1)
    case s => s.add(self)
  }
  def mul: Size => Size = {
    case t: TinySize => FiniteSize(bits + Size.bits(t.repr))
    case f: FiniteSize => FiniteSize(bits + f.bits)
    case s => s.mul(self)
  }
  def pow: Size => Size = {
    case t: TinySize => FiniteSize(bits * t.repr)
    case f: FiniteSize if f.bits.isValidInt =>
        FiniteSize (bits * (BigInt(1) << f.bits.toInt) )
    case f: FiniteSize =>
      if (bits.isValidInt) FiniteSize (f.bits * (BigInt(1) << bits.toInt) )
      else EffectiveOmega
    case s => s.exp(self)
  }
}

object FiniteSize {
  def apply(bs: BigInt): FiniteSize = new FiniteSize { val bits: BigInt = bs }
}

sealed trait LossyInfiniteSize extends FiniteSize { self =>
  override def larger: Size => Boolean = {
    case i: LossyInfiniteSize => bits > i.bits
    case _: TinySize | _: FiniteSize => true
    case _ => false
  }
  override def equals(obj: Any): Boolean = obj match {
    case i: LossyInfiniteSize => bits == i.bits
    case _ => false
  }
}

object LossyInfiniteSize {
  def apply(bs: BigInt): LossyInfiniteSize = new LossyInfiniteSize { val bits: BigInt = bs }
}

case object NothingSize extends TinySize { val repr = 0 }
case object UnitSize extends TinySize { val repr = 1 }
case object BooleanSize extends TinySize { val repr = 2 }
case object ByteSize extends TinySize { val repr = 8 }
case object ShortSize extends FiniteSize { val bits = 16 }
case object CharSize extends FiniteSize { val bits = 16 }
case object IntSize extends FiniteSize { val bits = 32 }
case object LongSize extends FiniteSize { val bits = 64 }
case object FloatSize extends LossyInfiniteSize { val bits = 32 }
case object DoubleSize extends LossyInfiniteSize { val bits = 64 }

case object EffectiveOmega extends Size {
  def larger: Size => Boolean = {
    case _: TinySize | _: FiniteSize | EffectiveOmega => false
    case _ => true
  }
  def add: Size => Size = {
    case _: TinySize | _: FiniteSize | EffectiveOmega => EffectiveOmega
    case s => s.add(EffectiveOmega)
  }
  def mul: Size => Size = {
    case _: TinySize | _: FiniteSize | EffectiveOmega => EffectiveOmega
    case s => s.add(EffectiveOmega)
  }
  override def pow: Size => Size = {
    case _: TinySize | _: FiniteSize => EffectiveOmega
    case EffectiveOmega => EffectiveTau
    case s => s.exp(EffectiveOmega)
  }
}

case object EffectiveTau extends Size {
  def larger: Size => Boolean = {
    case _: TinySize | _: FiniteSize | EffectiveOmega | EffectiveTau => false
    case _ => true
  }
  def add: Size => Size = {
    case _: TinySize | _: FiniteSize | EffectiveOmega | EffectiveTau => EffectiveTau
    case s => s.add(EffectiveTau)
  }
  def mul: Size => Size = {
    case _: TinySize | _: FiniteSize | EffectiveOmega | EffectiveTau => EffectiveTau
    case s => s.add(EffectiveTau)
  }
  override def pow: Size => Size = {
    case _: TinySize | _: FiniteSize | EffectiveOmega | EffectiveTau => EffectiveTau
    case s => s.exp(EffectiveTau)
  }
}
