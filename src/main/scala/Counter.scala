import scala.meta._

object Counter {
  def source: Source => Size = s => sum(s.stats)

  def stat: Stat => Size = {
    case p: Pkg => sum(p.body.stats)
    case d: Defn => defn(d)
    case _ => NothingSize
  }

  def defn: Defn => Size = {
    case c: Defn.Class => ctor(c.ctor)
    case _: Defn.Val => UnitSize
    case _ => NothingSize
  }

  def ctor: Ctor.Primary => Size =
    _.paramClauses.flatMap(_.values).foldLeft(UnitSize: Size)(_ * param(_))

  def param: Term.Param => Size = _.decltpe.fold(EffectiveOmega: Size)(`type`)

  def `type`: Type => Size = {
    case Type.Name("Boolean") => BooleanSize
    case Type.Name("Byte") => ByteSize
    case Type.Name("Short") => ShortSize
    case Type.Name("Char") => CharSize
    case Type.Name("Int") => IntSize
    case Type.Name("Long") => LongSize
    case Type.Name("Float") => FloatSize
    case Type.Name("Double") => DoubleSize
    // ponytail: String, List[_], user-defined types all count as effectively infinite; resolve sealed hierarchies when needed
    case _ => EffectiveOmega
  }

  private def sum(stats: Seq[Stat]): Size = stats.foldLeft(NothingSize: Size)(_ + stat(_))
}
