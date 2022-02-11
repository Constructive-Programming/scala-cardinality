import scala.meta._

object Counter {
  def source: Source => Size = {
    case Source(sts) => sts.foldLeft(NothingSize:Size)(_ + stat(_))
  }

  def stat: Stat => Size = {
    case d : Defn => defn(d)
  }

  def defn: Defn => Size = {
    case Defn.Class(_, _, _, c, _) => ctor(c)
    case _ : Defn.Val => UnitSize
  }

  def ctor: Ctor => Size = {
    case Ctor.Primary(_, _, paramss) => paramss.flatten.foldLeft(UnitSize:Size)(_ * param(_))
  }

  def param: Term.Param => Size = {
    case Term.Param(_, _, Some(t), _) => `type`(t)
  }

  def `type`: Type => Size = {
    case Type.Name("Boolean") => BooleanSize
    case Type.Name("Int") => IntSize
  }
}
