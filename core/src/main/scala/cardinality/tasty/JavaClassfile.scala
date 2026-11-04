package cardinality.tasty

import java.io.{ByteArrayInputStream, DataInputStream}

/** What a Java `.class` file says that tasty-query 1.9.0 does not model: which static fields are
  * enum constants (`ACC_ENUM`, in declaration order) and whether the class is `sealed` (a
  * `PermittedSubclasses` attribute, Java 17).
  */
final private[tasty] case class JavaClassfile(enumConstants: List[String], sealedClass: Boolean)

private[tasty] object JavaClassfile {

  private val AccEnum = 0x4000

  /** The class file's facts; one that does not read is sealed (closed to a reading of its values)
    * with no constants, so a reading through it refuses instead of counting.
    */
  def read(bytes: Array[Byte]): JavaClassfile =
    try parse(new DataInputStream(new ByteArrayInputStream(bytes)))
    catch { case _: java.io.IOException => JavaClassfile(Nil, sealedClass = true) }

  private def parse(in: DataInputStream): JavaClassfile = {
    in.skipNBytes(8) // magic, minor and major version
    val count = in.readUnsignedShort()
    val utf8 = scala.collection.mutable.Map.empty[Int, String]
    var i = 1
    while (i < count) {
      in.readUnsignedByte() match {
        case 1                    => utf8(i) = in.readUTF()
        case 5 | 6                => in.skipNBytes(8); i += 1 // a long or double takes two slots
        case 15                   => in.skipNBytes(3)
        case 7 | 8 | 16 | 19 | 20 => in.skipNBytes(2)
        case _ => in.skipNBytes(4) // integer, float, member refs, name-and-type, dynamic
      }
      i += 1
    }
    in.skipNBytes(6) // access flags, this class, super class
    in.skipNBytes(2L * in.readUnsignedShort()) // interfaces
    def attributes(): List[String] = List.fill(in.readUnsignedShort()) {
      val name = utf8.getOrElse(in.readUnsignedShort(), "")
      in.skipNBytes(in.readInt() & 0xffffffffL)
      name
    }
    val constants = List
      .fill(in.readUnsignedShort()) {
        val flags = in.readUnsignedShort()
        val name = utf8.getOrElse(in.readUnsignedShort(), "")
        in.skipNBytes(2) // descriptor
        val _ = attributes()
        Option.when((flags & AccEnum) != 0)(name)
      }
      .flatten
    val _ = List.fill(in.readUnsignedShort()) { in.skipNBytes(6); attributes() } // methods
    JavaClassfile(constants, sealedClass = attributes().contains("PermittedSubclasses"))
  }

}
