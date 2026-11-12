package cardinality.tasty

import java.io.{ByteArrayInputStream, DataInputStream}

/** What a `.class` file says that tasty-query 1.9.0 does not model: which static fields are enum
  * constants (`ACC_ENUM`, in declaration order), whether the class is `sealed` (a
  * `PermittedSubclasses` attribute, Java 17), and for a class no symbol stands for (a local or an
  * anonymous one) the binary names of the classes it extends and the names of the methods an
  * instance of it declares.
  */
final private[tasty] case class JavaClassfile(
    enumConstants: List[String],
    sealedClass: Boolean,
    parents: List[String] = Nil,
    methods: List[String] = Nil
)

private[tasty] object JavaClassfile {

  private val AccEnum = 0x4000
  private val AccPrivate = 0x0002
  private val AccStatic = 0x0008

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
    val classes = scala.collection.mutable.Map.empty[Int, Int]
    var i = 1
    while (i < count) {
      in.readUnsignedByte() match {
        case 1                => utf8(i) = in.readUTF()
        case 5 | 6            => in.skipNBytes(8); i += 1 // a long or double takes two slots
        case 15               => in.skipNBytes(3)
        case 7                => classes(i) = in.readUnsignedShort()
        case 8 | 16 | 19 | 20 => in.skipNBytes(2)
        case _ => in.skipNBytes(4) // integer, float, member refs, name-and-type, dynamic
      }
      i += 1
    }
    in.skipNBytes(4) // access flags, this class
    // The super class, then the interfaces: each a class entry of the pool, by its binary name.
    val parents = (in.readUnsignedShort() :: List.fill(in.readUnsignedShort())(
      in.readUnsignedShort()
    )).flatMap(classes.get).flatMap(utf8.get)
    def attributes(): List[String] = List.fill(in.readUnsignedShort()) {
      val name = utf8.getOrElse(in.readUnsignedShort(), "")
      in.skipNBytes(in.readInt() & 0xffffffffL)
      name
    }
    // The fields, then the methods: each its flags and its name, the names `keep` keeps.
    def members(keep: (Int, String) => Boolean): List[String] = List
      .fill(in.readUnsignedShort()) {
        val flags = in.readUnsignedShort()
        val name = utf8.getOrElse(in.readUnsignedShort(), "")
        in.skipNBytes(2) // descriptor
        val _ = attributes()
        Option.when(keep(flags, name))(name)
      }
      .flatten
    val constants = members((flags, _) => (flags & AccEnum) != 0)
    // The methods an instance declares: neither private nor static, and no constructor.
    val methods =
      members((flags, name) => (flags & (AccPrivate | AccStatic)) == 0 && !name.startsWith("<"))
    JavaClassfile(
      constants,
      sealedClass = attributes().contains("PermittedSubclasses"),
      parents,
      methods
    )
  }

}
