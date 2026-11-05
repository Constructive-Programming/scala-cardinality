package cardinality.tasty

import java.nio.charset.StandardCharsets.UTF_8

import scala.util.control.NonFatal

/** The names a TASTy file's name table holds, read from its header alone (tasty-query 1.9.0 keeps
  * its unpickler private). A file declares classes of its package, so the package's qualified name
  * is in the table; a shaded jar relocates the file to another directory without rewriting it.
  */
private[tasty] object TastyNames {

  private val Utf8 = 1
  private val Qualified = 2

  /** Whether the TASTy declares `pkg` as its package; one that does not read is taken at its word.
    */
  def declares(tasty: Array[Byte], pkg: String): Boolean =
    pkg.isEmpty || (try names(tasty).contains(pkg)
    catch { case NonFatal(_) => true })

  private def names(bytes: Array[Byte]): Set[String] = {
    var at = 4 // magic
    // A natural number: seven bits a byte, most significant first, the last byte's top bit set.
    def nat(): Int = {
      var n = 0
      var b = 0
      while ({ b = bytes(at) & 0xff; at += 1; n = (n << 7) | (b & 0x7f); (b & 0x80) == 0 }) ()
      n
    }
    val _ = (nat(), nat(), nat()) // major, minor, experimental version
    val tooling = nat() // `at += nat()` would read `at` before `nat()` moves it
    at += tooling + 16 // the tooling version, then the UUID
    val end = { val length = nat(); at + length }
    val table = scala.collection.mutable.ArrayBuffer.empty[String]
    while (at < end) {
      val tag = bytes(at) & 0xff
      at += 1
      val length = nat()
      val next = at + length
      table += (tag match {
        case Utf8      => new String(bytes, at, length, UTF_8)
        case Qualified => val prefix = nat(); s"${table(prefix)}.${table(nat())}"
        case _         => ""
      })
      at = next
    }
    table.toSet
  }

}
