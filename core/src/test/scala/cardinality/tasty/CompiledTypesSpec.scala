package cardinality.tasty

import scala.jdk.CollectionConverters.*

import java.nio.file.Files
import java.util.concurrent.{Callable, Executors}
import org.specs2.Specification

/** How `CompiledTypes` hands tasty-query a content-addressed jar with no suffix, as sbt 2 hands one
  * out: through a `.jar`-named link, made once per jar.
  */
class CompiledTypesSpec extends Specification {

  def is = s2"""
    A suffixless jar is linked once, however often and from however many threads it is asked $once
    A link that is gone is made again                                                          $remade
    A directory or a `.jar` is read where it is                                               $direct
  """

  private def suffixless() = {
    val jar = cardinality.Compiled.temporary("links").resolve("0123abcd")
    Files.write(jar, Array[Byte](1, 2, 3))
  }

  def once = {
    val jar = suffixless()
    val pool = Executors.newFixedThreadPool(8)
    val links =
      try
        pool
          .invokeAll(
            List.fill(32)((() => CompiledTypes.readable(jar)): Callable[java.nio.file.Path]).asJava
          )
          .asScala
          .map(_.get)
          .toSet
      finally pool.shutdown()
    (links.size === 1)
      .and(links.head.getFileName.toString === "0123abcd.jar")
      .and(Files.readAllBytes(links.head).toList === List[Byte](1, 2, 3))
      .and(CompiledTypes.readable(jar.getParent.resolve(".").resolve("0123abcd")) === links.head)
  }

  def remade = {
    val jar = suffixless()
    val first = CompiledTypes.readable(jar)
    Files.delete(first)
    val second = CompiledTypes.readable(jar)
    (second !== first).and(Files.exists(second) must beTrue)
  }

  def direct = {
    val dir = cardinality.Compiled.temporary("links")
    val jar = dir.resolve("named.jar")
    (CompiledTypes.readable(dir) === dir).and(CompiledTypes.readable(jar) === jar)
  }

}
