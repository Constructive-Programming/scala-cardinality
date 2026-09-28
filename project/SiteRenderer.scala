import cats.effect.{IO, Resource}
import cats.effect.unsafe.implicits.global
import laika.api.*
import laika.ast.Path.Root
import laika.format.*
import laika.helium.Helium
import laika.helium.config.{HeliumIcon, IconLink}
import laika.io.api.TreeTransformer
import laika.io.syntax.*
import laika.theme.ThemeProvider

import java.io.File

/** Renders the markdown under `docs/` into the static site, with the same engine the sister project
  * `eo` uses (Laika + its Helium theme). Laika runs inside the build JVM, so this is invoked from
  * the `siteRender` task rather than from a shell command.
  */
object SiteRenderer {

  private val repository = "https://github.com/constructive-programming/scala-cardinality"

  /** Reads `input` (the pages under `docs/`) and writes HTML to `output`. */
  def render(input: File, output: File): Unit =
    transformer
      .use(
        _.fromDirectory(input.getAbsolutePath)
          .toDirectory(output.getAbsolutePath)
          .transform
          .void
      )
      .unsafeRunSync()

  // `parallel` is what makes the directory-to-directory transform available; it is not a
  // concurrency knob we need until a site grows large enough to matter.
  private def transformer: Resource[IO, TreeTransformer[IO]] =
    Transformer
      .from(Markdown)
      .to(HTML)
      .using(Markdown.GitHubFlavor)
      .parallel[IO]
      .withTheme(theme)
      .build

  // Outside a `README.md` title document Helium has no home target of its own and fails the render,
  // so the landing page is named explicitly. The GitHub link mirrors the top nav of the sister
  // project's site.
  private def theme: ThemeProvider =
    Helium.defaults.all
      .metadata(
        title = Some("scala-cardinality"),
        description = Some("How many values can a Scala type hold?")
      )
      .site
      .topNavigationBar(
        homeLink = IconLink.internal(Root / "index.md", HeliumIcon.home),
        navLinks = Seq(IconLink.external(repository, HeliumIcon.github))
      )
      .build

}
