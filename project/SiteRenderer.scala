import cats.effect.{IO, Resource}
import cats.effect.unsafe.implicits.global
import laika.api.*
import laika.ast.Image
import laika.ast.Path.Root
import laika.format.*
import laika.helium.Helium
import laika.helium.config.{HeliumIcon, IconLink, ImageLink, TextLink}
import laika.io.api.TreeTransformer
import laika.io.syntax.*
import laika.theme.ThemeProvider
import laika.theme.config.Color

import java.io.File

/** Renders the markdown under `docs/site/` into the static site, with the same engine and brand
  * overlay the sister project `eo` uses (Laika + Helium). Laika runs inside the build JVM, so this
  * is invoked from the `siteRender` task rather than from a shell command.
  *
  * Internal notes (`docs/plans`, `docs/research`, `docs/baselines`) sit outside the input directory
  * and are therefore neither rendered nor reachable from the site.
  */
object SiteRenderer {

  private val repository = "https://github.com/constructive-programming/scala-cardinality"

  /** Reads `input` (the published pages under `docs/site/`) and writes HTML to `output`. */
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

  // The colours, fonts and toggle mirror eo.constructive.dev: Helium's stock red/blue is replaced
  // by the constructive.dev indigo palette, and the two static files under `docs/site/static/`
  // carry the font stacks and the manual light/dark switch. The home link is a text link because
  // Helium's default home icon expects a landing page it can discover on its own.
  private def theme: ThemeProvider =
    Helium.defaults.all
      .metadata(
        title = Some("cardinality"),
        description = Some("Build development cycles that reduce complexity.")
      )
      .all
      .themeColors(
        primary = Color.hex("3b3b8c"),
        primaryMedium = Color.hex("8585c5"),
        primaryLight = Color.hex("eaeaf5"),
        secondary = Color.hex("5e5eb8"),
        text = Color.hex("23272e"),
        background = Color.hex("ffffff"),
        bgGradient = (Color.hex("ffffff"), Color.hex("ffffff"))
      )
      .site
      .darkMode
      .themeColors(
        primary = Color.hex("7878d6"),
        primaryMedium = Color.hex("5e5eb8"),
        primaryLight = Color.hex("2d333b"),
        secondary = Color.hex("a3a3e6"),
        text = Color.hex("e6edf3"),
        background = Color.hex("1f242b"),
        bgGradient = (Color.hex("1f242b"), Color.hex("23272e"))
      )
      .site
      .topNavigationBar(
        homeLink = TextLink.internal(Root / "index.md", "cardinality"),
        navLinks = Seq(
          ImageLink.external(
            "https://www.constructive.dev/",
            Image.external("https://www.constructive.dev/assets/img/favicon.svg")
          ),
          IconLink.external(repository, HeliumIcon.github)
        )
      )
      .site
      .internalCSS(Root / "static" / "cp-theme.css")
      .site
      .internalJS(Root / "static" / "cp-theme-toggle.js")
      .build

}
