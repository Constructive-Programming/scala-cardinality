// Temporary: drives the failure rows of the PR report (coverage below the floor,

// a CPD duplicate, CodeScene findings). Reverted in the following commit.
object ReportProbe {

  def classifyA(a: Int, b: Int, c: Int, d: Int): String =
    if (a > 0) {
      if (b > 0) {
        if (c > 0) {
          if (d > 0) "pppp" else if (d < -10) "pppn" else "pppz"
        } else if (c < -10) {
          if (d > 0) "ppnp" else "ppnn"
        } else "ppz"
      } else if (b < -10) {
        if (c > 0 && d > 0) "pnpp" else if (c < 0 || d < 0) "pnn" else "pnz"
      } else "pz"
    } else if (a < -10) {
      if (b > 0) { if (c > 0) "nppx" else "npnx" }
      else if (b < -10) { if (c > 0) "nnpx" else "nnnx" }
      else "nz"
    } else "z"

  def classifyB(a: Int, b: Int, c: Int, d: Int): String =
    if (a > 0) {
      if (b > 0) {
        if (c > 0) {
          if (d > 0) "pppp" else if (d < -10) "pppn" else "pppz"
        } else if (c < -10) {
          if (d > 0) "ppnp" else "ppnn"
        } else "ppz"
      } else if (b < -10) {
        if (c > 0 && d > 0) "pnpp" else if (c < 0 || d < 0) "pnn" else "pnz"
      } else "pz"
    } else if (a < -10) {
      if (b > 0) { if (c > 0) "nppx" else "npnx" }
      else if (b < -10) { if (c > 0) "nnpx" else "nnnx" }
      else "nz"
    } else "z"

}
