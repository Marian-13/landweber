Discretization <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  t <- function(capital_m) {
    Mesh$equidistant_mesh_nodes(capital_m)
  }

  # (3.9)
  sle <- function(t) {
    # TODO
  }
})
