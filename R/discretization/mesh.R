Mesh <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  node_t <- function(i, m) {
    Parametrization$lower_limit_of_t() + (i * pi) / m
  }

  first_node <- function(m) {
    node_t(0, m)
  }

  last_node <- function(m) {
    node_t(2 * m - 1, m)
  }

  mesh_step <- function(m) {
    pi / m
  }

  equidistant_mesh_nodes <- function(m) {
    seq(from = first_node(m), to = last_node(m), by = mesh_step(m))
  }
})
