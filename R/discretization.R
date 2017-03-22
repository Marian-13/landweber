Discretization <- module({
  # m := M

  # old_node_t <- function(i, m) {
  #   # R's / operator always returns floating point number
  #   (i * pi) / m
  # }
  #
  # old_equidistant_mesh_nodes <- function(m) {
  #   indices <- 0:(2 * m - 1)
  #
  #   result <- vector(length = 0)
  #
  #   for (i in indices) {
  #     result <- c(result, NodeT(i, m))
  #   }
  #
  #   result
  # }

  node_t <- function(i, m) {
    lower_limit_of_t() + (i * pi) / m
  }

  first_node <- function(m) {
    node_t(0, m)
  }

  last_node <- function(m) {
    node_t(2 * m - 1, m)
  }

  step <- function(m) {
    pi / m
  }

  equidistant_mesh_nodes <- function(m) {
    seq(from = first_node(m), to = last_node(m), by = step(m))
  }
})
