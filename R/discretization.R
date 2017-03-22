Discretization <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

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
  #     result <- c(result, node_t(i, m))
  #   }
  #
  #   result
  # }

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

  weight_function_r <- function(m, t, t_j) {
    indices <- 1:(m - 1)

    sum <- Reduce(
      function(memo, index) {
        memo + (1 / index) * cos(index * (t - t_j))
      },
      indices,
      0.0
    )

    1 %>%
    add(2 * sum) %>%
    add((1 / m) * cos(m * (t - t_j))) %>%
    multiply_by(-(1 / (2 * m)))
  }
})
