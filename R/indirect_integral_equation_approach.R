IndirectIEApproach <- module({
  import("magrittr")

  use(.GlobalEnv, attach = true)

  # TODO normal name, test
  star <- function(y) {
    c(y[1], -y[2])
  }

  # (3.9)
  # TODO implement
  discretized_system <- function() {

  }

  # TODO implement
  # row <- function(j, m) {
  #
  # }
  #
  # TODO implement
  discretized_system_matrix_not_last_row <- function(i, m) {

  }

  # TODO implement
  discretized_system_vector_not_last_element <- function(i, m) {
    w_tilde()
  }

  # TODO normal name
  discretized_system_matrix_last_row <- function(m) {
    c(rep(1, 2 * m), 0)
  }

  # TODO normal name
  discretized_system_vector_last_element <- function() {
    0
  }

  # TODO Constant
  h_infinity <- function() {
    ExampleSpecificFunctions.c / sqrt(ExampleSpecificFunctions.m_1)
  }

  # TODO implement
  w_tilde(t_j, h_j) {
    # sum_indices = -ExampleSpecificFunctions.m_1:ExampleSpecificFunctions.m_1
    #
    # sum <-
    #   Reduce(
    #     function(memo, index) {
    #       product_of_i_and_h_infinity = i * h_infinity
    #       memo +
    #     }
    #   )
    #
    # h_j - h_infinity
  }


  # TODO IMPLEMENT (3.11)
  u <- function(mu, x, y, alpha) {
    # TODO remove stub
  }
})
