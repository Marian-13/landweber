DiscretizedMatrix <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  # matrix of sle (3.9)
  construct_matrix <- function(capital_m, size_of_vector_t, size_of_discretized_system, matrix_r, matrix_h_1) {
    .form_first_2_m_rows_elements(
      capital_m                  = capital_m,
      size_of_vector_t           = size_of_vector_t,
      size_of_discretized_system = size_of_discretized_system,
      matrix_r                   = matrix_r,
      matrix_h_1                 = matrix_h_1
    ) %>%
    c(.form_last_row_elements(size_of_vector_t)) %>%
    Helpers$generate_square_matrix_from_elements(size_of_discretized_system)
  }

  .form_first_2_m_rows_elements <- function(capital_m, size_of_vector_t, size_of_discretized_system, matrix_r, matrix_h_1) {
    elements <- Helpers$generate_vector(
      size = size_of_vector_t * size_of_discretized_system
    )

    indices <- 1:size_of_vector_t

    k <- 0

    for (i in indices) {
      for (j in indices) {
        k <- k + 1
        elements[k] <- -0.5 * matrix_r[i, j] + (1 / (2 * capital_m)) * matrix_h_1[i, j]
      }

      k <- k + 1
      elements[k] <- 1
    }

    elements
  }

  .form_last_row_elements <- function(size_of_vector_t) {
    Helpers$generate_vector_with_equal_elements(
      size = size_of_vector_t,
      element = 1
    ) %>%
    c(0)
  }
})
