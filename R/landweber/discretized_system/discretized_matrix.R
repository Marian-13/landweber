DiscretizedMatrix <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  # matrix of sle (3.9)
  construct_matrix <- function(capital_m, vectors, matrices, sizes) {
    .form_first_2_m_rows_elements(
      capital_m = capital_m,
      vectors   = vectors,
      matrices  = matrices,
      sizes     = sizes
    ) %>%
    c(.form_last_row_elements(sizes$t)) %>%
    Helpers$generate_square_matrix(sizes$discretized_system)
  }

  .form_first_2_m_rows_elements <- function(capital_m, vectors, matrices, sizes) {
    indices <- 1:sizes$t

    elements <- Helpers$generate_vector(sizes$discretized_system)

    index <- 0

    for (i in indices) {
      for (j in indices) {
        index <- index + 1

        coeficient_first_addend <- .calculate_coeficient_first_addend(
          capital_m   = capital_m,
          element_t_i = vectors$t[i],
          element_t_j = vectors$t[j]
        )

        coeficient_second_addend <- .calculate_coeficient_second_addend(
          vector_derivative_of_x_i = matrices$derivative_of_x[i, ],
          vector_x_i               = matrices$x[i, ],
          vector_x_star_j          = matrices$x_star[j, ]
        )

        elements[index] <- coeficient_first_addend + coeficient_second_addend

      }

      index <- index + 1

      elements[index] <- 1
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

  .calculate_coeficient_first_addend <- function(capital_m, element_t_i, element_t_j) {
    -0.5 * Functions$weight_function_r(capital_m, element_t_i, element_t_j)
  }

  .calculate_coeficient_second_addend <- function(vector_derivative_of_x_i, vector_x_i, vector_x_star_j) {
    0.25 * Functions$kernel_h_1(
      vector_derivative_of_x_i = vector_derivative_of_x_i,
      vector_x_i               = vector_x_i,
      vector_x_star_j          = vector_x_star_j
    )
  }
})
