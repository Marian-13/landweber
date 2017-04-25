DiscretizedMatrix <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  form_vector_r <- function(capital_m, size_of_vector_t, vector_t) {
    Helpers$generate_vector_from_vector(
      vector = vector_t,
      size   = size_of_vector_t,
      func = function(element_t_i) {
        Functions$weight_function_r(
          capital_m   = capital_m,
          element_t_i = element_t_i,
          element_t_j = element_t_i
        )
      }
    )
  }

  form_vector_h_1 <- function(size_of_vector_t, matrix_x, matrix_derivative_of_x, matrix_x_star) {
    Helpers$generate_vector_by_function(
      size   = size_of_vector_t,
      func = function(i) {
        Functions$kernel_h_1(
          vector_x_i               = matrix_x[i, ],
          vector_derivative_of_x_i = matrix_derivative_of_x[i, ],
          vector_x_star_j          = matrix_x_star[i, ]
        )
      }
    )
  }

  form_matrix_n_1_2 <- function(capital_m, size_of_vector_t, vector_r, vector_h_1, matrix_x, matrix_x_star) {
    # TODO Optimize (replace if by triple loop)
    Helpers$generate_matrix_by_function(
      row_size    = size_of_vector_t,
      column_size = size_of_vector_t,
      func        = function(i, j) {
        if (i == j) {
          -0.5 * vector_r[i] + (1 / (2 * capital_m)) * vector_h_1[i]
        } else {
          Functions$green_function_n(
            vector_x      = matrix_x[i, ],
            vector_y      = matrix_x[j, ],
            vector_y_star = matrix_x_star[j, ]
          )
        }
      }
    )
  }

  # matrix of sle (3.9)
  form_discretized_matrix <- function(size_of_vector_t, size_of_discretized_system, matrix_n_1_2) {
    .form_first_2_m_rows_elements(
      size_of_vector_t           = size_of_vector_t,
      size_of_discretized_system = size_of_discretized_system,
      matrix_n_1_2               = matrix_n_1_2
    ) %>%
    c(.form_last_row_elements(size_of_vector_t)) %>%
    Helpers$generate_square_matrix_from_elements(size_of_discretized_system)
  }

  .form_first_2_m_rows_elements <- function(size_of_vector_t, size_of_discretized_system, matrix_n_1_2) {
    Helpers$generate_elements_from_square_matrix(
      square_matrix      = matrix_n_1_2,
      square_matrix_size = size_of_vector_t,
      elements_size      = size_of_vector_t * size_of_discretized_system
    )
  }

  .form_last_row_elements <- function(size_of_vector_t) {
    Helpers$generate_vector_with_equal_elements(
      size = size_of_vector_t,
      element = 1
    ) %>%
    c(0)
  }
})
