System <- module({
  use(.GlobalEnv, attach = TRUE)

  form_matrix_r <- function(capital_m, size_of_vector_t, vector_t) {
    Helpers$generate_square_matrix_by_function(
      size = size_of_vector_t,
      func = function(i, j) {
        Functions$weight_function_r(
          capital_m   = capital_m,
          element_t_i = vector_t[i],
          element_t_j = vector_t[j]
        )
      }
    )
  }

  form_matrix_h_1 <- function(size_of_vector_t, matrix_derivative_of_x, matrix_x, matrix_x_star) {
    Helpers$generate_square_matrix_by_function(
      size = size_of_vector_t,
      func = function(i, j) {
        Functions$kernel_h_1(
          vector_derivative_of_x_i = matrix_derivative_of_x[i, ],
          vector_x_i               = matrix_x[i, ],
          vector_x_star_j          = matrix_x_star[j, ]
        )
      }
    )
  }
})
