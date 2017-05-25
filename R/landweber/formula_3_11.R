Formula311 <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  form_matrix_n_t_2 <- function(size_of_vector_t, size_of_vector_q, matrix_x_infinity, matrix_x, matrix_x_star) {
    Helpers$generate_matrix_by_function(
      row_size    = size_of_vector_q,
      column_size = size_of_vector_t,
      func        = function(i, j) {
        Functions$green_function_n(
          vector_x      = matrix_x_infinity[i, ],
          vector_y      = matrix_x[j, ],
          vector_y_star = matrix_x_star[j, ]
        )
      }
    )
  }

  form_vector_of_first_sums_from_u <- function(size_of_vector_t, size_of_vector_q, vector_mu, matrix_n_t_2) {
    sum_indices <- 1:size_of_vector_t

    Helpers$generate_vector_by_function(
      size = size_of_vector_q,
      func = function(i) {
        Helpers$reduce(
          initial = 0,
          vector  = sum_indices,
          func    = function(memo, j) {
            memo + vector_mu[j] * matrix_n_t_2[i, j]
          }
        )
      }
    )
  }

  form_matrix_h_2 <- function(size_of_vector_q, matrix_x_infinity, matrix_derivative_of_x_infinity) {
    Helpers$generate_square_matrix_by_function(
      size = size_of_vector_q,
      func = function(i, j) {
        if (i == j) {
          Functions$kernel_h_2_without_singularity(
            vector_derivative_of_x_infinity_j = matrix_derivative_of_x_infinity[j, ]
          )
        } else {
          Functions$kernel_h_2_with_singularity(
            vector_x_infinity_i = matrix_x_infinity[i, ],
            vector_x_infinity_j = matrix_x_infinity[j, ]
          )
        }
      }
    )
  }

  form_vector_of_second_sums_from_u <- function(size_of_vector_q, vector_f, matrix_h_2) {
    sum_indices <- 1:size_of_vector_q

    Helpers$generate_vector_by_function(
      size = size_of_vector_q,
      func = function(i) {
        Helpers$reduce(
          initial = 0,
          vector  = sum_indices,
          func    = function(memo, j) {
            memo + vector_f[j] * matrix_h_2[i, j]
          }
        )
      }
    )
  }

  form_vector_of_third_sums_from_u <- function(size_of_vector_q, vector_f, matrix_x_infinity, matrix_x_infinity_star) {
    sum_indices <- 1:size_of_vector_q

    Helpers$generate_vector_by_function(
      size = size_of_vector_q,
      func = function(i) {
        Helpers$reduce(
          initial = 0,
          vector  = sum_indices,
          func    = function(memo, j) {
            memo %>%
            add(vector_f[j]) %>%
            multiply_by(
              AdvancedMath$natural_logarithm(
                1 %>%
                divide_by(
                  AdvancedMath$modulus_of_vector(
                    matrix_x_infinity[i, ] - matrix_x_infinity_star[j, ]
                  )
                )
              )
            )
          }
        )
      }
    )
  }

  # TODO
  form_vector_u <- function(size_of_vector_q, capital_m, h_infinity, vector_of_first_sums_from_u,
                            vector_of_second_sums_from_u, vector_of_third_sums_from_u, alpha) {
    Helpers$generate_vector_by_function(
      size = size_of_vector_q,
      func = function(i) {
        (1 / (2 * capital_m)) %>%
        multiply_by(vector_of_first_sums_from_u[i]) %>%
        add(
          (h_infinity / 2 * AdvancedMath$PI) %>%
          multiply_by(vector_of_second_sums_from_u[i])
        ) %>%
        add(
          (h_infinity / 2 * AdvancedMath$PI) %>%
          multiply_by(vector_of_third_sums_from_u[i])
        ) %>%
        add(alpha)
      }
    )
  }
})
