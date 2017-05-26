Formula310 <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  form_matrix_h_3 <- function(size_of_vector_t, matrix_x, matrix_x_star, matrix_derivative_of_x,
                              matrix_second_derivative_of_x) {
    Helpers$generate_square_matrix_by_function(
      size = size_of_vector_t,
      func = function(i, j) {
        if (i == j) {
          Functions$kernel_k_without_singularity(
            vector_derivative_of_x_i        = matrix_derivative_of_x[i, ],
            vector_second_derivative_of_x_i = matrix_second_derivative_of_x[i, ]
          ) %>%
          add(
            Functions$kernel_k_with_singularity( # L(t, tau)
              vector_x_i               = matrix_x[i, ],
              vector_x_j               = matrix_x_star[j, ],
              vector_derivative_of_x_i = matrix_derivative_of_x[i, ]
            )
          )
        } else {
          Functions$kernel_k_with_singularity(
            vector_x_i               = matrix_x[i, ],
            vector_x_j               = matrix_x[j, ],
            vector_derivative_of_x_i = matrix_derivative_of_x[i, ]
          ) %>%
          add(
            Functions$kernel_k_with_singularity(
              vector_x_i               = matrix_x[i, ],
              vector_x_j               = matrix_x_star[j, ],
              vector_derivative_of_x_i = matrix_derivative_of_x[i, ]
            )
          )
        }
      }
    )
  }

  form_vector_of_first_sums_from_derivative_of_v <- function(size_of_vector_t, vector_mu, matrix_h_3) {
    sum_indices <- 1:size_of_vector_t

    Helpers$generate_vector_by_function(
      size = size_of_vector_t,
      func = function(i) {
        Helpers$reduce(
          initial = 0,
          vector  = sum_indices,
          func    = function(sum, j) {
            sum + vector_mu[j] * matrix_h_3[i, j]
          }
        )
      }
    )
  }

  form_matrix_derivative_of_n_1_q <- function(size_of_vector_t, size_of_vector_q, matrix_x, matrix_derivative_of_x,
                                              matrix_x_infinity, matrix_x_infinity_star,
                                              matrix_derivative_of_x_infinity) {
    Helpers$generate_matrix_by_function(
      row_size    = size_of_vector_t,
      column_size = size_of_vector_q,
      func        = function(i, j) {
        Functions$kernel_k_with_singularity(
          vector_x_i               = matrix_x[i, ],
          vector_x_j               = matrix_x_infinity[j, ],
          vector_derivative_of_x_i = matrix_derivative_of_x[i, ]
        ) %>%
        add(
          Functions$kernel_k_with_singularity(
            vector_x_i               = matrix_x[i, ],
            vector_x_j               = matrix_x_infinity_star[j, ],
            vector_derivative_of_x_i = matrix_derivative_of_x[i, ]
          )
        )
      }
    )
  }

  form_vector_of_second_sums_from_derivative_of_v <- function(size_of_vector_t, size_of_vector_q,
                                                              vector_f, matrix_derivative_of_n_1_q) {
    sum_indices <- 1:size_of_vector_q

    Helpers$generate_vector_by_function(
      size = size_of_vector_t,
      func = function(i) {
        Helpers$reduce(
          initial = 0,
          vector  = sum_indices,
          func    = function(sum, j) {
            sum + vector_f[j] * matrix_derivative_of_n_1_q[i, j]
          }
        )
      }
    )
  }

  form_vector_derivative_of_v <- function(capital_m, size_of_vector_t, h_infinity,
                                          vector_of_first_sums_from_derivative_of_v,
                                          vector_of_second_sums_from_derivative_of_v,
                                          matrix_derivative_of_x, vector_mu) {
    Helpers$generate_vector_by_function(
      size = size_of_vector_t,
      func = function(i) {
        -0.5 %>%
        multiply_by(
          vector_mu[i] %>%
          divide_by(AdvancedMath$modulus_of_vector(matrix_derivative_of_x[i, ]))
        ) %>%
        add(
          vector_of_first_sums_from_derivative_of_v[i] / (2 * capital_m)
        ) %>%
        add(
          h_infinity %>%
          # TODO h_infinity or (h_infinity / (2 * pi))
          divide_by(2 * AdvancedMath$PI) %>%
          multiply_by(vector_of_second_sums_from_derivative_of_v[i])
        )
      }
    )
  }
})
