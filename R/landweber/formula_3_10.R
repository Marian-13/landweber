Formula310 <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  form_matrix_h_3 <- function(size_of_vector_t, matrix_x) {
    Helpers$generate_square_matrix_by_function(
      size = size_of_vector_t,
      func = function(i, j) {
        if (i == j) {
          Functions$kernel_h_3_without_singularity()
        } else {
          Functions$kernel_h_3_with_singularity(
            vector_x_i = matrix_x[i, ],
            vector_x_j = matrix_x[j, ]
          )
        }
      }
    )
  }

  form_matrix_h_4 <- function(size_of_vector_t, matrix_x, matrix_derivative_of_x, matrix_second_derivative_of_x) {
    Helpers$generate_square_matrix_by_function(
      size = size_of_vector_t,
      func = function(i, j) {
        if (i == j) {
          Functions$kernel_h_4_without_singularity(
            vector_second_derivative_of_x_i = matrix_second_derivative_of_x[i, ],
            vector_derivative_of_x_i        = matrix_derivative_of_x[i, ]
          )
        } else {
          Functions$kernel_h_4_with_singularity(
            vector_derivative_of_x_j = matrix_derivative_of_x[j, ],
            vector_x_i               = matrix_x[i, ],
            vector_x_j               = matrix_x[j, ]
          )
        }
      }
    )
  }

  form_vector_of_first_sums_from_derivative_of_v <- function(size_of_vector_t, vector_mu, matrix_h_3, matrix_h_4,
                                                             matrix_derivative_of_x, matrix_x, matrix_x_star) {
    sum_indices <- 1:size_of_vector_t

    Helpers$generate_vector_by_function(
      size = size_of_vector_t,
      func = function(i) {
        Helpers$reduce(
          initial = 0,
          vector  = sum_indices,
          func    = function(memo, j) {
            memo %>%
            add(
              vector_mu[j] %>%
              multiply_by(
                -1 %>%
                multiply_by(
                  matrix_derivative_of_x[i, 2] %>%
                  multiply_by(
                    matrix_h_3[i, j] %>%
                    add(1 / AdvancedMath$modulus_of_vector(matrix_x[i, ] - matrix_x_star[j, ]))
                  ) %>%
                  add(
                    matrix_h_4[i, j] %>%
                    multiply_by(matrix_derivative_of_x[i, 1]) %>%
                    divide_by(AdvancedMath$modulus_of_vector(matrix_derivative_of_x[i, ]))
                  )
                )
              )
            )
          }
        )
      }
    )
  }

  form_matrix_derivative_of_n_1_q <- function(size_of_vector_t, size_of_vector_q, matrix_x, matrix_x_infinity_star,
                                            matrix_x_infinity, matrix_derivative_of_x, matrix_derivative_of_x_infinity) {
    Helpers$generate_matrix_by_function(
      row_size    = size_of_vector_t,
      column_size = size_of_vector_q,
      func        = function(i, j) {
        Functions$derivative_of_green_function_n(
          vector_x               = matrix_x[i, ],
          vector_y               = matrix_x_infinity[j, ],
          vector_y_star          = matrix_x_infinity_star[j, ],
          vector_derivative_of_x = matrix_derivative_of_x[i, ],
          vector_derivative_of_y = matrix_derivative_of_x_infinity[j, ]
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
          func    = function(memo, j) {
            memo + vector_f[j] * matrix_derivative_of_n_1_q[i, j]
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
          (1 / 2 * capital_m) %>%
          multiply_by(vector_of_first_sums_from_derivative_of_v[i]))
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
