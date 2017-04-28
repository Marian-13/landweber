Formula311 <- module({
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

  # TODO
  form_matrix_n_t_q <- function(size_of_vector_q, matrix_x_infinity, matrix_x_infinity_star) {
    # TODO Remove Singularity!!!
    Helpers$generate_matrix_by_function(
      row_size    = size_of_vector_q,
      column_size = size_of_vector_q,
      func        = function(i, j) {
        # TODO Remove if, result !!!
        result <- Functions$green_function_n(
          vector_x      = matrix_x_infinity[i, ],
          vector_y      = matrix_x_infinity[j, ],
          vector_y_star = matrix_x_infinity_star[j, ]
        )

        if (AdvancedMath$is_infinity(result)) {
          0 # TODO TODO TODO
        } else {
          result
        }
      }
    )
  }

  # TODO
  form_vector_of_second_sums_from_u <- function(size_of_vector_q, vector_f_tilde, matrix_n_t_q) {
    sum_indices <- 1:size_of_vector_q

    Helpers$generate_vector_by_function(
      size = size_of_vector_q,
      func = function(i) {
        Helpers$reduce(
          initial = 0,
          vector  = sum_indices,
          func    = function(memo, j) {
            memo + vector_f_tilde[j] * matrix_n_t_q[i, j]
          }
        )
      }
    )
  }

  form_vector_u <- function(size_of_vector_q, capital_m, h_infinity,
                              vector_of_first_sums_from_u, vector_of_second_sums_from_u) {
    Helpers$generate_vector_by_function(
      size = size_of_vector_q,
      func = function(i) {
        first_addend  <- (1 / (2 * capital_m)) * (vector_of_first_sums_from_u[i])
        second_addend <- h_infinity * vector_of_second_sums_from_u[i] # TODO Possible 1 / 2 * PI ?

        first_addend + second_addend
      }
    )
  }
})
