DiscretizedRightHandSide <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  form_matrix_n_1_q <- function(size_of_vector_t, size_of_vector_q, matrix_x,
                                matrix_x_infinity, matrix_x_infinity_star) {
    Helpers$generate_matrix_by_function(
      row_size    = size_of_vector_t,
      column_size = size_of_vector_q,
      func        = function(i, j) {
        Functions$green_function_n(
          vector_x               = matrix_x[i, ],
          vector_y               = matrix_x_infinity[j, ],
          vector_y_star          = matrix_x_infinity_star[j, ]
        )
      }
    )
  }

  form_vector_f <- function(size_of_vector_q, matrix_x_infinity, function_f) {
    Helpers$generate_vector_from_matrix(
      matrix = matrix_x_infinity,
      size   = size_of_vector_q,
      func   = function_f
    )
  }

  form_vector_of_sums_from_w_tilde <- function(size_of_vector_t, size_of_vector_q, vector_f, matrix_n_1_q) {
    sum_indices <- 1:size_of_vector_q

    Helpers$generate_vector_by_function(
      size = size_of_vector_t,
      func = function(i) {
        Helpers$reduce(
          initial = 0,
          vector  = sum_indices,
          func    = function(memo, j) {
            memo + vector_f[j] * matrix_n_1_q[i, j]
          }
        )
      }
    )
  }

  form_vector_w_tilde <- function(size_of_vector_t, vector_h, h_infinity, vector_of_sums_from_w_tilde) {
    Helpers$generate_vector_by_function(
      size = size_of_vector_t,
      func = function(index) {
        vector_h[index] - h_infinity * vector_of_sums_from_w_tilde[index]
      }
    )
  }

  # right hand side of sle (3.9)
  form_discretized_right_hand_side <- function(size_of_vector_t, vector_w_tilde) {
    c(
      .first_2_m_elements(size_of_vector_t, vector_w_tilde),
      .last_element()
    )
  }

  .first_2_m_elements <- function(size_of_vector_t, vector_w_tilde) {
    Helpers$generate_vector_by_function(
      size = size_of_vector_t,
      func = function(i) {
        vector_w_tilde[i]
      }
    )
  }

  .last_element <- function() {
    0
  }
})
