DiscretizedRightHandSide <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  .calculate_element_n <- function(vector_x, vector_x_infinity, vector_x_infinity_star) {
    Functions$green_function_n(
      vector_x      = vector_x,
      vector_y      = vector_x_infinity,
      vector_y_star = vector_x_infinity_star
    )
  }

  .form_vector_n <- function(size_of_vector_q, vector_x, matrix_x_infinity, matrix_x_infinity_star) {
    vector_n <- Helpers$generate_vector(size_of_vector_q)

    indices <- 1:size_of_vector_q

    for (index in indices) {
      vector_n[index] <- .calculate_element_n(
        vector_x                 = vector_x,
        vector_x_infinity      = matrix_x_infinity[index, ],
        vector_x_infinity_star = matrix_x_infinity_star[index, ]
      )
    }

    vector_n
  }

  form_matrix_n <- function(size_of_vector_t, size_of_vector_q, matrix_x, matrix_x_infinity, matrix_x_infinity_star) {
    indices     <- 1:size_of_vector_t
    sum_indices <- 1:size_of_vector_q

    matrix_n <- Helpers$generate_matrix(
      row_size    = size_of_vector_t,
      column_size = size_of_vector_q
    )

    for (j in indices) {
      for (i in sum_indices) {
        matrix_n[j, ] <- .form_vector_n(
          size_of_vector_q         = size_of_vector_q,
          vector_x                 = matrix_x[j, ],
          matrix_x_infinity      = matrix_x_infinity,
          matrix_x_infinity_star = matrix_x_infinity_star
        )
      }
    }

    matrix_n
  }

  form_vector_f_tilde <- function(size_of_vector_q, matrix_x_infinity, function_f) {
    Helpers$generate_vector_from_matrix(
      matrix = matrix_x_infinity,
      size   = size_of_vector_q,
      func   = function_f
    )
  }

  form_vector_of_sums_from_w_tilde <- function(size_of_vector_t, size_of_vector_q, vector_f_tilde, matrix_n_1_q) {
    indices     <- 1:size_of_vector_t
    sum_indices <- 1:size_of_vector_q

    vector_of_sums_from_w_tilde <- Helpers$generate_vector(size_of_vector_t)

    for (index in indices) {
      vector_of_sums_from_w_tilde[index] <- Helpers$reduce(
        initial = 0.0,
        vector  = sum_indices,
        func    = function(memo, sum_index) {
          memo + vector_f_tilde[sum_index] * matrix_n_1_q[index, sum_index]
        }
      )
    }

    vector_of_sums_from_w_tilde
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
    .first_2_m_elements(size_of_vector_t, vector_w_tilde) %>%
    c(.last_element())
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
