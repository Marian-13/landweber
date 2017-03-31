RightHandSide <- module({
  use(.GlobalEnv, attach = TRUE)

  calculate_sum_from_w_tilde <- function(capital_m_1, h_infinity, function_f) {
    size <- capital_m_1 * 2 + 1

    q_1 <- form_vector_q_1(size, h_infinity)
    q_2 <- form_matrix_q_2(size, q_1)
    q_3 <- form_vector_q_3(size, q_2, function_f)
  }

  form_vector_q_1 <- function(size_of_sum_from_w_tilde, h_infinity) {
    vector <- Helpers$generate_vector(size_of_sum_from_w_tilde)
    indices <- 1:size_of_sum_from_w_tilde # vector indices

    i <- -2 # sum index i

    for (index in indices) {
      i <- i + 1
      vector[index] <- i * h_infinity
    }

    vector
  }

  form_matrix_q_2 <- function(size_of_sum_from_w_tilde, vector_q_1) {
    Helpers$generate_matrix_from_vector(
      vector      = vector_q_1,
      row_size    = size_of_sum_from_w_tilde,
      column_size = 2,
      func        = function(element_q_1_i) {
        c(element_q_1_i, 0)
      }
    )
  }

  # form_vector_q_3 <- function(size_of_vector_t, matrix_x, matrix_x_star, matrix_q_2) {
  #   vector_q_3 <- Helpers$generate_vector(size_of_vector_t)
  #
  #   indices <- 1:size_of_vector_t
  #
  #   for (index in indices) {
  #     vector_q_3[index] <- Functions$green_function_n(
  #       vector_x      = matrix_x[index, ],
  #       vector_y      = matrix_q_2[index, ],
  #       vector_y_star = matrix_x_star[index, ]
  #     )
  #   }
  #
  #   vector_q_3
  # }

  form_vector_q_3 <- function(size_of_sum_from_w_tilde, matrix_q_2, function_f_2) {
    Helpers$generate_vector_from_matrix(
      matrix = matrix_q_2,
      size   = size_of_sum_from_w_tilde,
      func   = function(vector_of_matrix_q_2) {
        function_f_2(vector_of_matrix_q_2)
      }
    )
  }

  # form_vector_q_4 <- function(size_of_sum_from_w_tilde, matrix_q_2, vector_q_3) {
  #
  # }
})
