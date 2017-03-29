RightSizeSide <- module({
  use(.GlobalEnv, attach = TRUE)

  calculate_sum_from_w_tilde <- function(capital_m_1, h_infinity, function_f) {
    size <- capital_m_1 * 2 + 1

    q_1 <- form_vector_q_1(size, h_infinity)
    q_2 <- form_matrix_q_2(size, q_1)
    q_3 <- form_vector_q_3(size, q_2, function_f)
  }

  


  # q_1[index] = i * h_infinity # under (3.9)
  form_vector_q_1 <- function(size, h_infinity) {
    vector         <- vector(length = size)
    indices <- 1:size # vector indices

    i <- -2 # sum index i

    for (index in indices) {
      i <- i + 1
      vector[index] <- i * h_infinity
    }

    vector
  }

  form_matrix_q_2 <- function(size, vector_q_1) {
    Helpers$generate_matrix_from_vector(
      vector      = vector_q_1,
      row_size    = size,
      column_size = 2,
      func        = function(element_q_1_i) {
        c(element_q_1_i, 0)
      }
    )
  }

  form_vector_q_3 <- function(size, matrix_q_2, function_f) {
    Helpers$generate_vector_from_matrix(
      matrix = matrix_q_2,
      size   = size,
      func   = function(vector_of_matrix_q_2) {
        function_f(vector_of_matrix_q_2)
      }
    )
  }
})
