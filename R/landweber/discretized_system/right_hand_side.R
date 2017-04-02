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

  .calculate_element_q_3_j_i <- function(vector_x_j, vector_q_2_i, vector_q_2_star_i) {
    Functions$green_function_n(
      vector_x      = vector_x_j,
      vector_y      = vector_q_2_i,
      vector_y_star = vector_q_2_star_i
    )
  }

  .form_vector_q_3_j <- function(size_of_sum_from_w_tilde, vector_x_j, matrix_q_2, matrix_q_2_star) {
    vector_q_3_j <- Helpers$generate_vector(size_of_sum_from_w_tilde)

    indices <- 1:size_of_sum_from_w_tilde

    for (index in indices) {
      vector_q_3_j[index] <- .calculate_element_q_3_j_i(
        vector_x_j        = vector_x_j,
        vector_q_2_i      = matrix_q_2[index, ],
        vector_q_2_star_i = matrix_q_2_star[index, ]
      )
    }

    vector_q_3_j
  }

  form_matrix_q_3 <- function(size_of_vector_t, size_of_sum_from_w_tilde, matrix_x, matrix_q_2, matrix_q_2_star) {
    indices     <- 1:size_of_vector_t
    sum_indices <- 1:size_of_sum_from_w_tilde

    matrix_q_3 <- Helpers$generate_matrix(
      row_size    = size_of_vector_t,
      column_size = size_of_sum_from_w_tilde
    )

    for (j in indices) {
      for (i in sum_indices) {
        matrix_q_3[j, ] <- .form_vector_q_3_j(
          size_of_sum_from_w_tilde = size_of_sum_from_w_tilde,
          vector_x_j               = matrix_x[j, ],
          matrix_q_2               = matrix_q_2,
          matrix_q_2_star          = matrix_q_2_star
        )
      }
    }

    matrix_q_3
  }

  form_vector_q_4 <- function(size_of_sum_from_w_tilde, matrix_q_2, function_f_2) {
    Helpers$generate_vector_from_matrix(
      matrix = matrix_q_2,
      size   = size_of_sum_from_w_tilde,
      func   = function(vector_q_2_i) {
        function_f_2(vector_q_2_i)
      }
    )
  }

  form_vector_q_5 <- function(size_of_sum_from_w_tilde, matrix_q_2, function_u, function_f_1) {
    Helpers$generate_vector_from_matrix(
      matrix = matrix_q_2,
      size   = size_of_sum_from_w_tilde,
      func   = function(vector_q_2_i) {
        function_u(vector_q_2_i) - function_f_1(vector_q_2_i)
      }
    )
  }

  form_vector_q_6 <- function(size_of_vector_t, size_of_sum_from_w_tilde, h_infinity, matrix_q_3, vector_q_4) {
    indices     <- 1:size_of_vector_t
    sum_indices <- 1:size_of_sum_from_w_tilde

    vector_q_6 <- Helpers$generate_vector(size_of_vector_t)

    for (index in indices) {
      vector_q_6[index] <- h_infinity * Helpers$reduce(
        initial = 0.0,
        vector  = sum_indices,
        func    = function(memo, sum_index) {
          memo + vector_q_4[sum_index] * matrix_q_3[index, sum_index]
        }
      )
    }

    vector_q_6
  }

  form_vector_q_7 <- function(size_of_vector_t, size_of_sum_from_w_tilde, h_infinity, matrix_q_3, vector_q_5) {
    form_vector_q_6(
      size_of_vector_t = size_of_vector_t,
      size_of_sum_from_w_tilde = size_of_sum_from_w_tilde,
      h_infinity = h_infinity,
      matrix_q_3 = matrix_q_3,
      vector_q_4 = vector_q_5 # Correct when vector_q_5 !!!
    )
  }
})
