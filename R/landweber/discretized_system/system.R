System <- module({
  use(.GlobalEnv, attach = TRUE)

  form_matrix_r <- function(capital_m, size_of_vector_t, vector_t) {
    indices <- 1:size_of_vector_t

    matrix_r <- Helpers$generate_square_matrix(size_of_vector_t)

    for (i in indices) {
      for (j in indices) {
        matrix_r[i, j] <- Functions$weight_function_r(
          capital_m   = capital_m,
          element_t_i = vector_t[i],
          element_t_j = vector_t[j]
        )
      }
    }

    matrix_r
  }
})
