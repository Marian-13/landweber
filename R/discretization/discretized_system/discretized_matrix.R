DiscretizedMatrix <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  # matrix of (3.9)
  construct_matrix <- function(capital_m, t) {
    first_2_m_rows(capital_m, t) %>%
    last_matrix_row(capital_m)
  }

  first_2_m_rows <- function(capital_m, t) {
    indices <- 1:5

    elements <- zero_length_vector

    for (i in indices) {
      row <- zero_length_vector

      for (j in indices) {
        row <- c(row, matrix_koef(capital_m, t[i], t[j]))
      }

      row <- c(row, 1)

      elements <- c(elements, row)
    }
  }

  last_matrix_row <- function(capital_m) {
    rep(x = 1, times = 2 * capital_m) %>%
    c(0)
  }

  matrix_koef <- function(capital_m, t_i, t_j) {
    first_addend <-
      -0.5 %>%
      multiply_by(Parametrization$weight_function_r(capital_m, t_i, t_j))

    second_addend <-
      0.25 %>%
      multiply_by(Parametrization$kernel_h_1(t_i, t_j))

    first_addend + second_addend
  }

  diagonal_matrix_koef <- function(capital_m, t_i, t_j) {
    matrix_koef(capital_m, t_i, t_j)
  }

  zero_length_vector <- function() {
    AdvancedMath$zero_length_vector()
  }
})
