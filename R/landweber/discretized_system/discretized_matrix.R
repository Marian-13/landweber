DiscretizedMatrix <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  # matrix of sle (3.9)
  construct_matrix <- function(capital_m, vector_t, size) {
    first_2_m_rows_elements(capital_m, vector_t) %>%
    c(last_row_elements(capital_m)) %>%
    matrix(nrow = size, ncol = size, byrow = TRUE)
  }

  first_2_m_rows_elements <- function(capital_m, t) {

    indices <- 1:(2 * capital_m)

    elements <- zero_length_vector()

    for (i in indices) {
      row <- zero_length_vector()

      for (j in indices) {
        row <- c(row, coeficient(capital_m, t[i], t[j]))
      }

      row <- c(row, 1)

      elements <- c(elements, row)
    }

    elements
  }

  last_row_elements <- function(capital_m) {
    rep(x = 1, times = 2 * capital_m) %>%
    c(0)
  }

  coeficient <- function(capital_m, t_i, t_j) {
    p("HEllo")
    first_addend <-
      -0.5 %>%
      multiply_by(Functions$weight_function_r(capital_m, t_i, t_j))

    second_addend <-
      0.25 %>%
      multiply_by(Functions$kernel_h_1(t_i, t_j))

    first_addend + second_addend
  }

  # diagonal_coeficient <- function(capital_m, t_i, t_j) {
  #   matrix_coeficient(capital_m, t_i, t_j)
  # }

  zero_length_vector <- function() {
    AdvancedMath$zero_length_vector()
  }
})
