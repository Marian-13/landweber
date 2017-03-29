DiscretizedVector <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  # vector of sle (3.9)
  construct_vector <- function(capital_m, x, h, function_f) {
    first_2_m_elements(capital_m, x, h, function_f) %>%
    c(last_element())
  }

  first_2_m_elements <- function(capital_m, x, h, function_f) {
    indices <- 1:(2 * capital_m)

    elements <- zero_length_vector()

    for (i in indices) {
      x_i <- unlist(x[i])   # TODO unlist !!!
      h_i <- unlist(h[i])   # TODO unlist !!!

      element <- Functions$w_tilde(x_i, h_i, function_f)

      elements <- c(elements, element)
    }

    elements
  }

  last_element <- function() {
    0
  }

  zero_length_vector <- function() {
    AdvancedMath$zero_length_vector()
  }
})
