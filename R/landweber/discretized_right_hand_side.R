DiscretizedRightHandSide <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  # right hand side of sle (3.9)
  construct_right_hand_side <- function(size_of_vector_t, vector_w_tilde) {
    first_2_m_elements(size_of_vector_t, vector_w_tilde) %>%
    c(last_element())
  }

  first_2_m_elements <- function(size_of_vector_t, vector_w_tilde) {
    Helpers$generate_vector_by_function(
      size = size_of_vector_t,
      func = function(i) {
        vector_w_tilde[i]
      }
    )
  }

  last_element <- function() {
    0
  }
})
