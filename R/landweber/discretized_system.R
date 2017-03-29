source("R/landweber/discretized_system/discretized_matrix.R")
source("R/landweber/discretized_system/discretized_vector.R")
source("R/landweber/discretized_system/right_hand_side.R")

# (3.9)
DiscretizedSystem <- module({
  use(.GlobalEnv, attach = TRUE)

  solve <- function(capital_m, vector_t, matrix_x, vector_h, function_f) {
    sle_size <- 2 * capital_m + 1

    matrix <- .construct_discretized_matrix(capital_m, vector_t, sle_size)
    # vector <- .construct_discretized_vector(capital_m, x, h, function_f)

    # AdvancedMath$solve_sle(matrix, vector)
  }

  .construct_discretized_matrix <- function(capital_m, vector_t, sle_size) {
    DiscretizedMatrix$construct_matrix(capital_m, vector_t, sle_size)
  }

  .construct_discretized_vector <- function(capital_m, x, h, function_f) {
    DiscretizedVector$construct_vector(capital_m, x, h, function_f)
  }
})
