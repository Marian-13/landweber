source("R/landweber/discretized_system/discretized_matrix.R")
source("R/landweber/discretized_system/discretized_vector.R")
source("R/landweber/discretized_system/right_hand_side.R")

# (3.9)
DiscretizedSystem <- module({
  use(.GlobalEnv, attach = TRUE)

  solve <- function(constants, sizes, vectors, matrices, functions) {
    # matrix <- .construct_discretized_matrix(
    #   capital_m              = capital_m,
    #   vector_t               = vector_t,
    #   size_of_vector_t       = size_of_vector_t,
    #   matrix_x               = matrix_x,
    #   matrix_derivative_of_x = matrix_derivative_of_x,
    #   matrix_x_star          = matrix_x_star,
    #   sle_size               = sle_size
    # )

    matrix <- .construct_discretized_matrix(
      capital_m        = constants$capital_m,
      vector_t         = vectors$t,
      sizes            = sizes
    )

    # vector <- .construct_discretized_vector(capital_m, x, h, function_f)

    # AdvancedMath$solve_sle(matrix, vector)
  }

  .construct_discretized_matrix <- function(capital_m, vector_t, sizes) {
    DiscretizedMatrix$construct_matrix(capital_m, vector_t, sizes)
  }

  .construct_discretized_vector <- function(capital_m, x, h, function_f) {
    DiscretizedVector$construct_vector(capital_m, x, h, function_f)
  }
})
