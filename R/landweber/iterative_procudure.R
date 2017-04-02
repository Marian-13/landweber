IterativeProcedure <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  start <- function(capital_m, example_specific_functions) {
    constants <- list()
    vectors   <- list()
    matrices  <- list()
    functions <- list()
    sizes     <- list()

    constants$capital_m <- capital_m
    vectors$t <- .form_vector_t(capital_m)

    sizes$t <- Helpers$calculate_size_of_vector(vectors$t)

    functions$x <- .form_function_x(
      function_x_1 = example_specific_functions$x_1,
      function_x_2 = example_specific_functions$x_2
    )

    matrices$x <- .form_matrix_x(vectors$t, sizes$t, functions$x)  # matrix - vector of vectors

    functions$x_infinity <- .form_function_x_infinity()

    matrices$x_infinity <- .form_matrix_x_infinity(
      vector_t            = vectors$t,
      size_of_vector_t    = sizes$t,
      function_x_infinity = functions$x_infinity
    )

    # # First step !!!
    functions$h_0 <- example_specific_functions$h_0
    functions$f_2 <- example_specific_functions$f_2

    vectors$h_0 <- .form_vector_h_0(matrices$x, sizes$t, functions$h_0)
    vectors$f_2 <- .form_vector_f_2(matrices$x, sizes$t, functions$f_2)

    # # Second step !!!
    constants$capital_m_1 <- example_specific_functions$CAPITAL_M_1
    constants$h_infinity  <- example_specific_functions$H_INFINITY

    functions$derivative_of_x <- .form_function_derivative_of_x(
      function_derivative_of_x_1 = example_specific_functions$derivative_of_x_1,
      function_derivative_of_x_2 = example_specific_functions$derivative_of_x_2
    )

    matrices$derivative_of_x <- .form_matrix_derivative_of_x(
      vector_t                 = vectors$t,
      size_of_vector_t         = sizes$t,
      function_derivative_of_x = functions$derivative_of_x
    )

    matrices$x_star <- .form_matrix_x_star(
      size_of_vector_t = sizes$t,
      matrix_x         = matrices$x
    )

    sizes$discretized_system <- .calculate_size_of_discretized_system(
      capital_m = constants$capital_m
    )

    matrices$discretized_matrix <- .construct_discretized_matrix(
      capital_m = constants$capital_m,
      vectors   = vectors,
      matrices  = matrices,
      sizes     = sizes
    )

    sizes$sum_from_w_tilde <- .calculate_size_of_sum_from_w_tilde(
      capital_m_1 = constants$capital_m_1
    )

    # from w_tilde(t[j]): q_1[i] = i * h_infinity
    vectors$q_1 <- .form_vector_q_1(
      size_of_sum_from_w_tilde = sizes$sum_from_w_tilde,
      h_infinity               = constants$h_infinity
    )

    # from w_tilde(t[j]): q_2[i] = x_infinity(i * h_infinity)
    matrices$q_2 <- .form_matrix_q_2(
      size_of_sum_from_w_tilde = sizes$sum_from_w_tilde,
      vector_q_1               = vectors$q_1
    )

    matrices$q_2_star <- .form_matrix_q_2_star(
      size_of_sum_from_w_tilde = sizes$sum_from_w_tilde,
      matrix_q_2               = matrices$q_2
    )

    matrices$q_3 <- .form_matrix_q_3(
      size_of_vector_t         = sizes$t,
      size_of_sum_from_w_tilde = sizes$sum_from_w_tilde,
      matrix_x                 = matrices$x,
      matrix_q_2               = matrices$q_2,
      matrix_q_2_star          = matrices$q_2_star
    )

    vectors$q_4 <- .form_vector_q_4(
      size_of_sum_from_w_tilde = sizes$sum_from_w_tilde,
      matrix_q_2               = matrices$q_2,
      function_f_2             = functions$f_2
    )

    functions$f_1 <- example_specific_functions$f_1

    vectors$q_5 <- .form_vector_q_5(
      size_of_sum_from_w_tilde = sizes$sum_from_w_tilde,
      matrix_q_2               = matrices$q_2,
      function_u               = function(x) { 0 },
      function_f_1             = functions$f_1
    )

    vectors$q_6 <- .form_vector_q_6(
      size_of_vector_t         = sizes$t,
      size_of_sum_from_w_tilde = sizes$sum_from_w_tilde,
      h_infinity               = constants$h_infinity,
      matrix_q_3               = matrices$q_3,
      vector_q_4               = vectors$q_4
    )

    vectors$q_7 <- .form_vector_q_7(
      size_of_vector_t         = sizes$t,
      size_of_sum_from_w_tilde = sizes$sum_from_w_tilde,
      h_infinity               = constants$h_infinity,
      matrix_q_3               = matrices$q_3,
      vector_q_5               = vectors$q_5
    )

    p(vectors$h_0, "h_0")
    p(vectors$q_6, "q_6")

    vectors$w_tilde_1 <- .form_vector_w_tilde_1(
      size_of_vector_t = sizes$t,
      vector_h         = vectors$h_0,
      vector_q_6       = vectors$q_6
    )

    p(vectors$w_tilde_1, "w_tilde_1")

    p(vectors$h_0, "h_0")
    p(vectors$q_7, "q_7")

    vectors$w_tilde_2 <- .form_vector_w_tilde_2(
      size_of_vector_t = sizes$t,
      vector_h         = vectors$h_0,
      vector_q_7       = vectors$q_7
    )

    p(vectors$w_tilde_2, "w_tilde_2")
    # # TODO
  }

  .form_vector_t <- function(capital_m) {
    step       <- AdvancedMath$PI / capital_m
    first_node <- Functions$get_lower_limit_of_t()
    last_node  <- Functions$get_upper_limit_of_t() - step

    Mesh$generate_equidistant_nodes(
      first_node = first_node,
      last_node  = last_node,
      step       = step
    )
  }

  .form_function_x <- function(function_x_1, function_x_2) {
    function(t) { c(function_x_1(t), function_x_2(t)) }
  }

  .form_matrix_x <- function(vector_t, size_of_vector_t, function_x) {
    Helpers$generate_matrix_from_vector(
      vector      = vector_t,
      row_size    = size_of_vector_t,
      column_size = 2,
      func        = function(element_t_i) {
        function_x(element_t_i)
      }
    )
  }

  .form_function_x_infinity <- function() {
    function(t) { c(t, 0) }
  }

  .form_matrix_x_infinity <- function(vector_t, size_of_vector_t, function_x_infinity) {
    Helpers$generate_matrix_from_vector(
      vector      = vector_t,
      row_size    = size_of_vector_t,
      column_size = 2,
      func        = function(element_t_i) {
        function_x_infinity(element_t_i)
      }
    )
  }

  .form_vector_h_0 <- function(matrix_x, size_of_vector_t, function_h_0) {
    Helpers$generate_vector_from_matrix(
      matrix = matrix_x,
      size   = size_of_vector_t,
      func   = function(vector_of_matrix_x) {
        function_h_0(vector_of_matrix_x)
      }
    )
  }

  .form_vector_f_2 <- function(matrix_x, size_of_vector_t, function_f_2) {
    Helpers$generate_vector_from_matrix(
      matrix = matrix_x,
      size   = size_of_vector_t,
      func   = function(vector_of_matrix_x) {
        function_f_2(vector_of_matrix_x)
      }
    )
  }

  .form_function_derivative_of_x <- function(function_derivative_of_x_1, function_derivative_of_x_2) {
    function(t) {
      c(function_derivative_of_x_1(t), function_derivative_of_x_2(t))
    }
  }

  .form_matrix_derivative_of_x <- function(vector_t, size_of_vector_t, function_derivative_of_x) {
    Helpers$generate_matrix_from_vector(
      vector      = vector_t,
      row_size    = size_of_vector_t,
      column_size = 2,
      func        = function(element_t_i) {
        function_derivative_of_x(element_t_i)
      }
    )
  }

  .form_matrix_x_star <- function(size_of_vector_t, matrix_x) {
    Helpers$generate_matrix_from_matrix(
      matrix      = matrix_x,
      row_size    = size_of_vector_t,
      column_size = 2,
      func        = function(vector_x_i) {
        c(vector_x_i[1], -vector_x_i[2])
      }
    )
  }

  .calculate_size_of_discretized_system <- function(capital_m) {
    2 * capital_m + 1
  }

  .construct_discretized_matrix <- function(capital_m, vectors, matrices, sizes) {
    DiscretizedMatrix$construct_matrix(
      capital_m = capital_m,
      vectors   = vectors,
      matrices  = matrices,
      sizes     = sizes
    )
  }

  .calculate_size_of_sum_from_w_tilde <- function(capital_m_1) {
    capital_m_1 * 2 + 1
  }

  .form_vector_q_1 <- function(size_of_sum_from_w_tilde, h_infinity) {
    RightHandSide$form_vector_q_1(size_of_sum_from_w_tilde, h_infinity)
  }

  .form_matrix_q_2 <- function(size_of_sum_from_w_tilde, vector_q_1) {
    RightHandSide$form_matrix_q_2(size_of_sum_from_w_tilde, vector_q_1)
  }

  .form_matrix_q_2_star <- function(size_of_sum_from_w_tilde, matrix_q_2) {
    Helpers$generate_matrix_from_matrix(
      matrix      = matrix_q_2,
      row_size    = size_of_sum_from_w_tilde,
      column_size = 2,
      func        = function(vector_q_2_i) {
        c(vector_q_2_i[1], -vector_q_2_i[2])
      }
    )
  }

  .form_matrix_q_3 <- function(size_of_vector_t, size_of_sum_from_w_tilde, matrix_x, matrix_q_2, matrix_q_2_star) {
    RightHandSide$form_matrix_q_3(
      size_of_vector_t         = size_of_vector_t,
      size_of_sum_from_w_tilde = size_of_sum_from_w_tilde,
      matrix_x                 = matrix_x,
      matrix_q_2               = matrix_q_2,
      matrix_q_2_star          = matrix_q_2_star
    )
  }

  .form_vector_q_4 <- function(size_of_sum_from_w_tilde, matrix_q_2, function_f_2) {
    RightHandSide$form_vector_q_4(
      size_of_sum_from_w_tilde = size_of_sum_from_w_tilde,
      matrix_q_2               = matrix_q_2,
      function_f_2             = function_f_2
    )
  }

  .form_vector_q_5 <- function(size_of_sum_from_w_tilde, matrix_q_2, function_u, function_f_1) {
    RightHandSide$form_vector_q_5(
      size_of_sum_from_w_tilde = size_of_sum_from_w_tilde,
      matrix_q_2               = matrix_q_2,
      function_u               = function_u,
      function_f_1             = function_f_1
    )
  }

  .form_vector_q_6 <- function(size_of_vector_t, size_of_sum_from_w_tilde, h_infinity, matrix_q_3, vector_q_4) {
    RightHandSide$form_vector_q_6(
      size_of_vector_t         = size_of_vector_t,
      size_of_sum_from_w_tilde = size_of_sum_from_w_tilde,
      h_infinity               = h_infinity,
      matrix_q_3               = matrix_q_3,
      vector_q_4               = vector_q_4
    )
  }

  .form_vector_q_7 <- function(size_of_vector_t, size_of_sum_from_w_tilde, h_infinity, matrix_q_3, vector_q_5) {
    RightHandSide$form_vector_q_7(
      size_of_vector_t         = size_of_vector_t,
      size_of_sum_from_w_tilde = size_of_sum_from_w_tilde,
      h_infinity               = h_infinity,
      matrix_q_3               = matrix_q_3,
      vector_q_5               = vector_q_5
    )
  }

  .form_vector_w_tilde_1 <- function(size_of_vector_t, vector_h, vector_q_6) {
    RightHandSide$form_vector_w_tilde_1(size_of_vector_t, vector_h, vector_q_6)
  }

  .form_vector_w_tilde_2 <- function(size_of_vector_t, vector_h, vector_q_7) {
    RightHandSide$form_vector_w_tilde_2(size_of_vector_t, vector_h, vector_q_7)
  }

  # TODO
  .form_vector_u <- function(constants, sizes, vectors, matrices, functions) {

    # vector_mu <- .extract_vector_mu(size_of_vector_t, discretized_system_solution)
    # alpha     <- .extract_alpha(size_of_vector_t, discretized_system_solution)
    #
    # Helpers$generate_vector_from_matrix(
    #   matrix = matrix_x_infinity,
    #   size   = size_of_vector_t,
    #   func   = function(vector_of_matrix_x_infinity) {
    #     Functions$u(
    #       vector_of_matrix_x_infinity,
    #       capital_m,
    #       vector_t,
    #       matrix_x,
    #       vector_mu,
    #       alpha
    #     )
    #   }
    # )
  }

  .extract_mu <- function(size_of_vector_t, discretized_system_solution) {
    c(discretized_system_solution[1:size_of_vector_t])
  }

  .extract_alpha <- function(size_of_vector_t, discretized_system_solution) {
    discretized_system_solution[size_of_vector_t + 1]
  }
})
