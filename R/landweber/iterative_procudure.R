IterativeProcedure <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  start <- function(capital_m, example_specific_functions) {
    vector_t         <- .form_vector_t(capital_m)
    size_of_vector_t <- Helpers$calculate_size_of_vector(vector_t)

    function_x_1 <- example_specific_functions$x_1
    function_x_2 <- example_specific_functions$x_2

    matrix_x <- .form_matrix_x(vector_t, size_of_vector_t, function_x_1, function_x_2)  # matrix - vector of vectors

    matrix_x_infinity <- .form_matrix_x_infinity(vector_t, size_of_vector_t)

    # First step
    function_h_0 <- example_specific_functions$h_0
    function_f_2 <- example_specific_functions$f_2

    vector_h_0 <- .form_vector_h_0(matrix_x, size_of_vector_t, function_h_0)
    vector_f_2 <- .form_vector_f_2(matrix_x, size_of_vector_t, function_f_2)

    # function_f_2 <- get_function_f_2()
    #
    # # Second step
    # u_0 <- calculate_u_0(
    #   capital_m = capital_m,
    #   t = t,
    #   x = x,
    #   x_infinity = x_infinity,
    #   h = h_0,
    #   f = f_2,
    #   function_f = function_f_2
    # )
    #
    # h_with_zero_elements <- vector_with_zero_elements(size = 2 * capital_m)
    # f_1 <- calculate_f_1(x)
    #
    # derivative_of_v_0 <- calculate_partial_normal_derivative_of_v_0_by_x(
    #   capital_m = capital_m,
    #   t = t,
    #   x = x,
    #   x_infinity = x_infinity,
    #   h = h_with_zero_elements,
    #   f = u_0 - f_1
    #   # function_f = f TODO refactor w_tilde !!!!!!
    # )
    #
    # # Setup
    # small_gamma <- get_small_gamma()
    #
    # k <- 0
    # h_k <- h_0
    # u_k <- u_0
    # derivative_of_v_k <- derivative_of_v_0
    #
    # repeat {
    #   k <- k + 1
    #
    #   # Third step
    #   new_h_k <- calculate_h_k(
    #     previous_h = h_k,
    #     small_gamma = small_gamma,
    #     derivative_of_v = derivative_of_v_0
    #   )
    #
    #   # Forth step
    #   new_u_k <- calculate_u_k(
    #     capital_m = capital_m,
    #     t = t,
    #     x = x,
    #     x_infinity = x_infinity,
    #     h = h_k,
    #     f = f_2,
    #     function_f = function_f_2
    #   )
    #
    #   new_derivative_of_v_k <- calculate_partial_normal_derivative_of_v_k_by_x(
    #     capital_m = capital_m,
    #     t = t,
    #     x = x,
    #     x_infinity = x_infinity,
    #     h = h_with_zero_elements,
    #     f = u_k - f_1
    #     # function_f = f TODO refactor w_tilde !!!!!!
    #   )
    #
    #   if (stop_condition(previous_u = u_k, current_u = new_u_k) == TRUE) {
    #     break
    #   } else {
    #     h_k <- new_h_k
    #     u_k <- new_u_k
    #     derivative_of_v_k <- new_derivative_of_v_k
    #   }
    # }
    #
    # construct_procedure_result(k = k, u = u_k)
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

  .form_matrix_x <- function(vector_t, size_of_vector_t, function_x_1, function_x_2) {
    Helpers$generate_matrix_from_vector(
      vector      = vector_t,
      row_size    = size_of_vector_t,
      column_size = 2,
      func        = function(element_t_i) {
        c(function_x_1(element_t_i), function_x_2(element_t_i))
      }
    )
  }

  .form_matrix_x_infinity <- function(vector_t, size_of_vector_t) {
    Helpers$generate_matrix_from_vector(
      vector      = vector_t,
      row_size    = size_of_vector_t,
      column_size = 2,
      func        = function(element_t_i) {
        c(element_t_i, 0)
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

  get_function_f_2 <- function() {
    ExampleSpecificFunctions$f_2
  }

  calculate_u_0 <- function(capital_m, t, x, x_infinity, h, f, function_f) {
    calculate_u_k(capital_m, t, x, x_infinity, h, f, function_f)
  }

  vector_with_zero_elements <- function(size) {
    AdvancedMath$vector_with_zero_elements(size)
  }

  calculate_f_1 <- function(x) {
    Map(function(x_i) { ExampleSpecificFunctions$f_1(x_i) }, x)
  }

  # TODO function_f
  calculate_partial_normal_derivative_of_v_0_by_x <- function(capital_m, t, x, x_infinity, h, f) {
    calculate_partial_normal_derivative_of_v_k_by_x(
      capital_m, t, x, x_infinity, h, f
    )
  }

  get_small_gamma <- function() {
    ExampleSpecificFunctions$SMALL_GAMMA
  }

  # TODO test
  calculate_h_k <- function(previous_h, small_gamma, derivative_of_v) {
    unlist(previous_h) - small_gamma * derivative_of_v
  }

  calculate_u_k <- function(capital_m, t, x, x_infinity, h, f, function_f) {
    matrix <- DiscretizedSystem$discretized_matrix(capital_m, t)
    vector <- DiscretizedSystem$discretized_vector(capital_m, x, h, function_f)

    sle_solution <- AdvancedMath$solve_sle(matrix, vector)

    size <- 2 * capital_m

    mu <- c(sle_solution[1:size])

    alpha <- sle_solution[size + 1]

    u_0 <- Map(
      function(x_infinity_i) {
        Functions$u(unlist(x_infinity_i), capital_m, t, x, mu, alpha) # TODO unlist
      },
      x_infinity
    )
  }

  calculate_partial_normal_derivative_of_v_k_by_x <- function(capital_m, t, x, x_infinity, h, f) {
    # TODO remove stub
    AdvancedMath$vector_with_zero_elements(size = 2 * capital_m)
  }

  # TODO name
  stop_condition <- function(current_u, previous_u) {
    # TODO remove stub
    TRUE
  }

  construct_procedure_result <- function(k, u) {
    result <- list()
    result$k <- k
    result$u <- u

    result
  }
})
