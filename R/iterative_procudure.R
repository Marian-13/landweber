IterativeProcedure <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  # start <- function(capital_m, h_0, f_1, f_2) {
  start <- function(capital_m) {
    t <- calculate_t(capital_m)
    x <- calculate_x(t)
    x_infinity <- calculate_x_infinity(t)

    # First step
    h_0 <- calculate_h_0(x)
    f_2 <- calculate_f_2(x)

    function_f_2 <- get_function_f_2()

    # Second step
    u_0 <- calculate_u_0(
      capital_m = capital_m,
      t = t,
      x = x,
      x_infinity = x_infinity,
      h = h_0,
      f = f_2,
      function_f = function_f_2
    )

    h_with_zero_elements <- vector_with_zero_elements(size = 2 * capital_m)
    f_1 <- calculate_f_1(x)

    derivative_of_v_0 <- calculate_partial_normal_derivative_of_v_0_by_x(
      capital_m = capital_m,
      t = t,
      x = x,
      x_infinity = x_infinity,
      h = h_with_zero_elements,
      f = u_0 - f_1
      # function_f = f TODO refactor w_tilde !!!!!!
    )

    # Setup
    small_gamma <- get_small_gamma()

    k <- 0
    h_k <- h_0
    u_k <- u_0
    derivative_of_v_k <- derivative_of_v_0

    repeat {
      k <- k + 1

      # Third step
      new_h_k <- calculate_h_k(
        previous_h = h_k,
        small_gamma = small_gamma,
        derivative_of_v = derivative_of_v_0
      )

      # Forth step
      new_u_k <- calculate_u_k(
        capital_m = capital_m,
        t = t,
        x = x,
        x_infinity = x_infinity,
        h = h_k,
        f = f_2,
        function_f = function_f_2
      )

      new_derivative_of_v_k <- calculate_partial_normal_derivative_of_v_k_by_x(
        capital_m = capital_m,
        t = t,
        x = x,
        x_infinity = x_infinity,
        h = h_with_zero_elements,
        f = u_k - f_1
        # function_f = f TODO refactor w_tilde !!!!!!
      )

      if (stop_condition(previous_u = u_k, current_u = new_u_k) == TRUE) {
        break
      } else {
        h_k <- new_h_k
        u_k <- new_u_k
        derivative_of_v_k <- new_derivative_of_v_k
      }
    }

    construct_procedure_result(k = k, u = u_k)
  }

  calculate_t <- function(capital_m) {
    Mesh$equidistant_mesh_nodes(capital_m)
  }

  calculate_x <- function(t) {
    Map(function(t_i) { Functions$x(t_i) }, t)
  }

  calculate_x_infinity <- function(t) {
    Map(function(t_i) { Functions$x_infinity(t_i) }, t)
  }

  calculate_h_0 <- function(x) {
    Map(function(x_i) { ExampleSpecificFunctions$h_0(x) }, x)
  }

  calculate_f_2 <- function(x) {
    Map(function(x_i) { ExampleSpecificFunctions$f_2(x_i) }, x)
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
