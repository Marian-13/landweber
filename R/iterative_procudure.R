IterativeProcedure <- module({
  use(.GlobalEnv, attach = TRUE)

  # start <- function(capital_m, h_0, f_1, f_2) {
  start <- function(capital_m) {
    t <- calculate_t(capital_m)
    x <- calculate_x(t)

    # First step
    h_0 <- calculate_h_0(x)
    f_2 <- calculate_f_2(x)

    # Second step
    u_0 <- calculate_u_0(h = h_0, f = f_2)
    # normal_derivative_of_v_0 <- calculate_v_0(
    #   h = vector_with_zero_elements(size = ),
    #   f = u_0 - f_1
    # )
  }

  calculate_t <- function(capital_m) {
    # Discretization$equidistant_mesh_nodes(capital_m)
    Discretization$t(capital_m)
  }

  calculate_x <- function(t) {
    Map(function(t_i) { Parametrization$x(t_i) }, t)
  }

  calculate_h_0 <- function(x) {
    Map(function(x_i) { ExampleSpecificFunctions$h_0(x) }, x)
  }

  calculate_f_2 <- function(t) {
    Map(function(t_i) { ExampleSpecificFunctions$f_2(t_i) }, t)
  }

  calculate_u_0 <- function(h, f) {
    # TODO solve (3.4) with h = h_0, f = f_2 solution is mu
    # TODO find u_0 on Gamma by (3.11) Discretization$u(mu)
  }

  # # TODO IMPLEMENT, refactor
  # second_step <- function(capital_m, mu, f_1) {
  #   h <- AdvancedMath$vector_with_zero_elements(size = capital_m)
  #   f <- u_0 - f_1
  #   # TODO solve (3.4) with h = 0, f = u_0 - f_1
  #   # TODO find normal derivative of v_0 on Gamma_0 by (3.10)
  # }
  #
  # # TODO IMPLEMENT
  # calculate_normal_derivative_of_v_0 <- function() {
  #
  # }
  #
  # # TODO IMPLEMENT
  # calculate_size <- function(capital_m) {
  #   2 * capital_m + 1
  # }
  #
  # # TODO IMPLEMENT
  # vector_with_zero_elements <- function(size) {
  #
  # }
  #
  # # TODO IMPLEMENT
  # # capital or small
  # # uppercase or lowercase
  # # u_0 = initial_u
  # calculate_next_h <- function(previous_h, lowercase_gamma) {
  #
  # }
})
