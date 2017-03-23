IterativeProcedure <- module({
  use(.GlobalEnv, attach = TRUE)

  # TODO IMPLEMENT
  # start <- function(capital_m, h_0, f_1, f_2) {
  start <- function(capital_m) {
    size <- calculate_size(capital_m) # TODO

    t <- calculate_t(capital_m)

    # First step
    h_0 <- calculate_h_0(t)

    # Second step
    u_0 <- calculate_u_0(h = h_0, f = f_2)
    normal_derivative_of_v_0 <- calculate_v_0(
      h = vector_with_zero_elements(size = ),
      f = u_0 - f_1
    )
  }

  # TODO IMPLEMENT, refactor
  second_step <- function(capital_m, mu, f_1) {
    h <- AdvancedMath$vector_with_zero_elements(size = capital_m)
    f <- u_0 - f_1
    # TODO solve (3.4) with h = 0, f = u_0 - f_1
    # TODO find normal derivative of v_0 on Gamma_0 by (3.10)
  }

  # TODO IMPLEMENT
  calculate_u_0 <- function(h, f) {
    # TODO solve (3.4) with h = h_0, f = f_2 solution is mu
    # TODO find u_0 on Gamma by (3.11) Discretization$u(mu)
  }

  # TODO IMPLEMENT
  calculate_normal_derivative_of_v_0 <- function() {

  }

  # TODO IMPLEMENT
  calculate_size(capital_m) {
    2 * capital_m + 1
  }

  # TODO IMPLEMENT
  calculate_t(capital_m) {
    Discretization$equidistant_mesh_nodes(capital_m)
  }

  # TODO IMPLEMENT
  calculate_h_0(t) {
    Map(function(obj) { ExampleSpecificFunctions$h_0(t) }, t)
  }

  # TODO IMPLEMENT
  vector_with_zero_elements <- function(size) {

  }

  # TODO IMPLEMENT
  # capital or small
  # uppercase or lowercase
  # u_0 = initial_u
  calculate_next_h <- function(previous_h, lowercase_gamma) {

  }
})
