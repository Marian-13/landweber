Functions <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  # Parametrization
  x <- function(t) {
    c(ExampleSpecificFunctions$x_1(t), ExampleSpecificFunctions$x_2(t))
  }

  derivative_of_x <- function(t) {
    c(
      ExampleSpecificFunctions$derivative_of_x_1(t),
      ExampleSpecificFunctions$derivative_of_x_2(t)
    )
  }

  x_star <- function(t) {
    c(ExampleSpecificFunctions$x_1(t), -ExampleSpecificFunctions$x_2(t))
  }

  x_infinity <- function(t) {
    c(t, 0)
  }

  lower_limit_of_t <- function() {
    0
  }

  # upper_limit_of_t <- function() {
  #   2 * pi
  # }

  # Discretization
  # near (3.7)
  weight_function_r <- function(capital_m, t, t_j) {
    indices <- 1:(capital_m - 1)

    sum <- Reduce(
      function(memo, index) {
        memo + (1 / index) * cos(index * (t - t_j))
      },
      indices,
      0.0
    )

    1 %>%
    add(2 * sum) %>%
    add((1 / capital_m) * cos(capital_m * (t - t_j))) %>%
    multiply_by(-(1 / (2 * capital_m)))
  }

  # in (3.5)
  kernel_h_1 <- function(t_i, t_j) {
    first_addend <-
      derivative_of_x(t_i) %>%
      AdvancedMath$square_of_vector() %>%
      multiply_by(AdvancedMath$EULER_NUMBER) %>%
      { AdvancedMath$natural_logarithm(1 / .) } %>%
      multiply_by(1 / 2)

    second_addend <-
      x(t_i) %>%
      subtract(x_star(t_j)) %>%
      AdvancedMath$modulus_of_vector() %>%
      AdvancedMath$natural_logarithm()

    first_addend + second_addend
  }

  h_infinity <- function() {
    ExampleSpecificFunctions$H_INFINITY
  }

  capital_m_1 <- function() {
    ExampleSpecificFunctions$CAPITAL_M_1
  }

  convert_to_x_star <- function(y) {
    c(y[1], -y[2])
  }

  green_function_n <- function(x, y) {
    first_multiplicand <-
      x %>%
      subtract(y) %>%
      AdvancedMath$modulus_of_vector()

    second_multiplicand <-
      x %>%
      subtract(convert_to_x_star(y)) %>%
      AdvancedMath$modulus_of_vector()

    first_multiplicand %>%
    multiply_by(second_multiplicand) %>%
    { AdvancedMath$natural_logarithm(1 / .) }
  }

  # x_j, h_j - numbers, f - function
  w_tilde <- function(x_j, h_j, function_f) {
    sum_indices <- (-capital_m_1()):capital_m_1()

    sum <- Reduce(
      function(memo, index) {
        t <- index * h_infinity()
        memo + function_f(x_infinity(t)) * green_function_n(x_j, x_infinity(t))
      },
      sum_indices,
      0
    )

    h_j - h_infinity() * sum
  }

  # (3.11)
  u <- function(x_infinity_i, capital_m, t, x, mu, alpha) {
    size <- length(t)
    indices <- 1:size

    first_sum <- 0

    for (j in indices) {
      x_j <- unlist(x[j])   # TODO unlist !!!
      first_sum <- first_sum + mu[j] * green_function_n(x_infinity_i, x_j)
    }

    (1 / (2 * capital_m)) * first_sum %>%
    # TODO second sum -- sinc quadrature for ln with e
    add(alpha)
  }

  # (3.10)
  partial_normal_derivative_of_v_by_x <- function(capital_m) {
    # TODO
  }
})
