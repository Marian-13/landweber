Parametrization <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  lower_limit_of_t <- function() {
    0.0
  }

  upper_limit_of_t <- function() {
    2 * pi
  }

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

  # old_h1 <- function(t, tau) {
  #   (1 / 2) * log(1 / (exp(1) * AdvancedMath$SquareOfVector(derivative_of_x(t)))) +
  #     log(AdvancedMath$modulus_of_vector(x(t) - x_star(tau)))
  # }

  h_1 <- function(t, tau) {
    first_addend <-
      derivative_of_x(t) %>%
      AdvancedMath$square_of_vector() %>%
      multiply_by(AdvancedMath$EulerNumber) %>%
      { AdvancedMath$natural_logarithm(1 / .) } %>%
      multiply_by(1 / 2)

    second_addend <-
      x(t) %>%
      subtract(x_star(tau)) %>%
      AdvancedMath$modulus_of_vector() %>%
      AdvancedMath$natural_logarithm()

    first_addend + second_addend
  }
})
