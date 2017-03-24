Parametrization <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  x <- function(t) {
    c(ExampleSpecificFunctions$x_1(t), ExampleSpecificFunctions$x_2(t))
  }

  lower_limit_of_t <- function() {
    0
  }

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

  #
  # upper_limit_of_t <- function() {
  #   2 * pi
  # }
  #
  #
  # derivative_of_x <- function(t) {
  #   c(
  #     ExampleSpecificFunctions$derivative_of_x_1(t),
  #     ExampleSpecificFunctions$derivative_of_x_2(t)
  #   )
  # }
  #
  # x_star <- function(t) {
  #   c(ExampleSpecificFunctions$x_1(t), -ExampleSpecificFunctions$x_2(t))
  # }
  #
  # x_infinity <- function(t) {
  #   c(t, 0)
  # }

  # # TODO
  # f_tilde <- function() {
  #
  # }
})
