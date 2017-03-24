Parametrization <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

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

  lower_limit_of_t <- function() {
    0
  }

  # upper_limit_of_t <- function() {
  #   2 * pi
  # }

  # x_infinity <- function(t) {
  #   c(t, 0)
  # }

  # # TODO
  # f_tilde <- function() {
  #
  # }
})
