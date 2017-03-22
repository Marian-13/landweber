ExampleSpecificFunctions <- module({
  x_1 <- function(t) {
    radial_function_r(t) * cos(t)
  }

  x_2 <- function(t) {
    radial_function_r(t) * sin(t) + 1.5
  }

  derivative_of_x_1 <- function(t) {
    derivative_of_radial_function_r(t) * cos(t) - radial_function_r(t) * sin(t)
  }

  derivative_of_x_2 <- function(t) {
    derivative_of_radial_function_r(t) * sin(t) + radial_function_r(t) * cos(t)
  }

  radial_function_r <- function(t) {
    sqrt(cos(t)^2 + 0.25 * sin(t)^2)
  }

  derivative_of_radial_functionR <- function(t) {
    (-0.75 * sin(t) * cos(t)) / (sqrt(cos(t)^2 + 0.25 * sin(t)^2))
  }
})
