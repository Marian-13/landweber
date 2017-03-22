ExampleSpecificFunctions <- module({
  x_1 <- function(t) {
    1.5 * cos(t)
  }

  x_2 <- function(t) {
    sin(t) + 1.5
  }

  derivative_of_x_1 <- function(t) {
    -1.5 * sin(t)
  }

  derivative_of_x_2 <- function(t) {
    cos(t)
  }
})
