ExampleSpecificFunctions <- module({
  use(.GlobalEnv, attach = TRUE)

  CAPITAL_M_1 <- 10

  H_INFINITY <- (function() {
    c <- 1
    c / AdvancedMath$square_root(CAPITAL_M_1)
  })()

  SMALL_GAMMA <- 1.5 # SMALL_GAMMA > 0

  h_0 <- function(x) {
    0
  }

  f_1 <- function(x) {
    # TODO remove stub
    0
  }

  # x - vector
  f_2 <- function(x) {
    0
  }

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
