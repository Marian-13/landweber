ExampleSpecificFunctions <- module({
  use(.GlobalEnv, attach = TRUE)

  CAPITAL_M_1 <- (function() { 1 })()

  H_INFINITY <- (function() {
    c <- 1
    c / AdvancedMath$square_root(CAPITAL_M_1)
  })()

  h_0 <- function(x) {
    0
  }

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

  # # TODO Constants
  # m <- function() {
  #   64
  # }
  #
  # gamma <- function() {
  #   1.5
  # }
  #
  # h_0 <- function(t) {
  #   0
  # }
  #
  # f_2 <- function(t) {
  #
  # }
})
