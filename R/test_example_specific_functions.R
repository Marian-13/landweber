test_example_specific_functions <- list()

test_example_specific_functions$f_1 <- function(x) {
  10
}

test_example_specific_functions$f_2 <- function(x) {
  0
}

test_example_specific_functions$h_0 <- function(x) {
  0
}

test_example_specific_functions$x_1 <- function(t) {
  1.5 * cos(t)
}

test_example_specific_functions$x_2 <- function(t) {
  sin(t) + 1.5
}

test_example_specific_functions$CAPITAL_M_1 <- 1

test_example_specific_functions$H_INFINITY <- (function() {
  c <- 1
  c / AdvancedMath$square_root(test_example_specific_functions$CAPITAL_M_1)
})()

test_example_specific_functions$derivative_of_x_1 <- function(t) {
  -1.5 * sin(t)
}

test_example_specific_functions$derivative_of_x_2 <- function(t) {
  cos(t)
}
