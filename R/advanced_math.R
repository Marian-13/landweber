AdvancedMath <- module({
  import("magrittr")

  EULER_NUMBER <- exp(1)

  square_root <- function(number) {
    sqrt(number)
  }

  natural_logarithm <- function(number) {
    log(number)
  }

  zero_length_vector <- function() {
    vector(length = 0)
  }

  modulus_of_vector <- function(vector) {
    vector %>%
    Reduce(function(sum, element) { sum + element^2 }, ., 0.0) %>%
    square_root()
  }

  square_of_vector <- function(vector) {
    vector %*% vector
  }

  solve_sle <- function(matrix, vector) {
    solve(matrix, vector)
  }


  # TODO Used?
  dot_product <- function(first_vector, second_vector) {
    first_vector %*% second_vector
  }

  # TODO Used?
  vector_with_zero_elements <- function(size) {
    rep(x = 0, times = size)
  }
})
