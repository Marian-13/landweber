AdvancedMath <- module({
  import("magrittr")

  EULER_NUMBER <- exp(1)

  zero_length_vector <- function() {
    vector(length = 0)
  }

  natural_logarithm <- function(number) {
    log(number)
  }

  modulus_of_vector <- function(vector) {
    vector %>%
    Reduce(function(sum, element) { sum + element^2 }, ., 0.0) %>%
    sqrt
  }

  square_of_vector <- function(vector) {
    vector %*% vector
  }

  dot_product <- function(first_vector, second_vector) {
    first_vector %*% second_vector
  }

  # TODO IMPEMENT
  vector_with_zero_elements <- function(size) {
    rep(x = 0, times = size)
  }
})
