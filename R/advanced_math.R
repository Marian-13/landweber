AdvancedMath <- module({
  import("magrittr")
  import("stats")

  use(.GlobalEnv, attach = TRUE)

  PI <- pi

  EULER_NUMBER <- exp(1)

  square_root <- function(number) {
    sqrt(number)
  }

  natural_logarithm <- function(number) {
    log(number)
  }

  modulus_of_vector <- function(vector) {
    vector %>%
    Reduce(function(sum, element) { sum + element^2 }, ., 0.0) %>%
    square_root()
  }

  # square_of_vector <- function(vector) {
  #   vector %*% vector
  # }

  solve_system_of_linear_equations <- function(matrix, vector) {
    solve(matrix, vector)
  }

  vector_with_zero_elements <- function(size) {
    rep(x = 0, times = size)
  }

  multiply_vector_by_vector <- function(first_vector, second_vector) {
    first_vector %*% second_vector
  }

  is_infinity <- function(value) {
    is.infinite(x = value)
  }

  # TODO
  calculate_integral <- function(lower_limit, upper_limit, integrand) {
    integrate(f = integrand, lower = lower_limit, upper = upper_limit)
  }
})
