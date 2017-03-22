AdvancedMath <- module({
  import("magrittr")

  EulerNumber <- exp(1)

  natural_logarithm <- function(number) {
    log(number)
  }

  # OldModulusOfVector <- function(vector) {
  #   sqrt(
  #     Reduce(
  #       function(sum, element) {
  #         sum + element^2
  #       },
  #       vector,
  #       0.0
  #     )
  #   )
  # }

  modulus_of_vector <- function(vector) {
    vector %>%
    Reduce(function(sum, element) { sum + element^2 }, ., 0.0) %>%
    sqrt
  }

  square_of_vector <- function(vector) {
    vector %*% vector
  }
})
