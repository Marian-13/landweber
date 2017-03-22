EulerNumber <- function() {
  exp(1)
}

NaturalLogarithm <- function(number) {
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

ModulusOfVector <- function(vector) {
  vector %>%
  Reduce(function(sum, element) { sum + element^2 }, ., 0.0) %>%
  sqrt
}

SquareOfVector <- function(vector) {
  vector %*% vector
}
