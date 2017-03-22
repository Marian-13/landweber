# T:= t

LowerLimitOfT <- function() {
  0.0
}

UpperLimitOfT <- function() {
  2 * pi
}

X <- function(t) {
  c(X1(t), X2(t))
}

DerivativeOfX <- function(t) {
  c(DerivativeOfX1(t), DerivativeOfX2(t))
}

XStar <- function(t) {
  c(X1(t), -X2(t))
}

XInfinity <- function(t) {
  c(t, 0)
}

H1 <- function(t, tau) {
  # TODO refactor
  (1 / 2) * log(1 / (exp(1) * SquareOfVector(DerivativeOfX(t)))) + log(ModulusOfVector(X(t) - XStar(tau)))
}
