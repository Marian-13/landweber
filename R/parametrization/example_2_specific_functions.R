X1 <- function(t) {
  RadialFunctionR(t) * cos(t)
}

X2 <- function(t) {
  RadialFunctionR(t) * sin(t) + 1.5
}

DerivativeOfX1 <- function(t) {
  DerivativeOfRadialFunctionR(t) * cos(t) - RadialFunctionR(t) * sin(t)
}

DerivativeOfX2 <- function(t) {
  DerivativeOfRadialFunctionR(t) * sin(t) + RadialFunctionR(t) * cos(t)
}

RadialFunctionR <- function(t) {
  sqrt(cos(t)^2 + 0.25 * sin(t)^2)
}

DerivativeOfRadialFunctionR <- function(t) {
  (-0.75 * sin(t) * cos(t)) / (sqrt(cos(t)^2 + 0.25 * sin(t)^2))
}
