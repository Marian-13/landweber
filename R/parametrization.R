Parametrization <- module({
  import("magrittr")
  use(.GlobalEnv$ExampleSpecificFunctions, attach = TRUE)
  use(.GlobalEnv$AdvancedMath, attach = TRUE)

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

  # OldH1 <- function(t, tau) {
  #   (1 / 2) * log(1 / (exp(1) * AdvancedMath$SquareOfVector(DerivativeOfX(t)))) +
  #     log(AdvancedMath$ModulusOfVector(X(t) - XStar(tau)))
  # }

  H1 <- function(t, tau) {
    firstAddend <-
      DerivativeOfX(t) %>%
      SquareOfVector() %>%
      multiply_by(kEulerNumber) %>%
      { NaturalLogarithm(1 / .) } %>%
      multiply_by(1 / 2)

    secondAddend <-
      X(t) %>%
      subtract(XStar(tau)) %>%
      ModulusOfVector() %>%
      NaturalLogarithm()

    firstAddend + secondAddend
  }
})
