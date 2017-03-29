# Pipe operator in R
library("magrittr")
library("modules")

# Order is important!
source("R/printers.R")
source("R/helpers.R")
source("R/advanced_math.R")
source("R/landweber.R")

# Test Example
capital_m <- 5

f_1 <- function(x) {
  10
}

f_2 <- function(x) {
  0
}

h_0 <- function(x) {
  0
}

# Landweber
result = IterativeProcedure$start(
  capital_m = capital_m,
  function_f_1 = f_1,
  function_f_2 = f_2,
  function_h_0 = h_0
)

p(result)
