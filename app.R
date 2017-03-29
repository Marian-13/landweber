# Pipe operator in R
library("magrittr")
library("modules")

# Order is important!
source("R/printers.R")
source("R/helpers.R")
source("R/advanced_math.R")
source("R/example_specific_functions/example_1_specific_functions.R") # TODO Remove
source("R/landweber.R")
source("R/test_example_specific_functions.R")

# Test Example
capital_m <- 2
example_specific_functions <- test_example_specific_functions

# Landweber
result <- IterativeProcedure$start(
  capital_m                  = capital_m,
  example_specific_functions = example_specific_functions
)

p(result)
