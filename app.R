# Pipe operator in R
library("magrittr")
library("modules")

# Order is important!
source("R/printers.R")
source("R/helpers.R")
source("R/advanced_math.R")
source("R/example_specific_functions/example_1_specific_functions.R") # TODO Remove
source("R/landweber.R")
source("R/test_example.R")

# Landweber
result <- IterativeProcedure$start(example = test_example)

# p(result)

# warnings()
