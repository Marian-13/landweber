library("magrittr") # Pipe operator in R
library("modules")

# Order is important!
source("R/printers.R")
source("R/helpers.R")
source("R/advanced_math.R")
source("R/landweber.R")
source("R/test_example.R")

# Landweber
result <- IterativeProcedure$start(example = test_example)

p(result)

# p(warnings())
