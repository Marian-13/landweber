# Pipe operator in R
library("magrittr")
library("modules")

# Order is important!
source("R/advanced_math.R")
source("R/example_specific_functions/example_1.R")
# source("R/example_specific_functions/example_2.R")
# source("R/example_specific_functions/test_example.R")
source("R/functions.R")
source("R/mesh.R")
source("R/discretized_system/discretized_matrix.R")
source("R/discretized_system/discretized_vector.R")
source("R/discretized_system.R")
source("R/iterative_procudure.R")

capital_m <- 2
IterativeProcedure$start(capital_m)

# print(Functions$w_tilde(x_j = 0, h_j = 0, f = ExampleSpecificFunctions$f_2))
