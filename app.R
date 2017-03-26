# Pipe operator in R
library("magrittr")
library("modules")

# Order is important!
source("R/printers.R")
source("R/advanced_math.R")
source("R/example_specific_functions/example_1.R")
# source("R/example_specific_functions/example_2.R")
# source("R/example_specific_functions/test_example.R")
source("R/functions.R")
source("R/mesh.R")
source("R/discretized_system.R")
source("R/iterative_procudure.R")

capital_m <- 2
result <- IterativeProcedure$start(capital_m)

p(result)


# Create the data for the chart.
# v <- c(7,12,28,3,41)
# t <- c(14,7,6,19,3)
#
# # Give the chart file a name.
# png(file = "line_chart_2_lines.jpg")
#
# # Plot the bar chart.
# plot(v,type = "o",col = "red", xlab = "Month", ylab = "Rain fall",
#    main = "Rain fall chart")
#
# lines(t, type = "o", col = "blue")
#
# # Save the file.
# dev.off()
