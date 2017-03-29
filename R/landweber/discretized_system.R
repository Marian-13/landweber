source("R/landweber/discretized_system/discretized_matrix.R")
source("R/landweber/discretized_system/discretized_vector.R")

# (3.9)
DiscretizedSystem <- module({
  use(.GlobalEnv, attach = TRUE)

  discretized_matrix <- function(capital_m, t) {
    DiscretizedMatrix$construct_matrix(capital_m, t)
  }

  discretized_vector <- function(capital_m, x, h, function_f) {
    DiscretizedVector$construct_vector(capital_m, x, h, function_f)
  }
})
