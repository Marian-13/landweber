DiscretizedSystem <- module({
  use(.GlobalEnv, attach = TRUE)

  dicretized_matrix <- function(capital_m, t) {
    DiscretizedMatrix$construct_matrix(capital_m, t)
  }
})
