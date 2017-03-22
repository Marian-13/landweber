Discretization <- module({
  # m := M

  # OldNodeT <- function(i, m) {
  #   # R's / operator always returns floating point number
  #   (i * pi) / m
  # }
  #
  # OldEquidistantMeshNodes <- function(m) {
  #   indices <- 0:(2 * m - 1)
  #
  #   result <- vector(length = 0)
  #
  #   for (i in indices) {
  #     result <- c(result, NodeT(i, m))
  #   }
  #
  #   result
  # }

  NodeT <- function(i, m) {
    LowerLimitOfT() + (i * pi) / m
  }

  FirstNode <- function(m) {
    NodeT(0, m)
  }

  LastNode <- function(m) {
    NodeT(2 * m - 1, m)
  }

  Step <- function(m) {
    pi / m
  }

  EquidistantMeshNodes <- function(m) {
    seq(from = FirstNode(m), to = LastNode(m), by = Step(m))
  }
})
