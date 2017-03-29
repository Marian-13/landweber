Mesh <- module({
  generate_equidistant_nodes <- function(capital_m, first_node, last_node, step) {
    seq(from = first_node, to = last_node, by = step)
  }
})
