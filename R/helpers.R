Helpers <- module({
  use(.GlobalEnv, attach = TRUE)

  calculate_size_of_vector <- function(vector) {
    length(vector)
  }

  # TODO Comment
  generate_matrix_from_vector <- function(vector, row_size, column_size, func) {
    matrix <- matrix(nrow = row_size, ncol = column_size, byrow = TRUE)

    index <- 0

    for (element in vector) {
      index <- index + 1
      matrix[index, ] <- func(element)
    }

    matrix
  }
})
