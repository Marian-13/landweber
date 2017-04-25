Helpers <- module({
  use(.GlobalEnv, attach = TRUE)

  calculate_size_of_vector <- function(vector) {
    length(vector)
  }

  generate_vector <- function(size) {
    vector(length = size)
  }

  generate_vector_with_equal_elements <- function(size, element) {
    rep(x = element, times = size)
  }

  generate_vector_by_function <- function(size, func) {
    result <- generate_vector(size)

    indices <- 1:size

    for (index in indices) {
      result[index] <- func(index)
    }

    result
  }

  # TODO Comment
  generate_vector_from_vector <- function(vector, size, func) {
    result <- generate_vector(size)

    indices <- 1:size # in "for (row in matrix)" row is element!

    for (index in indices) {
      result[index] <- func(vector[index])
    }

    result
  }

  # TODO Comment
  generate_vector_from_matrix <- function(matrix, size, func) {
    vector <- generate_vector(size)

    indices <- 1:size # in "for (row in matrix)" row is element!

    for (index in indices) {
      vector[index] <- func(matrix[index, ])
    }

    vector
  }

  generate_matrix <- function(row_size, column_size) {
    matrix(nrow = row_size, ncol = column_size, byrow = TRUE)
  }

  generate_matrix_from_elements <- function(elements, row_size, column_size) {
    matrix(data = elements, nrow = row_size, ncol = column_size, byrow = TRUE)
  }

  generate_matrix_by_function <- function(row_size, column_size, func) {
    result <- generate_matrix(row_size, column_size)

    row_indices    <- 1:row_size
    column_indices <- 1:column_size

    for (i in row_indices) {
      for (j in column_indices) {
        result[i, j] <- func(i, j)
      }
    }

    result
  }

  # TODO Comment
  generate_matrix_from_vector <- function(vector, row_size, column_size, func) {
    matrix <- generate_matrix(row_size, column_size)

    index <- 0

    for (element in vector) {
      index <- index + 1
      matrix[index, ] <- func(element)
    }

    matrix
  }

  # TODO Comment
  generate_matrix_from_matrix <- function(matrix, row_size, column_size, func) {
    result <- generate_matrix(row_size, column_size)

    indices <- 1:row_size # in "for (row in matrix)" row is element!

    for (index in indices) {
      result[index, ] <- func(matrix[index, ])
    }

    result
  }

  generate_square_matrix <- function(size) {
    generate_matrix(size, size)
  }

  generate_square_matrix_from_elements <- function(elements, size) {
    generate_matrix_from_elements(elements, size, size)
  }

  generate_square_matrix_by_function <- function(size, func) {
    generate_matrix_by_function(size, size, func)
  }

  reduce <- function(initial, vector, func) {
    Reduce(f = func, x = vector, init = initial)
  }
})
