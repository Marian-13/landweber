Helpers <- module({
  use(.GlobalEnv, attach = TRUE)

  # map, which returns list of arbitrary objects
  # Default Map always returns list of lists
  map <- function(array, block) {
    map_result <- list()
    size <- length(array)
    indices <- 1:size

    j <- 0

    for (i in indices) {
      block_result <- block(array[i])

      if (is.null(block_result)) {
        next
      } else {
        j <- j + 1
        map_result[j] <- block_result
      }
    }

    map_result
  }





})
