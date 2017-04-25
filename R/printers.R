p <- function(value = "No value passed to p()", message = "") {
  cat(message)
  print(value)
}

puts <- function(value = "No value passed to puts()", message = "") {
  p(value, message)
}
