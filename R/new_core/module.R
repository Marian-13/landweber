# create_module <- function(module_body) {
#   this <- list()
#
#   this$define_method <- function(name, body) {
#     method <- list(body)
#     names(method) <- name
#
#     this <<- c(this, method) # TODO return method
#   }
#
#   this$define_constant <- function(name, value) {
#     this$define_method(name, (function() { value })())
#   }
#
#   module_body(this)
#
#   this
# }

# Example of new module
# TODO visible methods
FirstModule <- create_module(function(self) {
  self$define_method("method_1", function() {
    "Hello from method 1"
  })

  self$define_method("method_2", function() {
    "Method in 2 is here"
  })

  self$define_method("method_3", function() {
    self$method_2() # is not visible here!x
  })
})
#
FirstModule$method_1()
FirstModule$method_2()
FirstModule$method_3()
