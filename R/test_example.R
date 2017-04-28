test_example <- list()

test_example$CAPITAL_M <- 2

test_example$LOWER_LIMIT_OF_GAMMA_0 <- 0
test_example$UPPER_LIMIT_OF_GAMMA_0 <- 2 * pi

test_example$function_x_1 <- function(t) { 1.5 * cos(t) }
test_example$function_x_2 <- function(t) { sin(t) + 1.5 }

test_example$function_derivative_of_x_1 <- function(t) { -1.5 * sin(t) }
test_example$function_derivative_of_x_2 <- function(t) { cos(t) }

test_example$function_second_derivative_of_x_1 <- function(t) { -1.5 * cos(t) }
test_example$function_second_derivative_of_x_2 <- function(t) { -sin(t) }

test_example$CAPITAL_M_1 <- 1
test_example$H_INFINITY <- (function() {
  c <- 1
  c / sqrt(test_example$CAPITAL_M_1)
})()

test_example$function_h_0 <- function(x) { 0 }
test_example$function_f_1 <- function(x) { 10 }
test_example$function_f_2 <- function(x) { 0 }
