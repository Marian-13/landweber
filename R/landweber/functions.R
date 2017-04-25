Functions <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  # Unoptimized
  weight_function_r <- function(capital_m, element_t_i, element_t_j) {
    sum_indices <- 1:(capital_m - 1)

    sum <- Helpers$reduce(
      initial = 0,
      vector  = sum_indices,
      func    = function(memo, sum_index) {
        memo + (1 / sum_index) * cos(sum_index * (element_t_i - element_t_j))
      }
    )

    1 %>%
    add(2 * sum) %>%
    add((1 / capital_m) * cos(capital_m * (element_t_i - element_t_j))) %>%
    multiply_by(-(1 / (2 * capital_m)))
  }

  # in (3.5) TODO Modulus
  kernel_h_1 <- function(vector_x_i, vector_derivative_of_x_i, vector_x_star_j) {
    first_addend <-
      vector_derivative_of_x_i %>%
      AdvancedMath$square_of_vector() %>% # TODO Modulus
      # AdvancedMath$modulus_of_vector() %>%
      multiply_by(AdvancedMath$EULER_NUMBER) %>%
      { AdvancedMath$natural_logarithm(1 / .) } %>%
      multiply_by(1 / 2)

    second_addend <-
      vector_x_i %>%
      subtract(vector_x_star_j) %>%
      AdvancedMath$modulus_of_vector() %>%
      AdvancedMath$natural_logarithm()

    first_addend + second_addend
  }

  green_function_n <- function(vector_x, vector_y, vector_y_star) {
    first_multiplicand <-
      vector_x %>%
      subtract(vector_y) %>%
      AdvancedMath$modulus_of_vector()

    second_multiplicand <-
      vector_x %>%
      subtract(vector_y_star) %>%
      AdvancedMath$modulus_of_vector()

    first_multiplicand %>%
    multiply_by(second_multiplicand) %>%
    { AdvancedMath$natural_logarithm(1 / .) }
  }

  # Unoptimized
  normal_nu <- function(vector_derivative_of_x) {
    modulus_of_derivative_of_x <-
      AdvancedMath$modulus_of_vector(vector_derivative_of_x)

    c(
      vector_derivative_of_x[1] / modulus_of_derivative_of_x,
      -vector_derivative_of_x[2] / modulus_of_derivative_of_x
    )
  }

  # TODO testpe
  # Unoptimized
  # Partial derivative of green function n with respect to normal_nu of x
  d_green_function_n_by_d_normal_nu_of_x <- function(vector_x, vector_y, green_function_n_of_x_and_y, normal_nu_of_x) {
    vector_x %>%
    subtract(vector_y) %>%
    AdvancedMath$multiply_vector_by_vector(normal_nu_of_x) %>%
    divide_by(green_function_n_of_x_and_y)
  }

  # (3.11)
  u <- function(x_infinity_i, capital_m, t, x, mu, alpha) {
    size <- length(t)
    indices <- 1:size

    first_sum <- 0

    for (j in indices) {
      x_j <- unlist(x[j])   # TODO unlist !!!
      first_sum <- first_sum + mu[j] * green_function_n(x_infinity_i, x_j)
    }

    (1 / (2 * capital_m)) * first_sum %>%
    # TODO second sum -- sinc quadrature for ln with e
    add(alpha)
  }

  # (3.10)
  partial_normal_derivative_of_v_by_x <- function(capital_m) {
    # TODO
  }
})
