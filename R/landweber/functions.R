Functions <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  get_lower_limit_of_t <- function() {
    0
  }

  get_upper_limit_of_t <- function() {
    2 * AdvancedMath$PI
  }

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

  # in (3.5)
  kernel_h_1 <- function(vector_derivative_of_x_i, vector_x_i, vector_x_star_j) {
    first_addend <-
      vector_derivative_of_x_i %>%
      AdvancedMath$square_of_vector() %>% # TODO Modulus
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

  green_function_n <- function(x, y) {
    first_multiplicand <-
      x %>%
      subtract(y) %>%
      AdvancedMath$modulus_of_vector()

    second_multiplicand <-
      x %>%
      subtract(convert_to_x_star(y)) %>%
      AdvancedMath$modulus_of_vector()

    first_multiplicand %>%
    multiply_by(second_multiplicand) %>%
    { AdvancedMath$natural_logarithm(1 / .) }
  }


  # TODO green function out of f
  # x_j, h_j - numbers, f - function
  w_tilde <- function(x_j, h_j, function_f) {
    sum_indices <- (-capital_m_1()):capital_m_1()

    sum <- Reduce(
      function(memo, index) {
        t <- index * h_infinity()
        memo + function_f(x_infinity(t)) * green_function_n(x_j, x_infinity(t))
      },
      sum_indices,
      0
    )

    h_j - h_infinity() * sum
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
