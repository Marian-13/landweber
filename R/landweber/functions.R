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

  .kernel_h_1_second_addend <- function(vector_x_i, vector_x_star_j) {
    vector_x_i %>%
    subtract(vector_x_star_j) %>%
    AdvancedMath$modulus_of_vector() %>%
    AdvancedMath$natural_logarithm()
  }

  kernel_h_1_without_singularity <- function(vector_x_i, vector_derivative_of_x_i, vector_x_star_j) {
    first_addend <-
      vector_derivative_of_x_i %>%
      AdvancedMath$modulus_of_vector() %>% # AdvancedMath$square_of_vector() %>% # TODO Modulus
      raise_to_power(2) %>%                #
      multiply_by(AdvancedMath$EULER_NUMBER) %>%
      { AdvancedMath$natural_logarithm(1 / .) } %>%
      multiply_by(1 / 2)

    second_addend <- .kernel_h_1_second_addend(
      vector_x_i = vector_x_i,
      vector_x_star_j = vector_x_star_j
    )

    first_addend + second_addend
  }

  kernel_h_1_with_singularity <- function(element_t_i, element_t_j, vector_x_i, vector_x_j, vector_x_star_j) {
    first_addend <-
      vector_x_i %>%
      subtract(vector_x_j) %>%
      AdvancedMath$modulus_of_vector() %>%
      raise_to_power(2) %>%
      multiply_by(AdvancedMath$EULER_NUMBER) %>%
      { AdvancedMath$natural_logarithm((4 * (sin((element_t_i - element_t_j)))^2) / .) } %>%
      multiply_by(1 / 2)

    second_addend <- .kernel_h_1_second_addend(
      vector_x_i = vector_x_i,
      vector_x_star_j = vector_x_star_j
    )

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

  # Partial derivative of green function n with respect to normal_nu of x
  derivative_of_green_function_n <- function(vector_x, vector_y, vector_y_star,
                                             vector_derivative_of_x, vector_derivative_of_y) {
    -1 %>%
    multiply_by(
      AdvancedMath$modulus_of_vector(vector_x) %>%
      multiply_by(
        AdvancedMath$modulus_of_vector(vector_x - vector_y) %>%
        add(AdvancedMath$modulus_of_vector(vector_x - vector_y_star))
      ) %>%
      multiply_by(vector_derivative_of_x[2]) %>%
      divide_by(
        AdvancedMath$modulus_of_vector(vector_x - vector_y) %>%
        multiply_by(AdvancedMath$modulus_of_vector(vector_x - vector_y_star))
      ) %>%
      add(
        AdvancedMath$modulus_of_vector(vector_derivative_of_y) %>%
        multiply_by(vector_derivative_of_x[1]) %>%
        divide_by(
          AdvancedMath$modulus_of_vector(vector_x - vector_y) %>%
          multiply_by(AdvancedMath$modulus_of_vector(vector_derivative_of_x))
        )
      )
    )
  }

  kernel_h_3_with_singularity <- function(vector_x_i, vector_x_j) {
    1 / AdvancedMath$modulus_of_vector(vector_x_i - vector_x_j)
  }

  kernel_h_3_without_singularity <- function() {
    0
  }

  kernel_h_4_with_singularity <- function(vector_derivative_of_x_j, vector_x_i, vector_x_j) {
    AdvancedMath$modulus_of_vector(vector_derivative_of_x_j) %>%
    divide_by(AdvancedMath$modulus_of_vector(vector_x_i - vector_x_j))
  }

  kernel_h_4_without_singularity <- function(vector_second_derivative_of_x_i, vector_derivative_of_x_i) {
    -1 %>%
    multiply_by(
      AdvancedMath$modulus_of_vector(vector_second_derivative_of_x_i) %>%
      divide_by(AdvancedMath$modulus_of_vector(vector_derivative_of_x_i))
    )
  }
})
