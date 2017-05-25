Functions <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  weight_function_r <- function(capital_m, element_t_i, element_t_j) {
    sum_indices <- 1:(capital_m - 1)

    sum <- Helpers$reduce(
      initial = 0,
      vector  = sum_indices,
      func    = function(memo, sum_index) {
        memo + (1 / sum_index) * cos(sum_index * (element_t_i - element_t_j))
      }
    )

    -(1 / (2 * capital_m)) %>%
    multiply_by(
      1 %>%
      add(2 * sum) %>%
      add((1 / capital_m) * cos(capital_m * (element_t_i - element_t_j)))
    )
  }

  kernel_h_1_without_singularity <- function(vector_x_i, vector_derivative_of_x_i, vector_x_star_j) {
    (1 / 2) %>%
    multiply_by(
      AdvancedMath$natural_logarithm(
        1 %>%
        divide_by(
          AdvancedMath$EULER_NUMBER %>%
          multiply_by(
            AdvancedMath$modulus_of_vector(vector_derivative_of_x_i) %>%
            raise_to_power(2)
          )
        )
      )
    ) %>%
    subtract(
      AdvancedMath$natural_logarithm(
        AdvancedMath$modulus_of_vector(vector_x_i - vector_x_star_j)
      )
    )
  }

  kernel_h_1_with_singularity <- function(element_t_i, element_t_j, vector_x_i, vector_x_j, vector_x_star_j) {
    (1 / 2) %>%
    multiply_by(
      AdvancedMath$natural_logarithm(
        4 %>%
        multiply_by(
          sin((element_t_i - element_t_j) / 2) %>%
          raise_to_power(2)
        ) %>%
        divide_by(
          AdvancedMath$EULER_NUMBER %>%
          multiply_by(
            AdvancedMath$modulus_of_vector(vector_x_i - vector_x_j) %>%
            raise_to_power(2)
          )
        )
      )
    ) %>%
    subtract(
      AdvancedMath$natural_logarithm(
        AdvancedMath$modulus_of_vector(vector_x_i - vector_x_star_j)
      )
    )
  }

  green_function_n <- function(vector_x, vector_y, vector_y_star) {
    AdvancedMath$natural_logarithm(
      1 %>%
      divide_by(
        AdvancedMath$modulus_of_vector(vector_x - vector_y) %>%
        multiply_by(AdvancedMath$modulus_of_vector(vector_x - vector_y_star))
      )
    )
  }

  kernel_h_2_with_singularity <- function(vector_x_infinity_i, vector_x_infinity_j) {
    AdvancedMath$natural_logarithm(
      1 %>%
      divide_by(
        AdvancedMath$modulus_of_vector(
          vector_x_infinity_i - vector_x_infinity_j
        )
      )
    )
  }

  kernel_h_2_without_singularity <- function(vector_derivative_of_x_infinity_j) {
    AdvancedMath$natural_logarithm(
      1 / AdvancedMath$modulus_of_vector(vector_derivative_of_x_infinity_j)
    )
  }

  kernel_k_with_singularity <- function(vector_x_i, vector_x_j, vector_derivative_of_x_i) {
    -1 %>%
    multiply_by(
      vector_x_i[1] %>%
      subtract(vector_x_j[1])
    ) %>%
    divide_by(
      AdvancedMath$modulus_of_vector(
        vector_x_i %>%
        subtract(vector_x_j)
      ) %>%
      raise_to_power(2)
    ) %>%
    multiply_by(vector_derivative_of_x_i[2]) %>%
    divide_by(AdvancedMath$modulus_of_vector(vector_derivative_of_x_i)) %>%
    add(
      vector_x_i[2] %>%
      subtract(vector_x_j[2]) %>%
      divide_by(
        AdvancedMath$modulus_of_vector(
          vector_x_i %>%
          subtract(vector_x_j)
        ) %>%
        raise_to_power(2)
      ) %>%
      multiply_by(vector_derivative_of_x_i[1]) %>%
      divide_by(AdvancedMath$modulus_of_vector(vector_derivative_of_x_i))
    )
  }

  kernel_k_without_singularity <- function(vector_derivative_of_x_i, vector_second_derivative_of_x_i) {
    vector_second_derivative_of_x_i[1] %>%
    divide_by(
      2 %>%
      multiply_by(
        AdvancedMath$modulus_of_vector(vector_derivative_of_x_i) %>%
        raise_to_power(2)
      )
    ) %>%
    multiply_by(vector_derivative_of_x_i[2]) %>%
    divide_by(AdvancedMath$modulus_of_vector(vector_derivative_of_x_i)) %>%
    subtract(
      vector_second_derivative_of_x_i[2] %>%
      divide_by(
        2 %>%
        multiply_by(
          AdvancedMath$modulus_of_vector(vector_derivative_of_x_i) %>%
          raise_to_power(2)
        )
      ) %>%
      multiply_by(vector_derivative_of_x_i[1]) %>%
      divide_by(AdvancedMath$modulus_of_vector(vector_derivative_of_x_i))
    )
  }
})
