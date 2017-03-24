Discretization <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  t <- function(capital_m) {
    Mesh$equidistant_mesh_nodes(capital_m)
  }

  # near (3.7)
  weight_function_r <- function(capital_m, t, t_j) {
    indices <- 1:(capital_m - 1)

    sum <- Reduce(
      function(memo, index) {
        memo + (1 / index) * cos(index * (t - t_j))
      },
      indices,
      0.0
    )

    1 %>%
    add(2 * sum) %>%
    add((1 / capital_m) * cos(capital_m * (t - t_j))) %>%
    multiply_by(-(1 / (2 * capital_m)))
  }

  # in (3.5)
  kernel_h_1 <- function(t, tau) {
    first_addend <-
      Parametrization$derivative_of_x(t) %>%
      AdvancedMath$square_of_vector() %>%
      multiply_by(AdvancedMath$EULER_NUMBER) %>%
      { AdvancedMath$natural_logarithm(1 / .) } %>%
      multiply_by(1 / 2)

    second_addend <-
      Parametrization$x(t) %>%
      subtract(Parametrization$x_star(tau)) %>%
      AdvancedMath$modulus_of_vector() %>%
      AdvancedMath$natural_logarithm()

    first_addend + second_addend
  }
})
