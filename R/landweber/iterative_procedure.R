IterativeProcedure <- module({
  import("magrittr")

  use(.GlobalEnv, attach = TRUE)

  start <- function(example) {
    # Step 0:
    # Constants, vectors, matrices, functions that will never be modified by
    # iterative procedure
    capital_m   <- example$CAPITAL_M
    capital_m_1 <- example$CAPITAL_M_1
    h_infinity  <- example$H_INFINITY
    small_gamma <- example$SMALL_GAMMA

    vector_t <- .form_vector_t(
      capital_m              = capital_m,
      lower_limit_of_gamma_0 = example$LOWER_LIMIT_OF_GAMMA_0,
      upper_limit_of_gamma_0 = example$UPPER_LIMIT_OF_GAMMA_0
    )
    size_of_vector_t <- .calculate_size_of_vector_t(vector_t = vector_t)

    size_of_vector_q <- .calculate_size_of_vector_q(capital_m_1 = capital_m_1)
    vector_q <- .form_vector_q(
      capital_m_1      = capital_m_1,
      size_of_vector_q = size_of_vector_q,
      h_infinity       = h_infinity
    )

    function_x <- .form_function_x(
      function_x_1 = example$function_x_1,
      function_x_2 = example$function_x_2
    )
    matrix_x <- .form_matrix_x(
      vector_t         = vector_t,
      size_of_vector_t = size_of_vector_t,
      function_x       = function_x
    )
    matrix_x_star <- .form_matrix_x_star(
      size_of_vector_t = size_of_vector_t,
      matrix_x         = matrix_x
    )

    function_derivative_of_x <- .form_function_derivative_of_x(
      function_derivative_of_x_1 = example$function_derivative_of_x_1,
      function_derivative_of_x_2 = example$function_derivative_of_x_2
    )
    matrix_derivative_of_x <- .form_matrix_derivative_of_x(
      vector_t                 = vector_t,
      size_of_vector_t         = size_of_vector_t,
      function_derivative_of_x = function_derivative_of_x
    )

    function_second_derivative_of_x <- .form_function_second_derivative_of_x(
      function_second_derivative_of_x_1 = example$function_second_derivative_of_x_1,
      function_second_derivative_of_x_2 = example$function_second_derivative_of_x_2
    )
    matrix_second_derivative_of_x <- .form_matrix_second_derivative_of_x(
      vector_t                        = vector_t,
      size_of_vector_t                = size_of_vector_t,
      function_second_derivative_of_x = function_second_derivative_of_x
    )

    function_x_infinity <- .form_function_x_infinity()
    matrix_x_infinity <- .form_matrix_x_infinity(
      size_of_vector_q = size_of_vector_q,
      vector_q         = vector_q
    )
    matrix_x_infinity_star <- .form_matrix_x_infinity_star(
      size_of_vector_q  = size_of_vector_q,
      matrix_x_infinity = matrix_x_infinity
    )
    matrix_derivative_of_x_infinity <- .form_matrix_derivative_of_x_infinity(
      size_of_vector_q = size_of_vector_q
    )

    function_f_2 <- example$function_f_2
    vector_f_2 <- .form_vector_f_2(
      size_of_vector_q = size_of_vector_q,
      vector_q         = vector_q,
      function_f_2     = function_f_2
    )

    function_f_1 <- example$function_f_1
    vector_f_1 <- .form_vector_f_1(
      size_of_vector_q = size_of_vector_q,
      vector_q         = vector_q,
      function_f_1     = function_f_1
    )

    # First integral of first equation in 3.5
    matrix_r <- .form_matrix_r(
      capital_m        = capital_m,
      size_of_vector_t = size_of_vector_t,
      vector_t         = vector_t
    )
    matrix_h_1 <- .form_matrix_h_1(
      size_of_vector_t       = size_of_vector_t,
      vector_t               = vector_t,
      matrix_derivative_of_x = matrix_derivative_of_x,
      matrix_x               = matrix_x,
      matrix_x_star          = matrix_x_star
    )

    # System 3.5
    size_of_discretized_system <- .calculate_size_of_discretized_system(
      capital_m = capital_m
    )
    discretized_matrix <- .form_discretized_matrix(
      capital_m                  = capital_m,
      size_of_vector_t           = size_of_vector_t,
      size_of_discretized_system = size_of_discretized_system,
      matrix_r                   = matrix_r,
      matrix_h_1                 = matrix_h_1
    )

    # Right part of in first equation 3.5
    matrix_n_1_q <- .form_matrix_n_1_q(
      size_of_vector_t       = size_of_vector_t,
      size_of_vector_q       = size_of_vector_q,
      matrix_x               = matrix_x,
      matrix_x_infinity      = matrix_x_infinity,
      matrix_x_infinity_star = matrix_x_infinity_star
    )

    # System 3.5 for u
    function_h_0 <- example$function_h_0
    vector_h_0 <- .form_vector_h_0(
      size_of_vector_t = size_of_vector_t,
      matrix_x         = matrix_x,
      function_h_0     = function_h_0
    )
    vector_of_sums_from_w_tilde_for_u <- .form_vector_of_sums_from_w_tilde(
      size_of_vector_t = size_of_vector_t,
      size_of_vector_q = size_of_vector_q,
      vector_f         = vector_f_2,
      matrix_n_1_q     = matrix_n_1_q
    )

    # First integral of equation 3.11
    matrix_n_t_2 <- .form_matrix_n_t_2(
      size_of_vector_t  = size_of_vector_t,
      size_of_vector_q  = size_of_vector_q,
      matrix_x_infinity = matrix_x_infinity,
      matrix_x          = matrix_x,
      matrix_x_star     = matrix_x_star
    )

    # System 3.5 for derivative of v
    vector_with_zero_elements <- .form_vector_with_zero_elements(
      size_of_vector_t = size_of_vector_t
    )
    matrix_h_3 <- .form_matrix_h_3(
      size_of_vector_t = size_of_vector_t,
      matrix_x         = matrix_x
    )
    matrix_h_4 <- .form_matrix_h_4(
      size_of_vector_t              = size_of_vector_t,
      matrix_x                      = matrix_x,
      matrix_derivative_of_x        = matrix_derivative_of_x,
      matrix_second_derivative_of_x = matrix_second_derivative_of_x
    )

    # TODO
    p(vector_with_zero_elements, "vector_with_zero_elements:\n")
    p(matrix_h_3, "matrix_h_3:\n")
    p(matrix_h_4, "matrix_h_4:\n")


    # Equatin 3.10 -- derivative v
    matrix_derivative_of_n_1_q <- .form_matrix_derivative_of_n_1_q(
      size_of_vector_t                = size_of_vector_t,
      size_of_vector_q                = size_of_vector_q,
      matrix_x                        = matrix_x,
      matrix_x_infinity               = matrix_x_infinity,
      matrix_x_infinity_star          = matrix_x_infinity_star,
      matrix_derivative_of_x          = matrix_derivative_of_x,
      matrix_derivative_of_x_infinity = matrix_derivative_of_x_infinity
    )

    # Step 1:
    #
    #
    vector_u_0 <- (function() {
      vector_w_tilde <- .form_vector_w_tilde(
        size_of_vector_t            = size_of_vector_t,
        vector_h                    = vector_h_0,
        h_infinity                  = h_infinity,
        vector_of_sums_from_w_tilde = vector_of_sums_from_w_tilde_for_u
      )

      discretized_right_hand_side <- .form_discretized_right_hand_side(
        size_of_vector_t = size_of_vector_t,
        vector_w_tilde   = vector_w_tilde
      )

      discretized_system_solution <- .solve_discretized_system(
        discretized_matrix          = discretized_matrix,
        discretized_right_hand_side = discretized_right_hand_side
      )

      # Step 2
      #
      #
      vector_mu <- .extract_vector_mu(
        size_of_vector_t            = size_of_vector_t,
        discretized_system_solution = discretized_system_solution
      )

      alpha <- .extract_constant_alpha(
        size_of_vector_t            = size_of_vector_t,
        discretized_system_solution = discretized_system_solution
      )

      vector_of_first_sums_from_u <- .form_vector_of_first_sums_from_u(
        size_of_vector_t = size_of_vector_t,
        size_of_vector_q = size_of_vector_q,
        vector_mu        = vector_mu,
        matrix_n_t_2     = matrix_n_t_2
      )

      # TODO {
      matrix_n_t_q <- .form_matrix_n_t_q(
        size_of_vector_q       = size_of_vector_q,
        matrix_x_infinity      = matrix_x_infinity,
        matrix_x_infinity_star = matrix_x_infinity_star
      )

      vector_of_second_sums_from_u <- .form_vector_of_second_sums_from_u(
        size_of_vector_q = size_of_vector_q,
        vector_f         = vector_f_2,
        matrix_n_t_q     = matrix_n_t_q
      )
      # TODO }

      vector_u_0 <- .form_vector_u(
        size_of_vector_q             = size_of_vector_q,
        capital_m                    = capital_m,
        h_infinity                   = h_infinity,
        vector_of_first_sums_from_u  = vector_of_first_sums_from_u,
        vector_of_second_sums_from_u = vector_of_second_sums_from_u
      )
    })()

    vector_derivative_of_v_0 <- (function() {
      vector_f <- vector_u_0 - vector_f_1

      vector_of_sums_from_w_tilde_for_derivative_of_v <- .form_vector_of_sums_from_w_tilde(
        size_of_vector_t = size_of_vector_t,
        size_of_vector_q = size_of_vector_q,
        vector_f         = vector_f,
        matrix_n_1_q     = matrix_n_1_q
      )

      vector_w_tilde <- .form_vector_w_tilde(
        size_of_vector_t            = size_of_vector_t,
        vector_h                    = vector_with_zero_elements,
        h_infinity                  = h_infinity,
        vector_of_sums_from_w_tilde = vector_of_sums_from_w_tilde_for_derivative_of_v
      )

      discretized_right_hand_side <- .form_discretized_right_hand_side(
        size_of_vector_t = size_of_vector_t,
        vector_w_tilde   = vector_w_tilde
      )

      discretized_system_solution <- .solve_discretized_system(
        discretized_matrix          = discretized_matrix,
        discretized_right_hand_side = discretized_right_hand_side
      )

      vector_mu <- .extract_vector_mu(
        size_of_vector_t            = size_of_vector_t,
        discretized_system_solution = discretized_system_solution
      )

      alpha <- .extract_constant_alpha(
        size_of_vector_t            = size_of_vector_t,
        discretized_system_solution = discretized_system_solution
      )

      vector_of_first_sums_from_derivative_of_v <- .form_vector_of_first_sums_from_derivative_of_v(
        size_of_vector_t       = size_of_vector_t,
        vector_mu              = vector_mu,
        matrix_h_3             = matrix_h_3,
        matrix_h_4             = matrix_h_4,
        matrix_derivative_of_x = matrix_derivative_of_x,
        matrix_x               = matrix_x,
        matrix_x_star          = matrix_x_star
      )

      vector_of_second_sums_from_derivative_of_v <- .form_vector_of_second_sums_from_derivative_of_v(
        size_of_vector_t           = size_of_vector_t,
        size_of_vector_q           = size_of_vector_q,
        vector_f                   = vector_f,
        matrix_derivative_of_n_1_q = matrix_derivative_of_n_1_q
      )

      vector_derivative_of_v_0 <- .form_vector_derivative_of_v(
        size_of_vector_t                           = size_of_vector_t,
        h_infinity                                 = h_infinity,
        vector_of_first_sums_from_derivative_of_v  = vector_of_first_sums_from_derivative_of_v,
        vector_of_second_sums_from_derivative_of_v = vector_of_second_sums_from_derivative_of_v,
        matrix_derivative_of_x                     = matrix_derivative_of_x,
        vector_mu                                  = vector_mu
      )
    })()

    # Step 3
    #
    #
    k <- 0

    previous_vector_h               <- vector_h_0
    previous_vector_u               <- vector_u_0
    previous_vector_derivative_of_v <- vector_derivative_of_v_0
    current_vector_h                <- NULL
    current_vector_u                <- NULL
    current_vector_derivative_of_v  <- NULL

    repeat {
      k <- k + 1

      current_vector_h <- .calculate_current_vector_h(
        previous_vector_h               = previous_vector_h,
        small_gamma                     = small_gamma,
        previous_vector_derivative_of_v = previous_vector_derivative_of_v
      )

      # Step 4
      #
      #
      current_vector_u <- (function() {
        vector_w_tilde <- .form_vector_w_tilde(
          size_of_vector_t            = size_of_vector_t,
          vector_h                    = current_vector_h,
          h_infinity                  = h_infinity,
          vector_of_sums_from_w_tilde = vector_of_sums_from_w_tilde_for_u
        )

        discretized_right_hand_side <- .form_discretized_right_hand_side(
          size_of_vector_t = size_of_vector_t,
          vector_w_tilde   = vector_w_tilde
        )

        discretized_system_solution <- .solve_discretized_system(
          discretized_matrix          = discretized_matrix,
          discretized_right_hand_side = discretized_right_hand_side
        )

        vector_mu <- .extract_vector_mu(
          size_of_vector_t            = size_of_vector_t,
          discretized_system_solution = discretized_system_solution
        )

        alpha <- .extract_constant_alpha(
          size_of_vector_t            = size_of_vector_t,
          discretized_system_solution = discretized_system_solution
        )

        vector_of_first_sums_from_u <- .form_vector_of_first_sums_from_u(
          size_of_vector_t = size_of_vector_t,
          size_of_vector_q = size_of_vector_q,
          vector_mu        = vector_mu,
          matrix_n_t_2     = matrix_n_t_2
        )

        # TODO {
        matrix_n_t_q <- .form_matrix_n_t_q(
          size_of_vector_q       = size_of_vector_q,
          matrix_x_infinity      = matrix_x_infinity,
          matrix_x_infinity_star = matrix_x_infinity_star
        )

        vector_of_second_sums_from_u <- .form_vector_of_second_sums_from_u(
          size_of_vector_q = size_of_vector_q,
          vector_f     = vector_f_2,
          matrix_n_t_q = matrix_n_t_q
        )
        # TODO }

        current_vector_u <- .form_vector_u(
          size_of_vector_q             = size_of_vector_q,
          capital_m                    = capital_m,
          h_infinity                   = h_infinity,
          vector_of_first_sums_from_u  = vector_of_first_sums_from_u,
          vector_of_second_sums_from_u = vector_of_second_sums_from_u
        )
      })()

      current_vector_derivative_of_v <- (function() {
        vector_f <- current_vector_u - vector_f_1

        vector_of_sums_from_w_tilde_for_derivative_of_v <- .form_vector_of_sums_from_w_tilde(
          size_of_vector_t = size_of_vector_t,
          size_of_vector_q = size_of_vector_q,
          vector_f         = vector_f,
          matrix_n_1_q     = matrix_n_1_q
        )

        vector_w_tilde <- .form_vector_w_tilde(
          size_of_vector_t            = size_of_vector_t,
          vector_h                    = vector_with_zero_elements,
          h_infinity                  = h_infinity,
          vector_of_sums_from_w_tilde = vector_of_sums_from_w_tilde_for_derivative_of_v
        )

        discretized_right_hand_side <- .form_discretized_right_hand_side(
          size_of_vector_t = size_of_vector_t,
          vector_w_tilde   = vector_w_tilde
        )

        discretized_system_solution <- .solve_discretized_system(
          discretized_matrix          = discretized_matrix,
          discretized_right_hand_side = discretized_right_hand_side
        )

        vector_mu <- .extract_vector_mu(
          size_of_vector_t            = size_of_vector_t,
          discretized_system_solution = discretized_system_solution
        )

        alpha <- .extract_constant_alpha(
          size_of_vector_t            = size_of_vector_t,
          discretized_system_solution = discretized_system_solution
        )

        vector_of_first_sums_from_derivative_of_v <- .form_vector_of_first_sums_from_derivative_of_v(
          size_of_vector_t       = size_of_vector_t,
          vector_mu              = vector_mu,
          matrix_h_3             = matrix_h_3,
          matrix_h_4             = matrix_h_4,
          matrix_derivative_of_x = matrix_derivative_of_x,
          matrix_x               = matrix_x,
          matrix_x_star          = matrix_x_star
        )

        vector_of_second_sums_from_derivative_of_v <- .form_vector_of_second_sums_from_derivative_of_v(
          size_of_vector_t           = size_of_vector_t,
          size_of_vector_q           = size_of_vector_q,
          vector_f                   = vector_f,
          matrix_derivative_of_n_1_q = matrix_derivative_of_n_1_q
        )

        current_vector_derivative_of_v <- .form_vector_derivative_of_v(
          size_of_vector_t                           = size_of_vector_t,
          h_infinity                                 = h_infinity,
          vector_of_first_sums_from_derivative_of_v  = vector_of_first_sums_from_derivative_of_v,
          vector_of_second_sums_from_derivative_of_v = vector_of_second_sums_from_derivative_of_v,
          matrix_derivative_of_x                     = matrix_derivative_of_x,
          vector_mu                                  = vector_mu
        )
      })()

      if (k == 1) {
        p(current_vector_u)
        p(current_vector_derivative_of_v)
        break;
      } else {
        previous_vector_h               <- current_vector_h
        previous_vector_u               <- current_vector_u
        previous_vector_derivative_of_v <- current_vector_derivative_of_v
      }
    }
  }

  .form_vector_t <- function(capital_m, lower_limit_of_gamma_0, upper_limit_of_gamma_0) {
    step       <- AdvancedMath$PI / capital_m
    first_node <- lower_limit_of_gamma_0
    last_node  <- upper_limit_of_gamma_0 - step

    Mesh$generate_equidistant_nodes(
      first_node = first_node,
      last_node  = last_node,
      step       = step
    )
  }

  .calculate_size_of_vector_t <- function(vector_t) {
    Helpers$calculate_size_of_vector(vector_t)
  }

  .form_function_x <- function(function_x_1, function_x_2) {
    function(t) { c(function_x_1(t), function_x_2(t)) }
  }

  .form_function_derivative_of_x <- function(function_derivative_of_x_1, function_derivative_of_x_2) {
    function(t) {
      c(function_derivative_of_x_1(t), function_derivative_of_x_2(t))
    }
  }

  .form_matrix_x <- function(vector_t, size_of_vector_t, function_x) {
    Helpers$generate_matrix_from_vector(
      vector      = vector_t,
      row_size    = size_of_vector_t,
      column_size = 2,
      func        = function(element_t_i) { function_x(element_t_i) }
    )
  }

  .form_matrix_x_star <- function(size_of_vector_t, matrix_x) {
    Helpers$generate_matrix_from_matrix(
      matrix      = matrix_x,
      row_size    = size_of_vector_t,
      column_size = 2,
      func        = function(vector_x_i) { c(vector_x_i[1], -vector_x_i[2]) }
    )
  }

  .form_matrix_derivative_of_x <- function(vector_t, size_of_vector_t, function_derivative_of_x) {
    Helpers$generate_matrix_from_vector(
      vector      = vector_t,
      row_size    = size_of_vector_t,
      column_size = 2,
      func        = function(element_t_i) {
        function_derivative_of_x(element_t_i)
      }
    )
  }

  .form_function_x_infinity <- function() {
    function(t) { c(t, 0) }
  }

  .calculate_size_of_vector_q <- function(capital_m_1) {
    capital_m_1 * 2 + 1
  }

  .form_vector_q <- function(capital_m_1, size_of_vector_q, h_infinity) {
    vector <- Helpers$generate_vector(size_of_vector_q)
    indices <- 1:size_of_vector_q # vector indices

    i <- -capital_m_1 - 1 # sum index i

    for (index in indices) {
      i <- i + 1
      vector[index] <- i * h_infinity
    }

    vector
  }

  .form_matrix_x_infinity <- function(size_of_vector_q, vector_q) {
    Helpers$generate_matrix_from_vector(
      vector      = vector_q,
      row_size    = size_of_vector_q,
      column_size = 2,
      func        = function(element_q_i) { c(element_q_i, 0) }
    )
  }

  .form_matrix_x_infinity_star <- function(size_of_vector_q, matrix_x_infinity) {
    Helpers$generate_matrix_from_matrix(
      matrix      = matrix_x_infinity,
      row_size    = size_of_vector_q,
      column_size = 2,
      func        = function(vector_x_infinity_i) {
        c(vector_x_infinity_i[1], -vector_x_infinity_i[2])
      }
    )
  }

  .form_matrix_n_1_q <- function(size_of_vector_t, size_of_vector_q, matrix_x,
                                 matrix_x_infinity, matrix_x_infinity_star) {
    DiscretizedRightHandSide$form_matrix_n_1_q(
      size_of_vector_t       = size_of_vector_t,
      size_of_vector_q       = size_of_vector_q,
      matrix_x               = matrix_x,
      matrix_x_infinity      = matrix_x_infinity,
      matrix_x_infinity_star = matrix_x_infinity_star
    )
  }

  .form_matrix_r <- function(capital_m, size_of_vector_t, vector_t) {
    DiscretizedMatrix$form_matrix_r(
      capital_m        = capital_m,
      size_of_vector_t = size_of_vector_t,
      vector_t         = vector_t
    )
  }

  .form_matrix_h_1 <- function(size_of_vector_t, vector_t, matrix_derivative_of_x, matrix_x, matrix_x_star) {
    DiscretizedMatrix$form_matrix_h_1(
      size_of_vector_t       = size_of_vector_t,
      vector_t               = vector_t,
      matrix_derivative_of_x = matrix_derivative_of_x,
      matrix_x               = matrix_x,
      matrix_x_star          = matrix_x_star
    )
  }

  .calculate_size_of_discretized_system <- function(capital_m) {
    2 * capital_m + 1
  }

  .form_discretized_matrix <- function(capital_m, size_of_vector_t, size_of_discretized_system, matrix_r, matrix_h_1) {
    DiscretizedMatrix$form_discretized_matrix(
      capital_m                  = capital_m,
      size_of_vector_t           = size_of_vector_t,
      size_of_discretized_system = size_of_discretized_system,
      matrix_r                   = matrix_r,
      matrix_h_1                 = matrix_h_1
    )
  }

  .form_vector_h_0 <- function(size_of_vector_t, matrix_x, function_h_0) {
    Helpers$generate_vector_from_matrix(
      matrix = matrix_x,
      size   = size_of_vector_t,
      func   = function(vector_x_i) {
        function_h_0(vector_x_i)
      }
    )
  }

  .form_vector_f_2 <- function(size_of_vector_q, vector_q, function_f_2) {
    Helpers$generate_vector_from_vector(
      vector = vector_q,
      size   = size_of_vector_q,
      func   = function(vector_q_i) {
        function_f_2(vector_q_i)
      }
    )
  }

  .form_vector_of_sums_from_w_tilde <- function(size_of_vector_t, size_of_vector_q, vector_f, matrix_n_1_q) {
    DiscretizedRightHandSide$form_vector_of_sums_from_w_tilde(
      size_of_vector_t = size_of_vector_t,
      size_of_vector_q = size_of_vector_q,
      vector_f         = vector_f,
      matrix_n_1_q     = matrix_n_1_q
    )
  }

  .form_vector_w_tilde <- function(size_of_vector_t, vector_h, h_infinity, vector_of_sums_from_w_tilde) {
    DiscretizedRightHandSide$form_vector_w_tilde(
      size_of_vector_t            = size_of_vector_t,
      vector_h                    = vector_h,
      h_infinity                  = h_infinity,
      vector_of_sums_from_w_tilde = vector_of_sums_from_w_tilde
    )
  }

  .form_discretized_right_hand_side <-function(size_of_vector_t, vector_w_tilde) {
    DiscretizedRightHandSide$form_discretized_right_hand_side(
      size_of_vector_t = size_of_vector_t,
      vector_w_tilde   = vector_w_tilde
    )
  }

  .solve_discretized_system <- function(discretized_matrix, discretized_right_hand_side) {
    AdvancedMath$solve_system_of_linear_equations(
      matrix = discretized_matrix,
      vector = discretized_right_hand_side
    )
  }

  .extract_vector_mu <- function(size_of_vector_t, discretized_system_solution) {
    c(discretized_system_solution[1:size_of_vector_t])
  }

  .extract_constant_alpha <- function(size_of_vector_t, discretized_system_solution) {
    discretized_system_solution[size_of_vector_t + 1]
  }

  .form_matrix_n_t_2 <- function(size_of_vector_t, size_of_vector_q, matrix_x_infinity, matrix_x, matrix_x_star) {
    Formula311$form_matrix_n_t_2(
      size_of_vector_t  = size_of_vector_t,
      size_of_vector_q  = size_of_vector_q,
      matrix_x_infinity = matrix_x_infinity,
      matrix_x          = matrix_x,
      matrix_x_star     = matrix_x_star
    )
  }

  .form_vector_of_first_sums_from_u <- function(size_of_vector_t, size_of_vector_q, vector_mu, matrix_n_t_2) {
    Formula311$form_vector_of_first_sums_from_u(
      size_of_vector_t = size_of_vector_t,
      size_of_vector_q = size_of_vector_q,
      vector_mu        = vector_mu,
      matrix_n_t_2     = matrix_n_t_2
    )
  }

  .form_matrix_n_t_q <- function(size_of_vector_q, matrix_x_infinity, matrix_x_infinity_star) {
    Formula311$form_matrix_n_t_q(
      size_of_vector_q       = size_of_vector_q,
      matrix_x_infinity      = matrix_x_infinity,
      matrix_x_infinity_star = matrix_x_infinity_star
    )
  }

  .form_vector_of_second_sums_from_u <- function(size_of_vector_q, vector_f, matrix_n_t_q) {
    Formula311$form_vector_of_second_sums_from_u(
      size_of_vector_q = size_of_vector_q,
      vector_f         = vector_f,
      matrix_n_t_q     = matrix_n_t_q
    )
  }

  .form_vector_u <- function(size_of_vector_q, capital_m, h_infinity,
                             vector_of_first_sums_from_u, vector_of_second_sums_from_u) {
    Formula311$form_vector_u(
      size_of_vector_q             = size_of_vector_q,
      capital_m                    = capital_m,
      h_infinity                   = h_infinity,
      vector_of_first_sums_from_u  = vector_of_first_sums_from_u,
      vector_of_second_sums_from_u = vector_of_second_sums_from_u
    )
  }

  .form_vector_with_zero_elements <- function(size_of_vector_t) {
    Helpers$generate_vector_with_equal_elements(
      size    = size_of_vector_t,
      element = 0
    )
  }

  .form_vector_f_1 <- function(size_of_vector_q, vector_q, function_f_1) {
    Helpers$generate_vector_from_vector(
      vector = vector_q,
      size   = size_of_vector_q,
      func   = function(vector_q_i) {
        function_f_1(vector_q_i)
      }
    )
  }

  .form_matrix_h_3 <- function(size_of_vector_t, matrix_x) {
    Formula310$form_matrix_h_3(
      size_of_vector_t = size_of_vector_t,
      matrix_x         = matrix_x
    )
  }

  .form_function_second_derivative_of_x <- function(function_second_derivative_of_x_1,
                                                    function_second_derivative_of_x_2) {
    function(t) {
      c(function_second_derivative_of_x_1(t), function_second_derivative_of_x_2(t))
    }
  }

  .form_matrix_second_derivative_of_x <- function(vector_t, size_of_vector_t, function_second_derivative_of_x) {
    Helpers$generate_matrix_from_vector(
      vector      = vector_t,
      row_size    = size_of_vector_t,
      column_size = 2,
      func        = function(element_t_i) {
        function_second_derivative_of_x(element_t_i)
      }
    )
  }

  .form_matrix_h_4 <- function(size_of_vector_t, matrix_x, matrix_derivative_of_x, matrix_second_derivative_of_x) {
    Formula310$form_matrix_h_4(
      size_of_vector_t              = size_of_vector_t,
      matrix_x                      = matrix_x,
      matrix_derivative_of_x        = matrix_derivative_of_x,
      matrix_second_derivative_of_x = matrix_second_derivative_of_x
    )
  }

  .form_vector_of_first_sums_from_derivative_of_v <- function(size_of_vector_t, vector_mu, matrix_h_3, matrix_h_4,
                                                              matrix_derivative_of_x, matrix_x, matrix_x_star) {
    Formula310$form_vector_of_first_sums_from_derivative_of_v(
      size_of_vector_t       = size_of_vector_t,
      vector_mu              = vector_mu,
      matrix_h_3             = matrix_h_3,
      matrix_h_4             = matrix_h_4,
      matrix_derivative_of_x = matrix_derivative_of_x,
      matrix_x               = matrix_x,
      matrix_x_star          = matrix_x_star
    )
  }

  .form_matrix_derivative_of_x_infinity <- function(size_of_vector_q) {
    Helpers$generate_matrix_from_vector(
      vector      = 1:size_of_vector_q,
      row_size    = size_of_vector_q,
      column_size = 2,
      func        = function(element_q_i) {
        c(1, 0)
      }
    )
  }

  .form_matrix_derivative_of_n_1_q <- function(size_of_vector_t, size_of_vector_q, matrix_x, matrix_x_infinity_star,
                                            matrix_x_infinity, matrix_derivative_of_x, matrix_derivative_of_x_infinity) {
    Formula310$form_matrix_derivative_of_n_1_q(
      size_of_vector_t                = size_of_vector_t,
      size_of_vector_q                = size_of_vector_q,
      matrix_x                        = matrix_x,
      matrix_x_infinity               = matrix_x_infinity,
      matrix_x_infinity_star          = matrix_x_infinity_star,
      matrix_derivative_of_x          = matrix_derivative_of_x,
      matrix_derivative_of_x_infinity = matrix_derivative_of_x_infinity
    )
  }

  .form_vector_of_second_sums_from_derivative_of_v <- function(size_of_vector_t, size_of_vector_q,
                                                               vector_f, matrix_derivative_of_n_1_q) {
    Formula310$form_vector_of_second_sums_from_derivative_of_v(
      size_of_vector_t           = size_of_vector_t,
      size_of_vector_q           = size_of_vector_q,
      vector_f                   = vector_f,
      matrix_derivative_of_n_1_q = matrix_derivative_of_n_1_q
    )
  }

  .form_vector_derivative_of_v <- function(size_of_vector_t, h_infinity, vector_of_first_sums_from_derivative_of_v,
                                          vector_of_second_sums_from_derivative_of_v, matrix_derivative_of_x, vector_mu) {
    Formula310$form_vector_derivative_of_v(
      size_of_vector_t                           = size_of_vector_t,
      h_infinity                                 = h_infinity,
      vector_of_first_sums_from_derivative_of_v  = vector_of_first_sums_from_derivative_of_v,
      vector_of_second_sums_from_derivative_of_v = vector_of_second_sums_from_derivative_of_v,
      matrix_derivative_of_x                     = matrix_derivative_of_x,
      vector_mu                                  = vector_mu
    )
  }

  .calculate_current_vector_h <- function(previous_vector_h, small_gamma, previous_vector_derivative_of_v) {
    previous_vector_h - small_gamma * previous_vector_derivative_of_v
  }
})
