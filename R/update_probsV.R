#' Update Transition Probabilities
#'
#' Computes a transition probability matrix for each individual in the cohort
#' given the current health state, time spent in that state and a list of
#' baseline probabilities and modifiers. Probabilities are adjusted
#' for mortality using rate ratios and time in state. The resulting matrix
#' has one row per individual and one column for each possible next state.
#'
#' @param v_states_names Character vector of all possible health states.
#' @param v_occupied_state Character vector giving the current state for each
#' individual.
#' @param l_trans_probs A named list containing baseline transition
#' probabilities (`p_HD`, `p_HS1`, `p_S1H`, `p_S1S2`, `p_S1D`, `p_S2D`) and rate
#' ratios (`rp_S1`, `rp_S2`) used to update mortality probabilities.
#' @param v_time_in_state Numeric vector indicating the time each individual has
#' spent in their current state. Must be the same length as `v_occupied_state`.
#'
#' @return A matrix of transition probabilities with rows corresponding to
#'   individuals and columns corresponding to the health states listed in
#'   `v_states_names`. Each row sums to one.
#'
#' @examples
#' v_states_names <- c("H", "S1", "S2", "D")
#' v_occupied_state <- c("H", "S1")
#' l_trans_probs <- list(
#'   p_HD = 0.005,
#'   p_HS1 = 0.15,
#'   p_S1H = 0.5,
#'   p_S1S2 = 0.105,
#'   p_S1D = 1 - exp(-3 * (-log(1-0.005))),
#'   p_S2D = 1 - exp(-10 * (-log(1-0.005))), rp_S1 = 0.2,
#'   rp_S2 = 0.29
#' )
#' v_time_in_state <- c(1, 2)
#' 
#' m_probs <- update_probsV(
#'   v_states_names,
#'   v_occupied_state,
#'   l_trans_probs,
#'   v_time_in_state
#'  )
#' 
#' rowSums(m_probs)
#'
#' @export
update_probsV <- function(
    v_states_names,
    v_occupied_state,
    l_trans_probs,
    v_time_in_state) {

  with(
    data = l_trans_probs,
    expr = {

      # Convert baseline probabilities to rates and apply time‑dependent
      # rate ratios
      r_S1D <- -log(1 - p_S1D)
      r_S2D <- -log(1 - p_S2D)
      p_S1D  <- 1 - exp(
        -r_S1D * (1 + v_time_in_state[v_occupied_state == "S1"] * rp_S1)
      )
      p_S2D  <- 1 - exp(
        -r_S2D * (1 + v_time_in_state[v_occupied_state == "S2"] * rp_S2)
      )

      # Probability of remaining in S1 when sick is whatever probability is
      # left after moving to other states
      p_S1S1 <- 1 - p_S1S2 - p_S1H - p_S1D

      # Some probabilities may need to be repeated to match the number of
      # individuals
      p_HD_rep <- rep(p_HD, length(which(v_occupied_state == "H")))
      p_DD_rep <- rep(1, length(v_occupied_state[v_occupied_state == "D"]))

      # Allocate a transition matrix with appropriate row and column names
      m_probs <- matrix(
        nrow = length(v_time_in_state),
        ncol = length(v_states_names),
        dimnames = list(
          v_occupied_state,
          v_states_names
        )
      )

      # Fill in the transition matrix by current state
      if (any(v_occupied_state == "H")) {
        m_probs[v_occupied_state == "H", ]  <- cbind(
          1 - p_HS1 - p_HD_rep, p_HS1, 0, p_HD_rep
        )
      }
      if (any(v_occupied_state == "S1")) {
        m_probs[v_occupied_state == "S1", ] <- cbind(
          p_S1H, p_S1S1, p_S1S2, p_S1D
        )
      }
      if (any(v_occupied_state == "S2")) {
        m_probs[v_occupied_state == "S2", ] <- cbind(0, 0, 1 - p_S2D, p_S2D)
      }
      if (any(v_occupied_state == "D")) {
        m_probs[v_occupied_state == "D", ]  <- cbind(0, 0, 0, p_DD_rep)
      }

      # Ensure rows sum to one
      if (!all(abs(rowSums(m_probs) - 1) < 1e-12)) {
        stop("Probabilities do not sum to 1")
      }
      
      return(m_probs)
    }
  )
}