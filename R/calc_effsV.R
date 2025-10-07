#' Calculate Health Outcomes (QALYs)
#'
#' Computes the quality adjusted life years accrued in a single cycle for each
#' individual. Utilities for states S1 and S2 are adjusted by individual
#' covariate coefficients and by the time spent in the state.
#'
#' @param v_occupied_state Character vector of current state for each
#' individual.
#' @param v_states_utilities Named numeric vector of base utilities for each
#' state.
#' @param m_indi_features Matrix or data.frame of individual covariates.
#' @param v_util_coeffs Numeric vector of regression coefficients for the
#' individual covariates used to calculate utility decrements.
#' @param v_util_t_decs Named numeric vector of time‑dependent utility 
#' decrements for states S1 and S2. The names must correspond to state names.
#' @param v_time_in_state Numeric vector indicating the time spent in the
#' current state.
#' @param cycle_length Length of one cycle measured in years. Defaults to 1.
#'
#' @return A numeric vector of QALYs for each individual over the cycle.
#'
#' @examples
#' v_occupied_state <- c("H", "S1", "S2", "D")
#' v_states_utilities <- c(H = 1, S1 = 0.75, S2 = 0.5, D = 0)
#' m_indi_features <- cbind(age = c(50, 60, 70, 80), sex = c(0, 1, 0, 1))
#' v_util_coeffs <- c(age = -0.0018, sex = -0.015)
#' v_util_t_decs <- c(S1 = -0.0015, S2 = -0.002)
#' v_time_in_state <- c(1, 2, 3, 4)
#' 
#' calc_effsV(
#'   v_occupied_state = v_occupied_state,
#'   v_states_utilities = v_states_utilities,
#'   m_indi_features = m_indi_features,
#'   v_util_coeffs = v_util_coeffs,
#'   v_util_t_decs = v_util_t_decs,
#'   v_time_in_state = v_time_in_state
#' )
#'
#' @export
calc_effsV <- function (
    v_occupied_state,
    v_states_utilities,
    m_indi_features,
    v_util_coeffs,
    v_util_t_decs,
    v_time_in_state,
    cycle_length = 1) {
  
  # calculate individual-specific utility decrements using utilities reg coeffs
  v_ind_decrement <- (m_indi_features %*% v_util_coeffs)[,1]
  
  # calculate time-dependent state-specific utility decrements
  time_decrement <- rep(0, length(v_occupied_state))
  time_decrement[v_occupied_state == "S1"] <- v_util_t_decs["S1"] * 
    v_time_in_state[v_occupied_state == "S1"]
  time_decrement[v_occupied_state == "S2"] <- v_util_t_decs["S2"] * 
    v_time_in_state[v_occupied_state == "S2"]
  
  # estimate total decrements
  decrement <- v_ind_decrement + time_decrement
  
  # estimate utilities based on occupied state
  v_state_utility                           <- rep(
    NA_real_, 
    length(v_occupied_state)
  )
  # update the utility if healthy
  v_state_utility[v_occupied_state == "H"]  <- v_states_utilities["H"] +
    decrement[v_occupied_state == "H"]
  # update the utility if sick
  v_state_utility[v_occupied_state == "S1"] <- v_states_utilities["S1"] +
    decrement[v_occupied_state == "S1"]
  # update the utility if sicker
  v_state_utility[v_occupied_state == "S2"] <- v_states_utilities["S2"] +
    decrement[v_occupied_state == "S2"]
  # update the utility if dead
  v_state_utility[v_occupied_state == "D"]  <- v_states_utilities["D"]
  
  # calculate Quality Adjusted Life Years (QALYs)
  QALYs <-  v_state_utility * cycle_length                                                                    # calculate the QALYs during cycle `t`
  
  return(QALYs)                                                                                               # return the QALYs
}
