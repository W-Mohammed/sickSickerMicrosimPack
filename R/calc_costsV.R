#' Calculate Costs
#'
#' Computes the cost incurred by each individual in a cycle based on
#' their current health state and individual covariates. Costs for
#' states S1 and S2 can be modified by regression coefficients applied
#' to the individual features (e.g. age, sex).
#'
#' @inheritParams run_microSimV
#' @inheritParams calc_effsV
#'
#' @return A numeric vector of costs for the current cycle for each individual.
#'
#' @examples
#' v_occupied_state <- c("H", "S1", "S2", "D")
#' v_states_costs <- c(H = 2000, S1 = 4000, S2 = 15000, D = 0)
#' m_indi_features <- cbind(age = c(50, 60, 70, 80), sex = c(0, 1, 0, 1))
#' v_cost_coeffs <- c(age = 11.5, sex = 300)
#'
#' calc_costsV(
#'   v_occupied_state = v_occupied_state, 
#'   v_states_costs = v_states_costs, 
#'   m_indi_features = m_indi_features,
#'   v_cost_coeffs = v_cost_coeffs
#' )
#'
#' @export
calc_costsV <- function(
    v_occupied_state,
    v_states_costs,
    m_indi_features,
    v_cost_coeffs) {
  
  # Assertions
  assertthat::assert_that(is.character(v_occupied_state))
  assertthat::assert_that(is.numeric(v_states_costs))
  assertthat::assert_that(
    assertthat::has_name(x = v_states_costs, which = c("H", "S1", "S2", "D"))
  )
  assertthat::assert_that(is.matrix(m_indi_features))
  assertthat::assert_that(nrow(m_indi_features) == length(v_occupied_state))
  assertthat::assert_that(is.numeric(v_cost_coeffs))
  
  # calculate individual-specific costs based on costs regression coefficients
  v_indi_costs <- m_indi_features %*% v_cost_coeffs
  
  # estimate costs based on occupied state
  v_state_costs                           <- rep(NA_real_, length(v_occupied_state))
  v_state_costs[v_occupied_state == "H"]  <- v_states_costs["H"]                                           # update the cost if healthy
  # update the cost if sick
  v_state_costs[v_occupied_state == "S1"] <- v_states_costs["S1"] + 
    v_indi_costs[v_occupied_state == "S1"]
  # update the cost if sicker
  v_state_costs[v_occupied_state == "S2"] <- v_states_costs["S2"] + 
    v_indi_costs[v_occupied_state == "S2"]
  v_state_costs[v_occupied_state == "D"]  <- v_states_costs["D"]                                           # update the cost if dead
  
  return(v_state_costs)
}
