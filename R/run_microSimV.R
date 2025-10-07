#' Run Healthy–Sick–Sicker–Dead Microsimulation
#'
#' Runs a vectorised microsimulation for a cohort of individuals over a
#' specified number of cycles. Individuals transition between health states
#' according to probabilities that depend on their current state and time
#' spent in that state. Costs and QALYs are accumulated each cycle and
#' optionally discounted.
#'
#' @param v_starting_states Character vector giving the starting state for each
#' individual.
#' @param num_i Integer giving the number of individuals in the cohort.
#' @param num_cycles Integer giving the number of cycles to simulate.
#' @param m_indi_features Matrix or data.frame of individual covariates with
#' rows corresponding to individuals and columns corresponding to features.
#' Must include a column named `age` which is incremented each cycle.
#' @param v_states_names Character vector of all state names in the order used
#' throughout the model.
#' @param v_states_costs Named numeric vector of base costs for each state.
#' @param v_cost_coeffs Numeric vector of regression coefficients for costs.
#' @param v_states_utilities Named numeric vector of base utilities for each
#' state.
#' @param v_util_coeffs Numeric vector of regression coefficients for utilities.
#' @param v_util_t_decs Named numeric vector of time‑dependent utility
#' decrements for states S1 and S2.
#' @param l_trans_probs Named list of transition probabilities and rate ratios;
#' see `update_probsV` for details.
#' @param discount_rate_costs Annual discount rate used for costs.
#' @param discount_rate_QALYs Annual discount rate used for QALYs.
#' @param cycle_length Length of each cycle in years. Defaults to 1.
#' @param starting_seed Optional seed for the random number generator to allow
#' reproducible results.
#'
#' @return A list containing the matrices of states, costs and QALYs by cycle,
#' undiscounted and discounted totals per individual and averages across
#' individuals.
#'
#' @examples
#' res <- run_microSimV(
#'   v_starting_states = v_starting_states,
#'   num_i = num_i,
#'   num_cycles = num_cycles,
#'   m_indi_features = m_indi_features,
#'   v_states_names = v_states_names,
#'   v_states_costs = v_states_costs,
#'   v_cost_coeffs = v_cost_coeffs,
#'   v_states_utilities = v_states_utilities,
#'   v_util_coeffs = v_util_coeffs,
#'   v_util_t_decs = v_util_t_decs,
#'   l_trans_probs = l_trans_probs, 
#'   discount_rate_costs = discount_rate_costs,
#'   discount_rate_QALYs = discount_rate_QALYs,
#'   cycle_length = cycle_length,
#'   starting_seed = seed
#' )
#' 
#' res$mean_costs
#' res$mean_qalys
#'
#' @export
run_microSimV <- function(
    v_starting_states,
    num_i,
    num_cycles,
    m_indi_features,
    v_states_names,
    v_states_costs,
    v_cost_coeffs,
    v_states_utilities,
    v_util_coeffs,
    v_util_t_decs,
    l_trans_probs,
    discount_rate_costs,
    discount_rate_QALYs,
    cycle_length = 1,
    starting_seed = 1) {

  # Allocate result matrices
  m_States <- m_Costs <- m_Effs <- matrix(
    nrow = num_i,
    ncol = num_cycles + 1,
    dimnames = list(paste0("ind_", 1:num_i), paste0("cycle_", 0:num_cycles))
  )

  # Set seed for reproducibility
  set.seed(starting_seed)
  v_time_in_state <- rep(1, times = num_i)
  m_States[, 1] <- v_starting_states

  # Initial cycle outcomes
  m_Costs[, 1] <- calc_costsV(
    v_occupied_state = m_States[, 1],
    v_states_costs   = v_states_costs,
    m_indi_features  = m_indi_features,
    v_cost_coeffs    = v_cost_coeffs
  )
  m_Effs[, 1] <- calc_effsV(
    v_occupied_state   = m_States[, 1],
    v_states_utilities = v_states_utilities,
    m_indi_features    = m_indi_features,
    v_util_coeffs      = v_util_coeffs,
    v_util_t_decs      = v_util_t_decs,
    v_time_in_state    = v_time_in_state,
    cycle_length       = cycle_length
  )

  # Loop through cycles
  for (t in 1:num_cycles) {
    # Update transition probabilities
    m_trans_probs <- update_probsV(
      v_states_names   = v_states_names,
      v_occupied_state = m_States[, t],
      l_trans_probs    = l_trans_probs,
      v_time_in_state  = v_time_in_state
    )
    # Sample next state
    m_States[, t + 1] <- sampleV(
      m_trans_probs  = m_trans_probs,
      v_states_names = v_states_names
    )
    # Update time in state
    stayed <- m_States[, t] == m_States[, t + 1]
    v_time_in_state[stayed]  <- v_time_in_state[stayed] + 1
    v_time_in_state[!stayed] <- 1
    # Advance age if an age column exists
    if ("age" %in% colnames(m_indi_features)) {
      m_indi_features[, "age"] <- m_indi_features[, "age"] + 1
    }
    # Costs and QALYs for next cycle
    m_Costs[, t + 1] <- calc_costsV(
      v_occupied_state = m_States[, t + 1],
      v_states_costs   = v_states_costs,
      m_indi_features  = m_indi_features,
      v_cost_coeffs    = v_cost_coeffs
    )
    m_Effs[, t + 1]  <- calc_effsV(
      v_occupied_state   = m_States[, t + 1],
      v_states_utilities = v_states_utilities,
      m_indi_features    = m_indi_features,
      v_util_coeffs      = v_util_coeffs,
      v_util_t_decs      = v_util_t_decs,
      v_time_in_state    = v_time_in_state,
      cycle_length       = cycle_length
    )
  }

  # Discount weights
  v_c_dsc_wts <- calc_discount_wts(
    discount_rate = discount_rate_costs,
    num_cycles    = num_cycles,
    cycle_length  = cycle_length
  )
  v_e_dsc_wts <- calc_discount_wts(
    discount_rate = discount_rate_QALYs,
    num_cycles    = num_cycles,
    cycle_length  = cycle_length
  )

  # Summaries
  v_total_costs  <- rowSums(m_Costs)
  v_total_qalys  <- rowSums(m_Effs)
  mean_costs     <- mean(v_total_costs)
  mean_qalys     <- mean(v_total_qalys)
  v_total_Dcosts <- m_Costs %*% v_c_dsc_wts
  v_total_Dqalys <- m_Effs  %*% v_e_dsc_wts
  mean_Dcosts    <- mean(v_total_Dcosts)
  mean_Dqalys    <- mean(v_total_Dqalys)

  results <- list(
    m_States       = m_States,
    m_Costs        = m_Costs,
    m_Effs         = m_Effs,
    v_total_costs  = as.numeric(v_total_costs),
    v_total_qalys  = as.numeric(v_total_qalys),
    v_total_Dcosts = as.numeric(v_total_Dcosts),
    v_total_Dqalys = as.numeric(v_total_Dqalys),
    mean_costs     = mean_costs,
    mean_qalys     = mean_qalys,
    mean_Dcosts    = mean_Dcosts,
    mean_Dqalys    = mean_Dqalys
  )
  
  return(results)
}
