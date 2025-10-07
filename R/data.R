#' Number of simulated individuals
#' @format An integer
"num_i"

#' Time horizon of the simulation in cycles
#' @format An integer
"num_cycles"

#' Length of each cycle in years
#' @format An integer
"cycle_length"

#' Random number generator state
#' @format An integer
"seed"

#' Willingness to pay for each QALY
#' @format An integer
"wtp"

#' Annual discount rate for costs
#' @format A numeric value
"discount_rate_costs"

#' Annual discount rate for health outcomes
#' @format A numeric value
"discount_rate_QALYs"

#' Matrix of individual features (age and sex)
#' @format A matrix with two columns: "age" and "sex"
"m_indi_features"

#' Vector of the model health state names
#' @format A character vector
"v_states_names"

#' The starting health state for all individuals
#' @format A character vector
"v_starting_states"

#' List of transition probabilities and rates
#' @format A list containing probabilities and rates for transitions between health states.
"l_trans_probs"

#' Vector of cost regression coefficients
#' @format A numeric vector
"v_cost_coeffs"

#' Vector of utility regression coefficients
#' @format A numeric vector
"v_util_coeffs"

#' Vector of state-specific utility decrements over time
#' @format A numeric vector
"v_util_t_decs"

#' Vector of costs for each health state (no treatment)
#' @format A named numeric vector
"v_states_costs"

#' Vector of utilities for each health state (no treatment)
#' @format A named numeric vector
"v_states_utilities"

#' Vector of costs for each health state (treatment 1)
#' @format A named numeric vector
"v_states_costs1"

#' Vector of utilities for each health state (treatment 1)
#' @format A named numeric vector
"v_states_utilities1"

#' Vector of costs for each health state (treatment 2)
#' @format A named numeric vector
"v_states_costs2"

#' Vector of utilities for each health state (treatment 2)
#' @format A named numeric vector
"v_states_utilities2"