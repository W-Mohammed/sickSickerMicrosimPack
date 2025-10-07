#' Sample Health States
#'
#' Samples the health state for each individual given their row of the
#' transition probability matrix. A multinomial sampling approach is used
#' whereby a uniform random number is compared against the cumulative
#' probabilities to determine the next state.
#'
#' @param m_trans_probs A matrix of transition probabilities with rows
#' corresponding to individuals and columns to states.
#' @param v_states_names Character vector of state names matching the
#' column order of `m_trans_probs`.
#'
#' @return A character vector of the sampled next state for each row of
#' `m_trans_probs`.
#'
#' @examples
#' m_probs <- matrix(
#'   data = c(0.8, 0.2, 0, 0, 0.1, 0.7, 0.1, 0.1),
#'   nrow = 2,
#'   byrow = TRUE
#' )
#' colnames(m_probs) <- c("H", "S1", "S2", "D")
#' sampleV(
#'   m_trans_probs = m_probs, 
#'   v_states_names = colnames(m_probs)
#' )
#'
#' @export
sampleV <- function(
    m_trans_probs,
    v_states_names) {
  
  # create an upper triangular matrix of ones
  m_upper_tri <- upper.tri(
    x = diag(ncol(m_trans_probs)),
    diag = TRUE
  )
  
  # create matrix with row-wise cumulative transition probabilities
  m_cum_probs <- m_trans_probs %*% m_upper_tri
  colnames(m_cum_probs) <- v_states_names
  
  # ensure that the maximum cumulative probabilities are equal to 1
  if (any(m_cum_probs[, ncol(m_cum_probs)] > 1.000000)) {
    stop("Error in multinomial sampling: probabilities do not sum to 1")
  }
  
  # sample random values from Uniform standard distribution for each individual
  v_rand_values <- stats::runif(n = nrow(m_trans_probs))
  
  # repeat each sampled value to have as many copies as the number of states
  m_rand_values <- matrix(
    data  = rep(
      x = v_rand_values,
      each = length(v_states_names)
    ),
    nrow  = nrow(m_trans_probs),
    ncol  = length(v_states_names),
    byrow = TRUE
  )
  
  # identify transitions, compare random samples to cumulative probabilities
  m_transitions <- m_rand_values > m_cum_probs # transitions from first state
  
  # sum transitions to identify health state in next cycle
  v_transitions <- rowSums(m_transitions)
  
  # identify health state to which each individual is transitioning
  v_health_states <- v_states_names[1 + v_transitions]
  
  return(v_health_states)
}
