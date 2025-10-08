#' Calculate Discount Weights
#'
#' Computes discount weights for a sequence of cycles given a discount rate
#' expressed per annum and the length of each cycle. These weights can be
#' multiplied by costs or QALYs to obtain discounted values.
#'
#' @param discount_rate Annual discount rate expressed as a fraction 
#' (e.g. 0.03).
#' @inheritParams run_microSimV
#'
#' @return A numeric vector of discount weights of length `num_cycles + 1`.
#'
#' @examples
#' calc_discount_wts(
#'   discount_rate = 0.03,
#'   num_cycles = 5, 
#'   cycle_length = 1
#' )
#'
#' @export
calc_discount_wts <- function(
    discount_rate,
    num_cycles,
    cycle_length,
    assert = TRUE) {
  
  # Assertions
  if(isTRUE(assert)) {
    assertthat::assert_that(assertthat::is.number(discount_rate))
    assertthat::assert_that(assertthat::is.count(num_cycles))
    assertthat::assert_that(assertthat::is.number(cycle_length))
  }
  
  # calculate discount weights based on the number & length (in years) of cycles
  v_discount_wts <- 1 / (1 + discount_rate) ^ ((0:num_cycles) * cycle_length)
  
  return(v_discount_wts)
}
