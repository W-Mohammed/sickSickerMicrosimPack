test_that("calc_costsV calculates costs correctly", {
  v_states_costs <- c(H=1000, S1=2000, S2=3000, D=0)
  v_occupied_state <- c("H","S1","S2","D")
  m_indi_features <- cbind(age=c(50,60,70,80), sex=c(0,1,0,1))
  v_cost_coeffs <- c(age=10, sex=100)
  costs <- calc_costsV(v_occupied_state, v_states_costs, m_indi_features, v_cost_coeffs)
  
  # base costs for H and D should be equal to their base costs
  expect_equal(costs[1], v_states_costs[["H"]])
  expect_equal(costs[4], v_states_costs[["D"]])
  
  # costs for S1 and S2 include covariate effects
  expect_gt(costs[2], v_states_costs[["S1"]])
  expect_gt(costs[3], v_states_costs[["S2"]])
  
  # costs for S1 and S2 include covariate effects
  expected_costs <- v_states_costs[v_occupied_state[2:3]] |> unname() + 
    (v_cost_coeffs[["age"]] * m_indi_features[, "age"][2:3]) +
    (v_cost_coeffs[["sex"]] * m_indi_features[, "sex"][2:3])
  expect_equal(costs[2:3], expected_costs)
})
