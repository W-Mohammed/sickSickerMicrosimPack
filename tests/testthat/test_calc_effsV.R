test_that("calc_effsV computes QALYs appropriately", {
  
  v_states_utilities <- c(H = 1, S1 = 0.8, S2 = 0.5, D = 0)
  v_occupied_state <- c("H", "S1", "S2", "D")
  m_indi_features <- cbind(age = c(50, 60, 70, 80), sex = c(0, 1, 0, 1))
  v_util_coeffs <- c(age = -0.001, sex = -0.01)
  v_util_t_decs <- c(S1 = -0.002, S2 = -0.003)
  v_time <- c(1, 2, 3, 1)
  
  qalys <- calc_effsV(
    v_occupied_state, 
    v_states_utilities, 
    m_indi_features,
    v_util_coeffs, 
    v_util_t_decs, 
    v_time, 
    cycle_length = 1)
  
  # dead has zero QALY
  expect_equal(qalys[4], 0)

  # healthy female at age 50 QALY
  expect_equal(qalys[1], 0.950)
  
  # QALYs should be within 0 and 1
  expect_true(all(qalys >= 0))
  expect_true(all(qalys <= 1))
})
