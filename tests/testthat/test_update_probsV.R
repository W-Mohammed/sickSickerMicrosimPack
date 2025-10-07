test_that("update_probsV returns valid transition matrices", {
  v_states_names <- c("H","S1","S2","D")
  v_occupied_state <- c("H","S1","S2","D")
  v_time <- c(1,2,3,1)
  # simple example of transition probabilities
  p_HD <- 0.01
  p_HS1 <- 0.2
  p_S1H <- 0.5
  p_S1S2 <- 0.1
  p_S1D <- 0.05
  p_S2D <- 0.2
  rp_S1 <- 0.1
  rp_S2 <- 0.2
  # organize probabilities in a list
  l_trans <- list(
    p_HD = p_HD, 
    p_HS1 = p_HS1, 
    p_S1H = p_S1H, 
    p_S1S2 = p_S1S2,
    p_S1D = p_S1D, 
    p_S2D = p_S2D, 
    rp_S1 = rp_S1, 
    rp_S2 = rp_S2
  )
  m_probs <- update_probsV(
    v_states_names = v_states_names, 
    v_occupied_state = v_occupied_state, 
    l_trans_probs = l_trans, 
    v_time_in_state = v_time
  )
  # load expected results
  m_expected_probs <- readRDS(
    file = test_path("testdata/data_update_probsV.rds")
  )
  # confirm dimensions of results object
  expect_equal(dim(m_probs), c(length(v_time), length(v_states_names)))
  # each row should sum to 1
  expect_true(all(abs(rowSums(m_probs) - 1) < 1e-10))
  # confirm results are as expected
  expect_equal(m_probs, m_expected_probs)
})
