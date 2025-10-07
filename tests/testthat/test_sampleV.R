test_that("sampleV draws valid states", {
  m_trans_probs <- matrix(
    data = c(0.5, 0.5, 0, 0, 0.2, 0.3, 0.3, 0.2), 
    nrow = 2, 
    byrow = TRUE
  )
  v_states_names <- c("H","S1","S2","D")
  
  set.seed(123)
  v_res <- sampleV(
    m_trans_probs = m_trans_probs, 
    v_states_names = v_states_names
  )
  # define expected results
  v_expected_res <- c("H", "S2")
  # confirm length equal to number of simulated individuals (rows)
  expect_length(v_res, 2)
  # confirm only valid state names are returned
  expect_true(all(v_res %in% v_states_names))
  # confirm correct outputs
  expect_equal(v_res, v_expected_res)
})
