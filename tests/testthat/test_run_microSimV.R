test_that("run_microSimV returns sensible results", {
  set.seed(1)
  num_i <- 5
  num_cycles <- 2
  v_states_names <- c("H", "S1", "S2", "D")
  v_start <- rep("H", num_i)
  m_feats <- cbind(
    age = rnorm(n = num_i, mean = 50, sd = 3),
    sex = sample(c(0,1), num_i, replace = TRUE)
  )
  v_states_costs <- c(H = 1000, S1 = 2000, S2 = 3000, D = 0)
  v_cost_coeffs  <- c(age = 10, sex = 100)
  v_states_utilities <- c(H = 1, S1 = 0.8, S2 = 0.5, D = 0)
  v_util_coeffs <- c(age = -0.001, sex = -0.01)
  v_util_t_decs <- c(S1 = -0.002, S2 = -0.003)
  l_trans_probs <- list(
    p_HD = 0.01,
    p_HS1 = 0.2,
    p_S1H = 0.3,
    p_S1S2 = 0.1,
    p_S1D = 0.05,
    p_S2D = 0.2,
    rp_S1 = 0.1,
    rp_S2 = 0.2
  )
  set.seed(1)
  l_res <- run_microSimV(
    v_starting_states = v_start,
    num_i = num_i,
    num_cycles = num_cycles,
    m_indi_features = m_feats,
    v_states_names = v_states_names,
    v_states_costs = v_states_costs,
    v_cost_coeffs = v_cost_coeffs,
    v_states_utilities = v_states_utilities,
    v_util_coeffs = v_util_coeffs,
    v_util_t_decs = v_util_t_decs,
    l_trans_probs = l_trans_probs,
    discount_rate_costs = 0.03,
    discount_rate_QALYs = 0.015,
    cycle_length = 1,
    starting_seed = 1
  )
  # load expected results
  l_expected_res <- readRDS(test_path("testdata", "data_run_microSimV.rds"))
  # ensure names present
  expect_true(
    all(
      c(
        "m_States", "m_Costs", "m_Effs", "v_total_costs", "v_total_qalys",
        "mean_costs","mean_qalys"
      ) %in% names(l_res)
    )
  )
  # means positive
  expect_true(l_res$mean_costs >= 0)
  expect_true(l_res$mean_qalys >= 0)
  # equal results
  expect_equal(l_res$v_total_costs, l_expected_res$v_total_costs)
  expect_equal(l_res$v_total_qalys, l_expected_res$v_total_qalys)
  expect_equal(l_res$v_total_Dcosts, l_expected_res$v_total_Dcosts)
  expect_equal(l_res$v_total_Dqalys, l_expected_res$v_total_Dqalys)
  expect_equal(l_res$m_States, l_expected_res$m_States)
  expect_equal(l_res$m_Costs, l_expected_res$m_Costs)
  expect_equal(l_res$m_Effs, l_expected_res$m_Effs)
})
