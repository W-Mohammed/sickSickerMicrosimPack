test_that("calc_discount_wts returns correct weights", {
  wts <- calc_discount_wts(0.035, 3, 1)
  
  # length should be num_cycles + 1
  expect_length(wts, 4)
  
  # first weight is 1 (no discount at time zero)
  expect_equal(wts[1], 1)
  
  # weight at 24 months
  expect_equal(wts[2] |> round(digits = 7), 0.9661836)
  
  # decreasing sequence
  expect_true(all(diff(wts) <= 0))
})
