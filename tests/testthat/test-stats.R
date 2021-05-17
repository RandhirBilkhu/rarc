test_that("limited expected value", {
  expect_equal(lev_log_norm(10, 1, 15 , 2), 10)
  expect_equal(lev_log_norm(-10, 1, 15 , 2), 24154953)
})


test_that("lnorm sdev", {
  expect_equal(lev_log_norm_sdev(1000, 15, 2), 1.951595, tolerance = 0.1)
})
