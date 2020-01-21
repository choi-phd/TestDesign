test_that("Exposure control", {
  set.seed(1)
  true_theta <- seq(-3, 3, .5)
  resp_science <- makeTest(itempool_science, info_type = "FISHER", true_theta = true_theta)@data
  constraints_science2 <- updateConstraints(constraints_science, off = c(14:20, 32:36))

  config_science <- createShadowTestConfig(MIP = list(solver = "LPSYMPHONY"), exposure_control = list(method = "ELIGIBILITY"))
  solution <- Shadow(config_science, constraints_science2, true_theta, data = resp_science)
  exposure_rate <- solution$exposure_rate[, 2]
  expect_equal(mean(exposure_rate[exposure_rate > 0]) < config_science@exposure_control$max_exposure_rate, TRUE)

  true_theta <- 0
  config_science <- createShadowTestConfig(MIP = list(solver = "LPSYMPHONY"), refresh_policy = list(method = "THRESHOLD", threshold = .1))
  solution <- Shadow(config_science, constraints_science2, true_theta, data = resp_science)
  expect_equal(all(solution$output[[1]]@shadow_test_refreshed), FALSE)

  config_science <- createShadowTestConfig(MIP = list(solver = "LPSYMPHONY"), refresh_policy = list(method = "THRESHOLD", threshold = .0))
  solution <- Shadow(config_science, constraints_science2, true_theta, data = resp_science)
  expect_equal(all(solution$output[[1]]@shadow_test_refreshed), TRUE)

})
