test_that("Exposure control", {
  set.seed(1)
  true_theta <- runif(10, min = -3.5, max = 3.5)
  resp_science <- makeTest(itempool_science, info_type = "FISHER", true_theta = true_theta)@data
  constraints_science2 <- updateConstraints(constraints_science, off = c(14:20, 32:36))

  config_science <- createShadowTestConfig(MIP = list(solver = "LPSOLVE"), exposure_control = list(method = "ELIGIBILITY"))
  solution <- Shadow(itempool_science, config_science, true_theta, constraints_science2, data = resp_science)
  expect_equal(mean(solution$exposure_rate[solution$exposure_rate > 0]) < config_science@exposure_control$max_exposure_rate, TRUE)

  true_theta <- runif(1, min = -3.5, max = 3.5)
  config_science <- createShadowTestConfig(MIP = list(solver = "LPSOLVE"), refresh_policy = list(method = "THRESHOLD", threshold = .1))
  solution <- Shadow(itempool_science, config_science, true_theta, constraints_science2, data = resp_science)
  expect_equal(all(solution$output[[1]]@shadow_test_refreshed), FALSE)

  config_science <- createShadowTestConfig(MIP = list(solver = "LPSOLVE"), refresh_policy = list(method = "THRESHOLD", threshold = .0))
  solution <- Shadow(itempool_science, config_science, true_theta, constraints_science2, data = resp_science)
  expect_equal(all(solution$output[[1]]@shadow_test_refreshed), TRUE)


  set.seed(1)
  true_theta <- runif(10, min = -3.5, max = 3.5)
  resp_science <- makeTest(itempool_science, info_type = "FISHER", true_theta = true_theta)@data
  constraints_science2 <- updateConstraints(constraints_science, off = c(14:20, 32:36))

  config_science <- createShadowTestConfig(MIP = list(solver = "LPSOLVE"), exposure_control = list(method = "ELIGIBILITY"))
  solution <- Shadow(itempool_science, config_science, true_theta, constraints_science2, data = resp_science)
  expect_equal(mean(solution$exposure_rate[solution$exposure_rate > 0]) < config_science@exposure_control$max_exposure_rate, TRUE)

  true_theta <- runif(1, min = -3.5, max = 3.5)
  config_science <- createShadowTestConfig(MIP = list(solver = "LPSOLVE"), refresh_policy = list(method = "THRESHOLD", threshold = .1))
  solution <- Shadow(itempool_science, config_science, true_theta, constraints_science2, data = resp_science)
  expect_equal(all(solution$output[[1]]@shadow_test_refreshed), FALSE)

  config_science <- createShadowTestConfig(MIP = list(solver = "LPSOLVE"), refresh_policy = list(method = "THRESHOLD", threshold = .0))
  solution <- Shadow(itempool_science, config_science, true_theta, constraints_science2, data = resp_science)
  expect_equal(all(solution$output[[1]]@shadow_test_refreshed), TRUE)

})
