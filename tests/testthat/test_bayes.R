test_that("bayes", {
  set.seed(1)
  true_theta <- seq(-3, 3, .5)
  resp_bayes <- makeTest(itempool_bayes, info_type = "FISHER", true_theta = true_theta)@data

  config_bayes <- createShadowTestConfig(
    MIP = list(solver = "LPSOLVE"),
    exposure_control = list(method = "BIGM"))
  solution <- Shadow(config_bayes, constraints_bayes, true_theta, data = resp_bayes)
  exposure_rate <- solution@exposure_rate[, 2]

  expect_lt(
    mean(exposure_rate[exposure_rate > 0]),
    unique(config_bayes@exposure_control$max_exposure_rate)
  )

  config_bayes <- createShadowTestConfig(
    MIP = list(solver = "LPSOLVE"),
    exposure_control = list(method = "BIGM-BAYESIAN"),
    interim_theta = list(method = "EB"))
  solution <- Shadow(config_bayes, constraints_bayes, true_theta, data = resp_bayes)   #####

  expect_equal(
    length(solution@output[[7]]@posterior_sample),
    config_bayes@MCMC$post_burn_in
  )
  expect_equal(
    mean(solution@output[[7]]@posterior_sample),
    solution@output[[7]]@interim_theta_est[solution@constraints@test_length],
    tolerance = 1e-6
  )

  config_bayes <- createShadowTestConfig(
    MIP = list(solver = "LPSOLVE"),
    exposure_control = list(method = "BIGM-BAYESIAN"),
    interim_theta = list(method = "FB"))
  solution <- Shadow(config_bayes, constraints_bayes, true_theta, data = resp_bayes)
  expect_equal(
    length(solution@output[[7]]@posterior_sample),
    config_bayes@MCMC$post_burn_in
  )
  expect_equal(
    mean(solution@output[[7]]@posterior_sample),
    solution@output[[7]]@interim_theta_est[solution@constraints@test_length],
    tolerance = 1e-6
  )

})
