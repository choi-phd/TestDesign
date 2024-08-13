test_that("overlap control works", {

  skip_on_cran()
  skip_on_ci()

  solver <- detectBestSolver()

  # update constraints
  constraints_science <- toggleConstraints(
    constraints_science, off = c(14:20, 32:36)
  )

  # generate true theta
  set.seed(1)
  true_theta <- rnorm(100)

  # simulation 1 ---------------------------------------------------------------

  cfg_adaptive_1 <- createShadowTestConfig(
    MIP = list(solver = solver),
    refresh_policy = list(
      method = "THRESHOLD"
    ),
    exposure_control = list(
      method = "BIGM",
      n_segment = 1,
      segment_cut = c(-Inf, Inf)
    )
  )

  adaptive_science_1 <- Shadow(
    cfg_adaptive_1,
    constraints_science,
    true_theta = true_theta
  )

  expect_true(
    all(rowSums(adaptive_science_1@cumulative_usage_matrix) == 30)
  )

  expect_error({
    plot(adaptive_science_1, type = "overlap")
  })

  # simulation 2 ---------------------------------------------------------------

  cfg_adaptive_2 <- createShadowTestConfig(
    MIP = list(solver = solver),
    refresh_policy = list(
      method = "THRESHOLD"
    ),
    exposure_control = list(
      method = "BIGM",
      n_segment = 1,
      segment_cut = c(-Inf, Inf),
      initial_eligibility_stats = adaptive_science_1@eligibility_stats
    ),
    overlap_control = list(
      method = "BIGM"
    )
  )

  adaptive_science_2 <- Shadow(
    cfg_adaptive_2,
    constraints_science,
    true_theta = true_theta,
    cumulative_usage_matrix = adaptive_science_1@usage_matrix
  )

  expect_true(
    all(rowSums(adaptive_science_2@cumulative_usage_matrix) == 60)
  )

  plot(adaptive_science_2, type = "overlap")

})
