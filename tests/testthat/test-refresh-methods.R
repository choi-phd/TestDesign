test_that("refresh methods work", {

  set.seed(1)
  true_theta <- 0
  resp_science <- simResp(itempool_science, true_theta)
  constraints_science2 <- toggleConstraints(constraints_science, off = c(14:20, 32:36))

  cfg <- createShadowTestConfig(
    MIP = list(solver = "LPSOLVE"),
    refresh_policy = list(
      method = "ALWAYS"
    )
  )
  set.seed(1)
  solution <- Shadow(cfg, constraints_science2, true_theta, data = resp_science)
  expect_equal(all(solution@output[[1]]@shadow_test_refreshed), TRUE)

  cfg <- createShadowTestConfig(
    MIP = list(solver = "LPSOLVE"),
    refresh_policy = list(
      method = "POSITION",
      position = c(1, 10, 20)
    )
  )
  set.seed(1)
  solution <- Shadow(cfg, constraints_science2, true_theta, data = resp_science)
  expect_equal(which(solution@output[[1]]@shadow_test_refreshed), c(1, 10, 20))

  cfg <- createShadowTestConfig(
    MIP = list(solver = "LPSOLVE"),
    refresh_policy = list(
      method = "INTERVAL",
      interval = 3
    )
  )
  set.seed(1)
  solution <- Shadow(cfg, constraints_science2, true_theta, data = resp_science)
  expect_equal(which(solution@output[[1]]@shadow_test_refreshed), seq(1, 30, 3))

  cfg <- createShadowTestConfig(
    MIP = list(solver = "LPSOLVE"),
    refresh_policy = list(
      method = "THRESHOLD",
      threshold = .1
    )
  )
  set.seed(1)
  solution  <- Shadow(cfg, constraints_science2, true_theta, data = resp_science)

  theta     <- solution@output[[1]]@interim_theta_est
  delta     <- c(0, 0, abs(theta[2:30] - theta[1:29]))
  flag      <- c(delta > .1)[1:30]
  flag[1:2] <- TRUE

  expect_equal(solution@output[[1]]@shadow_test_refreshed, flag)

  cfg <- createShadowTestConfig(
    MIP = list(solver = "LPSOLVE"),
    refresh_policy = list(
      method = "INTERVAL-THRESHOLD",
      threshold = .1,
      interval = 2
    )
  )
  set.seed(1)
  solution  <- Shadow(cfg, constraints_science2, true_theta, data = resp_science)

  theta     <- solution@output[[1]]@interim_theta_est
  delta     <- c(0, 0, abs(theta[2:30] - theta[1:29]))
  flag      <- c(delta > .1)[1:30]
  flag[1:2] <- TRUE
  new_flag  <- rep(FALSE, 30)
  new_flag[seq(1, 30, 2)] <- flag[seq(1, 30, 2)]

  expect_equal(solution@output[[1]]@shadow_test_refreshed, new_flag)


})
