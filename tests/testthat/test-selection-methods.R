set.seed(1)
true_theta <- seq(-2, 2, 1)
resp_bayes <- simResp(itempool_bayes, true_theta)

test_that("item selection MFI works", {

  cfg <- createShadowTestConfig(
    MIP = list(solver = "LPSOLVE"),
    item_selection = list(method = "MFI")
  )
  set.seed(1)
  solution <- Shadow(cfg, constraints_bayes, true_theta, data = resp_bayes)
  fit <- lm(solution@final_theta_est ~ true_theta)
  expect_gt(coef(fit)[2], 0.7)

})

test_that("item selection MPWI works", {

  cfg <- createShadowTestConfig(
    MIP = list(solver = "LPSOLVE"),
    item_selection = list(method = "MPWI")
  )
  set.seed(1)
  solution <- Shadow(cfg, constraints_bayes, true_theta, data = resp_bayes)
  fit <- lm(solution@final_theta_est ~ true_theta)
  expect_gt(coef(fit)[2], 0.7)

})

test_that("item selection EB works", {

  cfg <- createShadowTestConfig(
    MIP = list(solver = "LPSOLVE"),
    item_selection = list(method = "EB"),
    interim_theta = list(method = "EB")
  )
  set.seed(1)
  solution <- Shadow(cfg, constraints_bayes, true_theta, data = resp_bayes)
  fit <- cor(solution@final_theta_est, true_theta)
  expect_gt(fit, 0.9)

})

test_that("item selection FB works", {

  cfg <- createShadowTestConfig(
    MIP = list(solver = "LPSOLVE"),
    item_selection = list(method = "FB"),
    interim_theta = list(method = "FB")
  )
  set.seed(1)
  solution <- Shadow(cfg, constraints_bayes, true_theta, data = resp_bayes)
  fit <- cor(solution@final_theta_est, true_theta)
  expect_gt(fit, 0.9)

})
