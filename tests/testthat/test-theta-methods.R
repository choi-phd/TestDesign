set.seed(1)
true_theta <- runif(10, -3, 3)
resp_bayes <- simResp(itempool_bayes, true_theta)

methods <- c("EAP", "MLE", "MLEF", "EB", "FB")

for (m in methods) {

  test_name <- sprintf("interim %s works", m)
  test_that(test_name, {

    cfg <- createShadowTestConfig(
      MIP = list(solver = "LPSOLVE"),
      interim_theta = list(method = m)
    )
    set.seed(1)
    solution <- Shadow(cfg, constraints_bayes, true_theta, data = resp_bayes)
    last_interim_theta <- unlist(lapply(solution@output, function(x) x@interim_theta_est[30]))
    fit <- lm(last_interim_theta ~ true_theta)
    expect_gt(coef(fit)[2], 0.7)

  })

}

for (m in methods) {

  test_name <- sprintf("final %s works", m)
  test_that(test_name, {

    cfg <- createShadowTestConfig(
      MIP = list(solver = "LPSOLVE"),
      final_theta = list(method = m)
    )
    set.seed(1)
    solution <- Shadow(cfg, constraints_bayes, true_theta, data = resp_bayes)
    fit <- cor(solution@final_theta_est, true_theta)
    expect_gt(fit, 0.7)

  })

}
