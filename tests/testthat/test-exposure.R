test_that("Exposure control", {
  set.seed(1)
  trueTheta <- runif(10, min = -3.5, max = 3.5)
  resp.science <- MakeTest(itempool.science, infoType = "FISHER", trueTheta = trueTheta)@Data
  config.science <- config.Shadow(MIP = list(solver = "LPSOLVE"), exposureControl = list(method = "ELIGIBILITY"))
  constraints.science2 <- UpdateConstraints(constraints.science, off = c(14:20, 32:36))
  solution <- Shadow(itempool.science, config.science, trueTheta, constraints.science2, Data = resp.science)

  expect_equal(mean(solution$exposureRate[solution$exposureRate > 0]) < config.science@exposureControl$maxExposureRate, TRUE)
})
