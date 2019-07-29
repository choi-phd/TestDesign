test_that("Exposure control", {
  set.seed(1)
  trueTheta <- runif(10, min = -3.5, max = 3.5)
  resp.science <- makeTest(itempool.science, info.type = "FISHER", true.theta = trueTheta)@Data
  config.science <- createShadowTestConfig(MIP = list(solver = "LPSOLVE"), exposureControl = list(method = "ELIGIBILITY"))
  constraints.science2 <- updateConstraints(constraints.science, off = c(14:20, 32:36))
  solution <- Shadow(itempool.science, config.science, trueTheta, constraints.science2, Data = resp.science)

  expect_equal(mean(solution$exposureRate[solution$exposureRate > 0]) < config.science@exposureControl$maxExposureRate, TRUE)
})
