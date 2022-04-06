test_that("set-based adaptive assembly works", {

  skip_on_cran()
  skip_on_ci()

  set.seed(1)
  true_theta <- runif(3, -3.5, 3.5)
  resp_reading <- simResp(itempool_reading, true_theta)

  cfg <- createShadowTestConfig(
    MIP = list(solver = "RSYMPHONY"),
    exposure_control = list(method = "NONE")
  )
  set.seed(1)
  solution <- Shadow(cfg, constraints_reading, true_theta, data = resp_reading)

})
