test_that("mixed (set+discrete) adaptive assembly works", {

  skip_on_cran()
  skip_on_ci()

  solver <- detectBestSolver()
  skip_if(solver == "lpSolve")

  set.seed(1)
  true_theta <- runif(3, -3.5, 3.5)
  resp_reading <- simResp(itempool_reading, true_theta)

  idx_to_remove_set <- 205:303
  itemattrib_reading_data$STID[idx_to_remove_set] <- NA
  itemattrib_reading <- loadItemAttrib(
    itemattrib_reading_data,
    itempool_reading
  )

  stimattrib_reading <- loadStAttrib(
    stimattrib_reading_data,
    itemattrib_reading
  )

  constraints_reading_data$LB[2] <- 4
  constraints_reading_data$UB[2] <- 4

  constraints_reading <- loadConstraints(
    constraints_reading_data,
    itempool_reading,
    itemattrib_reading,
    stimattrib_reading
  )

  cfg <- createShadowTestConfig(
    MIP = list(solver = solver),
    exposure_control = list(method = "NONE")
  )
  set.seed(1)
  solution <- Shadow(cfg, constraints_reading, true_theta, data = resp_reading)

})
