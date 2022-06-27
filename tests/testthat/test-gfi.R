test_that("GFI works", {

  skip_on_cran()

  set.seed(1)
  true_theta <- 0
  resp_science <- simResp(itempool_science, true_theta)

  target_value <- 5

  cfg <- createShadowTestConfig(
    item_selection = list(
      method       = "GFI",
      target_value = target_value
    ),
    exposure_control = list(
      method = "BIGM"
    ),
    MIP = list(
      solver = "LPSOLVE"
    )
  )
  set.seed(1)
  solution <- Shadow(cfg, constraints_science, true_theta, data = resp_science)

  for (item_position in 2:30) {
    shadow_test      <- solution@output[[1]]@shadow_test[[item_position]]$i
    target_theta     <- solution@output[[1]]@interim_theta_est[item_position - 1, ]
    shadow_test_info <- calcFisher(itempool_science[shadow_test], target_theta)
    shadow_test_info <- sum(shadow_test_info)
    expect_equal(shadow_test_info, target_value, tolerance = 0.05)
  }

})
