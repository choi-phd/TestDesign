test_that("include_items_for_estimation works", {

  skip_on_ci()
  skip_on_cran()

  include_items_for_estimation <- list()
  n_examinees <- 20
  true_theta <- rnorm(n_examinees)
  for (i in 1:n_examinees) {
    include_items_for_estimation[[i]] <- list()
    include_items_for_estimation[[i]]$administered_item_pool <-
      itempool_science[sample(1:1000, 100)]
    include_items_for_estimation[[i]]$administered_item_resp <-
      simResp(
        include_items_for_estimation[[i]]$administered_item_pool,
        true_theta[i]
      )
  }

  cfg <- createShadowTestConfig()
  o1 <- Shadow(
    cfg, constraints_science, true_theta
  )
  o2 <- Shadow(
    cfg, constraints_science, true_theta,
    include_items_for_estimation = include_items_for_estimation
  )

  expect_true(all(o2@final_se_est < o1@final_se_est))

  rmse_o1 <- sqrt(mean((o1@final_theta_est - true_theta) ** 2))
  rmse_o2 <- sqrt(mean((o2@final_theta_est - true_theta) ** 2))

  expect_true(rmse_o2 < rmse_o1)

})
