library(TestDesign)

true_theta <- 2

test_that("carryover estimation method works", {

  # working case
  cfg <- createShadowTestConfig(
    interim_theta = list(
      method = "CARRYOVER"
    )
  )
  o <- Shadow(cfg, constraints_science, true_theta = true_theta)
  expect_true(all(
    o@output[[1]]@interim_theta_est ==
    o@output[[1]]@initial_theta_est$theta
  ))

  # working case: custom initial theta
  cfg <- createShadowTestConfig(
    item_selection = list(
      initial_theta = -2
    ),
    interim_theta = list(
      method = "CARRYOVER"
    )
  )
  o <- Shadow(cfg, constraints_science, true_theta = true_theta)
  expect_true(all(
    o@output[[1]]@interim_theta_est == -2
  ))

  # working case: custom prior
  cfg <- createShadowTestConfig(
    interim_theta = list(
      method = "CARRYOVER",
      prior_dist = "NORMAL",
      prior_par = c(-2, 1)
    )
  )
  o <- Shadow(cfg, constraints_science, true_theta = true_theta)
  expect_equal(
    o@output[[1]]@initial_theta_est$theta,
    expected = -2,
    tolerance = 0.1
  )
  expect_true(all(
    o@output[[1]]@interim_theta_est ==
    o@output[[1]]@initial_theta_est$theta
  ))

  # working case: carryover from previous EAP simulation

  cfg <- createShadowTestConfig(
    final_theta = list(
      method = "EAP"
    )
  )
  o1 <- Shadow(cfg, constraints_science, true_theta = true_theta)

  include_items_for_estimation <- list(
    list(
      administered_item_pool = itempool_science[o1@output[[1]]@administered_item_index],
      administered_item_resp = o1@output[[1]]@administered_item_resp
    )
  )

  cfg <- createShadowTestConfig(
    interim_theta = list(
      method = "CARRYOVER"
    )
  )
  o2 <- Shadow(
    cfg, constraints_science, true_theta = true_theta,
    include_items_for_estimation = include_items_for_estimation
  )

  expect_equal(
    as.numeric(unique(o2@output[[1]]@interim_theta_est)),
    o1@output[[1]]@final_theta_est,
    tolerance = 1e-6
  )

  # non-trivial case: carryover from MLE
  # this causes a small shift in interim thetas in the second run
  #
  # this is because carryover from MLE simulation is done as carrying over items and responses, not as carrying over the MLE estimate directly
  # in the second run, initial theta/SEs are recreated by doing EAP using items and responses, which causes a small shift from MLE estimate

  cfg <- createShadowTestConfig(
    interim_theta = list(
      method = "EAP"
    ),
    final_theta = list(
      method = "MLE"
    )
  )
  o1 <- Shadow(cfg, constraints_science, true_theta = true_theta)

  include_items_for_estimation <- list(
    list(
      administered_item_pool = itempool_science[o1@output[[1]]@administered_item_index],
      administered_item_resp = o1@output[[1]]@administered_item_resp
    )
  )

  cfg <- createShadowTestConfig(
    interim_theta = list(
      method = "CARRYOVER"
    )
  )
  o2 <- Shadow(
    cfg, constraints_science, true_theta = true_theta,
    include_items_for_estimation = include_items_for_estimation
  )

  expect_equal(
    as.numeric(unique(o2@output[[1]]@interim_theta_est)),
    o1@output[[1]]@final_theta_est,
    tolerance = 0.1
  )

})
