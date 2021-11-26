cfg <- createStaticTestConfig()
o <- Static(cfg, constraints_science)

test_that("plot functions work", {
  plot(o, type = "info")
  plot(o, type = "score")
  expect_true(TRUE)
})

cfg <- createShadowTestConfig()
o <- Shadow(cfg, constraints_science, true_theta = c(0, 1), seed = 1)

test_that("plot functions work", {
  plot(o, type = "info")
  plot(o, type = "audit")
  plot(o, type = "shadow")
  plot(o, type = "exposure")
  expect_true(TRUE)
})

test_that("plot functions work", {
  plot(itempool_science, type = "info")
  plot(itempool_science, type = "score")
  plot(constraints_science, type = "info")
  expect_true(TRUE)
})
