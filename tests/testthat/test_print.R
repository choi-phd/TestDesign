test_that("print", {
  expect_equal(print(itempool_science)            , itempool_science)
  expect_equal(print(itemattrib_science)          , itemattrib_science@data)
  expect_equal(print(stimattrib_reading)          , stimattrib_reading@data)
  expect_equal(print(constraints_science)         , constraints_science@constraints)
  expect_equal(print(summary(itempool_science))   , summary(itempool_science))
  expect_equal(print(summary(itemattrib_science)) , summary(itemattrib_science))
  expect_equal(print(summary(stimattrib_reading)) , summary(stimattrib_reading))
  expect_equal(print(summary(constraints_science)), summary(constraints_science))

  cfg_st = createStaticTestConfig()
  expect_equal(print(cfg_st), cfg_st)

  cfg_sh = createShadowTestConfig()
  expect_equal(print(cfg_sh), cfg_sh)

  sol_st = Static(cfg_st, constraints_science)
  expect_equal(print(sol_st, index_only = FALSE), sol_st@selected)
  expect_equal(print(summary(sol_st))           , summary(sol_st))

  sol_sh = Shadow(cfg_sh, constraints_science, true_theta = seq(-1, 1, 1))
  expect_equal(print(sol_sh)         , sol_sh)
  expect_equal(print(summary(sol_sh)), summary(sol_sh))

})
