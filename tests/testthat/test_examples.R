test_that("datasets", {
  expect_equal(itempool_science@ni, 1000)
  expect_equal(itempool_reading@ni, 303)
  expect_equal(itempool_fatigue@ni, 95)
})
