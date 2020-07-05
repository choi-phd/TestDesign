test_that("calcFisher_value", {

  item_1 <- new("item_1PL", difficulty = 0.5)
  item_2 <- new("item_2PL", slope = 1.5, difficulty = 0.5)
  item_3 <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
  item_4 <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
  item_5 <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
  item_6 <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)

  theta_vec <- seq(-3, 3, 1)
  tol       <- 1e-06

  expect_equal(
    log(prod(calcFisher(item_1, theta_vec))),
    -15.57702,
    tolerance = tol)
  expect_equal(
    log(prod(calcFisher(item_2, theta_vec))),
    -15.12525,
    tolerance = tol)
  expect_equal(
    log(prod(calcFisher(item_3, theta_vec))),
    -21.50401,
    tolerance = tol)
  expect_equal(
    log(prod(calcFisher(item_4, theta_vec))),
    -7.791232,
    tolerance = tol)
  expect_equal(
    log(prod(calcFisher(item_5, theta_vec))),
    -7.756598,
    tolerance = tol)
  expect_equal(
    log(prod(calcFisher(item_6, theta_vec))),
    -12.44241,
    tolerance = tol)
  expect_equal(
    mean(log(unlist(calcFisher(itempool_science, theta_vec)))),
    -3.146117,
    tolerance = tol)
  expect_equal(
    mean(log(unlist(calcFisher(itempool_reading, theta_vec)))),
    -3.158933,
    tolerance = tol)
  expect_equal(
    mean(log(unlist(calcFisher(itempool_fatigue, theta_vec)))),
    -0.9209093,
    tolerance = tol)
  expect_equal(
    mean(log(unlist(calcFisher(itempool_bayes, theta_vec)))),
    -3.607789,
    tolerance = tol)
})
