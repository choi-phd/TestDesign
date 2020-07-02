test_that("calcProb_value", {

  item_1 <- new("item_1PL", difficulty = 0.5)
  item_2 <- new("item_2PL", slope = 1.5, difficulty = 0.5)
  item_3 <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
  item_4 <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
  item_5 <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
  item_6 <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)

  theta_vec <- seq(-3, 3, 1)
  tol       <- 1e-06

  expect_equal(
    log(prod(calcProb(item_1, theta_vec))),
    -15.57702,
    tolerance = tol)
  expect_equal(
    log(prod(calcProb(item_2, theta_vec))),
    -20.80176,
    tolerance = tol)
  expect_equal(
    log(prod(calcProb(item_3, theta_vec))),
    -12.77404,
    tolerance = tol)
  expect_equal(
    log(prod(calcProb(item_4, theta_vec))),
    -76.95776,
    tolerance = tol)
  expect_equal(
    log(prod(calcProb(item_5, theta_vec))),
    -91.78112,
    tolerance = tol)
  expect_equal(
    log(prod(calcProb(item_6, theta_vec))),
    -52.22233,
    tolerance = tol)
  expect_equal(
    mean(log(unlist(calcProb(itempool_science, theta_vec)))),
    -1.250381,
    tolerance = tol)
  expect_equal(
    mean(log(unlist(calcProb(itempool_reading, theta_vec)))),
    -1.732649,
    tolerance = tol)
  expect_equal(
    mean(log(unlist(calcProb(itempool_fatigue, theta_vec)))),
    -5.295978,
    tolerance = tol)
  expect_equal(
    mean(log(unlist(calcProb(itempool_bayes, theta_vec)))),
    -1.097485,
    tolerance = tol)
})
