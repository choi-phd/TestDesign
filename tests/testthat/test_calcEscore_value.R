test_that("calcEscore_value", {

  item_1 <- new("item_1PL", difficulty = 0.5)
  item_2 <- new("item_2PL", slope = 1.5, difficulty = 0.5)
  item_3 <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
  item_4 <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
  item_5 <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
  item_6 <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)

  theta_vec <- seq(-3, 3, 1)
  tol       <- 1e-06

  expect_equal(
    log(prod(calcEscore(item_1, theta_vec))),
    -9.53851,
    tolerance = tol)
  expect_equal(
    log(prod(calcEscore(item_2, theta_vec))),
    -13.02588,
    tolerance = tol)
  expect_equal(
    log(prod(calcEscore(item_3, theta_vec))),
    -5.173527,
    tolerance = tol)
  expect_equal(
    log(prod(calcEscore(item_4, theta_vec))),
    -0.1421625,
    tolerance = tol)
  expect_equal(
    log(prod(calcEscore(item_5, theta_vec))),
    0.03717596,
    tolerance = tol)
  expect_equal(
    log(prod(calcEscore(item_6, theta_vec))),
    0.8290602,
    tolerance = tol)
  expect_equal(
    mean(log(unlist(calcEscore(itempool_science, theta_vec)))),
    6.375118,
    tolerance = tol)
  expect_equal(
    mean(log(unlist(calcEscore(itempool_reading, theta_vec)))),
    4.890361,
    tolerance = tol)
  expect_equal(
    mean(log(unlist(calcEscore(itempool_fatigue, theta_vec)))),
    3.690174,
    tolerance = tol)
  expect_equal(
    mean(log(unlist(calcEscore(itempool_bayes, theta_vec)))),
    5.139598,
    tolerance = tol)
})
