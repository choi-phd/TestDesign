test_that("calcFisher_typecast", {

  item_1 <- new("item_1PL", difficulty = 0.5)
  item_2 <- new("item_2PL", slope = 1.5, difficulty = 0.5)
  item_3 <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
  item_4 <- new("item_PC", threshold = c(-1, 0, 1), ncat = 4)
  item_5 <- new("item_GPC", slope = 1.2, threshold = c(-0.8, -1.0, 0.5), ncat = 4)
  item_6 <- new("item_GR", slope = 0.9, category = c(-1, 0, 1), ncat = 4)

  arg_names <- paste0("item_", 1:6)
  arg_names <- c(
    arg_names,
    "itempool_science",
    "itempool_reading",
    "itempool_fatigue",
    "itempool_bayes"
  )

  theta_vec = seq(-3, 3, 1)
  theta_mat = matrix(theta_vec, length(theta_vec), 1)

  for (i in arg_names) {
    obj_vec <- eval(parse(text = sprintf("calcFisher(%s, theta_vec)", i)))
    obj_mat <- eval(parse(text = sprintf("calcFisher(%s, theta_mat)", i)))
    expect_identical(obj_vec, obj_mat)
  }

})
