test_that("deprecated", {

  f1 <- file.path(tempdir(), "ipar.csv")
  write.csv(itempool_bayes_data, f1, row.names = FALSE)
  f2 <- file.path(tempdir(), "se.csv")
  write.csv(itempool_se_bayes_data, f2, row.names = FALSE)

  pool_new <- loadItemPool(ipar = f1, ipar_se = f2)                   # is equivalent to
  pool_old <- suppressWarnings(loadItemPool(file = f1, se_file = f2)) # pre 1.1.0

  file.remove(f1)
  file.remove(f2)

  expect_equal(identical(pool_new, pool_old), TRUE)

})
