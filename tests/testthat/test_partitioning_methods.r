test_that("partitioning methods work", {

  skip_on_cran()
  skip_on_ci()

  set.seed(1)

  config <- createStaticTestConfig(MIP = list(solver = "LPSYMPHONY"))
  constraints <- constraints_science[1:10]

  n_partition <- 4
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "test")
  for (partition in o) {
    subpool <- constraints@pool[partition]
    expect_equal(subpool@ni, constraints@test_length)
  }

  n_partition <- 4
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "pool")
  for (partition in o) {
    subpool <- constraints@pool[partition]
    expect_equal(subpool@ni, constraints@pool@ni / n_partition)
  }

})
