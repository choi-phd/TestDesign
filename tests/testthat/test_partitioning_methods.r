test_that("partitioning methods work", {

  skip_on_cran()
  skip_on_ci()

  # discrete assembly

  set.seed(1)

  config <- createStaticTestConfig(MIP = list(solver = "LPSYMPHONY"))
  constraints <- constraints_science[1:10]

  n_partition <- 4
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "test")
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_equal(subpool@ni, constraints@test_length)
  }

  n_partition <- 4
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "pool")
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_equal(subpool@ni, constraints@pool@ni / n_partition)
  }

  n_partition <- 3
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "pool", partition_size_range = c(320, 340))
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_gte(subpool@ni, 320)
    expect_lte(subpool@ni, 340)
  }

  # set-based assembly

  set.seed(1)

  config <- createStaticTestConfig(MIP = list(solver = "LPSYMPHONY", time_limit = 300))
  constraints <- constraints_reading[1:6]

  n_partition <- 3
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "test")
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_equal(subpool@ni, constraints@test_length)
  }

  n_partition <- 3
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "pool", partition_size_range = c(90, 110))
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_gte(subpool@ni,  90)
    expect_lte(subpool@ni, 110)
  }

})
