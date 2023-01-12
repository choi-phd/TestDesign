test_that("partitioning methods work", {

  skip_on_cran()
  skip_on_ci()

  solver <- detectBestSolver()
  skip_if(solver == "lpSolve")

  # discrete assembly

  set.seed(1)

  config <- createStaticTestConfig(MIP = list(solver = solver, time_limit = 300, obj_tol = 0.25))
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

  config <- createStaticTestConfig(
    MIP = list(solver = solver, time_limit = 300, obj_tol = 0.25),
    item_selection = list(target_location = c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5))
  )
  constraints <- constraints_reading[1:5]

  n_partition <- 2
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "test")
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_equal(subpool@ni, constraints@test_length)
  }

  n_partition <- 2
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "pool", partition_size_range = c(140, 160))
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_gte(subpool@ni, 140)
    expect_lte(subpool@ni, 160)
  }

})
