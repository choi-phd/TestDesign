test_that("partitioning methods work", {

  skip_on_cran()
  skip_on_ci()

  solver <- detectBestSolver()
  skip_if(solver == "lpSolve")

  # make a small pool, because existing pools are too big for codetests
  itempool    <- loadItemPool(itempool_science_data[1:200, ])
  itemattrib  <- loadItemAttrib(itemattrib_science_data[1:200, ], itempool)
  constraints <- loadConstraints(constraints_science_data[1:10, ], itempool, itemattrib)

  # discrete assembly

  set.seed(1)
  config <- createStaticTestConfig(
    MIP = list(
      solver = solver, time_limit = 600, obj_tol = 0.25,
      verbosity = 1
    )
  )

  n_partition <- 4
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "test")
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_equal(subpool@ni, constraints@test_length)
    expect_true(length(partition$s) == 0)
  }

  n_partition <- 4
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "pool")
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_equal(subpool@ni, constraints@pool@ni / n_partition)
    expect_true(length(partition$s) == 0)
  }

  n_partition <- 3
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "pool", partition_size_range = c(60, 70))
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_gte(subpool@ni, 60)
    expect_lte(subpool@ni, 70)
    expect_true(length(partition$s) == 0)
  }

  # set-based assembly

  set.seed(1)

  config <- createStaticTestConfig(
    MIP = list(
      solver = solver, time_limit = 600, obj_tol = 0.25,
      verbosity = 1
    ),
    item_selection = list(target_location = c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5))
  )
  constraints <- constraints_reading[1:5]

  n_partition <- 2
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "test")
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_equal(subpool@ni, constraints@test_length)
    expect_true(length(partition$s) > 0)
  }

  n_partition <- 2
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "pool", partition_size_range = c(140, 160))
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_gte(subpool@ni, 140)
    expect_lte(subpool@ni, 160)
    expect_true(length(partition$s) > 0)
  }

})
