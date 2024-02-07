test_that("partitioning methods work", {

  skip_on_cran()
  skip_on_ci()

  solver <- detectBestSolver()
  skip_if(solver == "lpSolve")

  # make a small pool, because existing pools are too big for codetests
  itempool    <- loadItemPool(itempool_science_data[1:200, ])
  itemattrib  <- loadItemAttrib(itemattrib_science_data[1:200, ], itempool)
  constraints <- loadConstraints(constraints_science_data[1:10, ], itempool, itemattrib)

  # discrete assembly ----------------------------------------------------------

  set.seed(1)
  config <- createStaticTestConfig(
    MIP = list(
      solver = solver, time_limit = 600, obj_tol = 0.25,
      verbosity = 1
    )
  )

  # multiple tests
  n_partition <- 4
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "test")
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_equal(subpool@ni, constraints@test_length)
    expect_true(length(partition$s) == 0)
  }

  # multiple pools
  n_partition <- 4
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "pool")
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_equal(subpool@ni, constraints@pool@ni / n_partition)
    expect_true(length(partition$s) == 0)
  }

  # multiple pools (dynamic size)
  n_partition <- 3
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "pool", partition_size_range = c(60, 70))
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_gte(subpool@ni, 60)
    expect_lte(subpool@ni, 70)
    expect_true(length(partition$s) == 0)
  }

  # allow items to be assigned to multiple tests
  n_partition <- 3
  n_maximum_partitions_per_item <- 2
  o <- Split(
    config, constraints, n_partition = n_partition, partition_type = "test",
    n_maximum_partitions_per_item = n_maximum_partitions_per_item
  )
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_equal(subpool@ni, constraints@test_length)
    expect_true(length(partition$s) == 0)
  }
  all_partitions <- unlist(lapply(o@output, function(x) x$i))
  expect_true(all(table(all_partitions) <= n_maximum_partitions_per_item))

  # allow items to be assigned to multiple pools
  n_partition <- 3
  n_maximum_partitions_per_item <- 2
  o <- Split(
    config, constraints, n_partition = n_partition, partition_type = "pool",
    n_maximum_partitions_per_item = n_maximum_partitions_per_item,
    partition_size_range = c(60, 70)
  )
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_gte(subpool@ni, 60)
    expect_lte(subpool@ni, 70)
    expect_true(length(partition$s) == 0)
  }
  all_partitions <- unlist(lapply(o@output, function(x) x$i))
  expect_true(all(table(all_partitions) <= n_maximum_partitions_per_item))

  # set-based assembly ---------------------------------------------------------

  # make a small pool, because existing pools are too big for codetests
  itempool    <- loadItemPool(itempool_reading_data[1:112, ])
  itemattrib  <- loadItemAttrib(itemattrib_reading_data[1:112, ], itempool)
  stimattrib  <- loadStAttrib(stimattrib_reading_data[1:12, ], itemattrib)
  constraints_reading_data_short <- constraints_reading_data[1:5, ]
  constraints_reading_data_short[1, "LB"] <- 10
  constraints_reading_data_short[1, "UB"] <- 10
  constraints_reading_data_short[2, "LB"] <- 2
  constraints_reading_data_short[2, "UB"] <- 2
  constraints_reading_data_short[4, "LB"] <- 5
  constraints_reading_data_short[4, "UB"] <- 5
  constraints_reading_data_short[5, "LB"] <- 5
  constraints_reading_data_short[5, "UB"] <- 5
  constraints <- loadConstraints(
    constraints_reading_data_short,
    itempool,
    itemattrib,
    stimattrib
  )

  set.seed(1)

  config <- createStaticTestConfig(
    MIP = list(
      solver = solver, time_limit = 600, obj_tol = 0.25,
      verbosity = 1
    ),
    item_selection = list(target_location = c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5))
  )

  # multiple tests
  n_partition <- 2
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "test")
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_equal(subpool@ni, constraints@test_length)
    expect_true(length(partition$s) > 0)
  }

  # multiple pools (dynamic size)
  n_partition <- 2
  o <- Split(config, constraints, n_partition = n_partition, partition_type = "pool", partition_size_range = c(50, 60))
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_gte(subpool@ni, 50)
    expect_lte(subpool@ni, 60)
    expect_true(length(partition$s) > 0)
  }

  # allow items to be assigned to multiple tests
  n_partition <- 3
  n_maximum_partitions_per_item <- 2
  o <- Split(
    config, constraints, n_partition = n_partition, partition_type = "test",
    n_maximum_partitions_per_item = n_maximum_partitions_per_item
  )
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_equal(subpool@ni, constraints@test_length)
    expect_true(length(partition$s) > 0)
  }
  all_partitions <- unlist(lapply(o@output, function(x) x$s))
  expect_true(all(table(all_partitions) <= n_maximum_partitions_per_item))

  # allow items to be assigned to multiple pools
  n_partition <- 3
  n_maximum_partitions_per_item <- 2
  o <- Split(
    config, constraints, n_partition = n_partition, partition_type = "pool",
    n_maximum_partitions_per_item = n_maximum_partitions_per_item,
    partition_size_range = c(30, 40)
  )
  for (partition in o@output) {
    subpool <- constraints@pool[partition$i]
    expect_gte(subpool@ni, 30)
    expect_lte(subpool@ni, 40)
    expect_true(length(partition$s) > 0)
  }
  all_partitions <- unlist(lapply(o@output, function(x) x$s))
  expect_true(all(table(all_partitions) <= n_maximum_partitions_per_item))

})
