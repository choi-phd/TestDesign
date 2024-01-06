test_that("combining constraints works", {

  # usual working case
  expect_warning(
    o <- c(
      constraints_science[c(1, 2)],
      constraints_science[c(1, 10)]
    ),
    "duplicate constraint IDs were removed"
  )

  expect_equal(length(o@list_constraints), 3)
  expect_equal(o@list_constraints[[1]]@constraint_id, "C1")
  expect_equal(o@list_constraints[[2]]@constraint_id, "C2")
  expect_equal(o@list_constraints[[3]]@constraint_id, "C10")

  # edge working case: complete overlap
  expect_warning(
    o <- c(
      constraints_science[c(1, 3, 5)],
      constraints_science[c(1, 3, 5)]
    ),
    "duplicate constraint IDs were removed"
  )

  expect_equal(length(o@list_constraints), 3)
  expect_equal(o@list_constraints[[1]]@constraint_id, "C1")
  expect_equal(o@list_constraints[[2]]@constraint_id, "C3")
  expect_equal(o@list_constraints[[3]]@constraint_id, "C5")

  # usual non-working case: different constraints source

  expect_error(
    o <- c(
      constraints_science[c(1, 2)],
      constraints_reading[1:3]
    ),
    "@pool does not match"
  )

})
