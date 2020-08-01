test_that("static", {
  package_names <- c("lpSolve")
  solver_names <- c("lpSolve")

  for (i in 1:1) {
    if (length(find.package(package_names[i], quiet = TRUE)) > 0) {
      config_science <- createStaticTestConfig(
        item_selection = list(method = "MAXINFO", target_location = c(-1, 0, 1)),
        MIP = list(solver = solver_names[i])
      )
      solution <- Static(config_science, constraints_science)
      expect_equal(dim(solution@selected)[1], 30)

      config_science <- createStaticTestConfig(
        item_selection = list(method = "TIF", target_location = c(-1, 0, 1), target_value = c(20, 20, 20)),
        MIP = list(solver = solver_names[i])
      )
      solution <- Static(config_science, constraints_science)
      expect_equal(dim(solution@selected)[1], 30)

      config_science <- createStaticTestConfig(
        item_selection = list(method = "TCC", target_location = c(-1, 0, 1), target_value = c(10, 20, 30)),
        MIP = list(solver = solver_names[i])
      )
      solution <- Static(config_science, constraints_science)
      expect_equal(dim(solution@selected)[1], 30)

      config_reading <- createStaticTestConfig(
        item_selection = list(method = "MAXINFO", target_location = c(-2, 2)),
        MIP = list(solver = solver_names[i])
      )
      solution <- Static(config_reading, constraints_reading, force_solver = TRUE)
      expect_equal(dim(solution@selected)[1], 30)

      config_reading <- createStaticTestConfig(
        item_selection = list(method = "TIF", target_location = c(1, 2), target_value = c(10, 30)),
        MIP = list(solver = solver_names[i])
      )
      solution <- Static(config_reading, constraints_reading, force_solver = TRUE)
      expect_equal(dim(solution@selected)[1], 30)
    }
  }
})
