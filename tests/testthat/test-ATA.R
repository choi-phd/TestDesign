test_that("ATA", {
  package_names <- c("lpSolve")
  solver_names <- c("lpsolve")

  for (i in 1:1) {
    if (length(find.package(package_names[i], quiet = TRUE)) > 0) {
      config.science <- createStaticTestConfig(
        itemSelection = list(method = "MAXINFO", targetLocation = c(-1, 0, 1)),
        MIP = list(solver = solver_names[i])
      )
      solution <- ATA(config.science, constraints.science, plot = TRUE)
      expect_equal(dim(solution$selected)[1], 30)

      config.science <- createStaticTestConfig(
        itemSelection = list(method = "TIF", targetLocation = c(-1, 0, 1), targetValue = c(20, 20, 20)),
        MIP = list(solver = solver_names[i])
      )
      solution <- ATA(config.science, constraints.science, plot = TRUE)
      expect_equal(dim(solution$selected)[1], 30)

      config.science <- createStaticTestConfig(
        itemSelection = list(method = "TCC", targetLocation = c(-1, 0, 1), targetValue = c(10, 20, 30)),
        MIP = list(solver = solver_names[i])
      )
      solution <- ATA(config.science, constraints.science, plot = TRUE)
      expect_equal(dim(solution$selected)[1], 30)

      config.reading <- createStaticTestConfig(
        itemSelection = list(method = "MAXINFO", targetLocation = c(-2, 2), targetWeight = c(1, 1)),
        MIP = list(solver = solver_names[i])
      )
      solution <- ATA(config.reading, constraints.reading, plot = TRUE)
      expect_equal(dim(solution$selected)[1], 30)

      config.reading <- createStaticTestConfig(
        itemSelection = list(method = "TIF", targetLocation = c(1, 2), targetValue = c(10, 30), targetWeight = c(1, 1)),
        MIP = list(solver = solver_names[i])
      )
      solution <- ATA(config.reading, constraints.reading, plot = TRUE)
      expect_equal(dim(solution$selected)[1], 30)
    }
  }
})
