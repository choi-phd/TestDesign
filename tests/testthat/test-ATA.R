test_that("ATA", {
  packagenames <- c("lpSolve")
  solvernames <- c("lpsolve")

  for (i in 1:1) {
    if (length(find.package(packagenames[i], quiet = T)) > 0) {
      config.science <- CreateStaticTestConfig(
        itemSelection = list(method = "MAXINFO", targetLocation = c(-1, 0, 1)),
        MIP = list(solver = solvernames[i])
      )
      solution <- ATA(config.science, constraints.science, plot = T)
      expect_equal(dim(solution$selected)[1], 30)

      config.science <- CreateStaticTestConfig(
        itemSelection = list(method = "TIF", targetLocation = c(-1, 0, 1), targetValue = c(20, 20, 20)),
        MIP = list(solver = solvernames[i])
      )
      solution <- ATA(config.science, constraints.science, plot = T)
      expect_equal(dim(solution$selected)[1], 30)

      config.science <- CreateStaticTestConfig(
        itemSelection = list(method = "TCC", targetLocation = c(-1, 0, 1), targetValue = c(10, 20, 30)),
        MIP = list(solver = solvernames[i])
      )
      solution <- ATA(config.science, constraints.science, plot = T)
      expect_equal(dim(solution$selected)[1], 30)

      config.reading <- CreateStaticTestConfig(
        itemSelection = list(method = "MAXINFO", targetLocation = c(-2, 2), targetWeight = c(1, 1)),
        MIP = list(solver = solvernames[i])
      )
      solution <- ATA(config.reading, constraints.reading, plot = T)
      expect_equal(dim(solution$selected)[1], 30)

      config.reading <- CreateStaticTestConfig(
        itemSelection = list(method = "TIF", targetLocation = c(1, 2), targetValue = c(10, 30), targetWeight = c(1, 1)),
        MIP = list(solver = solvernames[i])
      )
      solution <- ATA(config.reading, constraints.reading, plot = T)
      expect_equal(dim(solution$selected)[1], 30)
    }
  }
})
