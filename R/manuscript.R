if (FALSE) {
  # IRT models

  item_1 <- new("item.1pl", difficulty = 0.5)
  item_2 <- new("item.2pl", slope = 1.0, difficulty = 0.5)
  item_3 <- new("item.3pl", slope = 1.0, difficulty = 0.5, guessing = 0.2)
  item_4 <- new("item.pc", threshold = c(-0.5, 0.5), ncat = 3)
  item_5 <- new("item.gpc", slope = 1.0, threshold = c(-0.5, 0.0, 0.5), ncat = 4)
  item_6 <- new("item.gr", slope = 1.0, category = c(-1, 0, 1), ncat = 4)

  calcProb(item_1, 0)
  calcFisher(item_2, 0)
  calcEscore(item_3, 0.5)
  calcDerivative(item_4, 1)
  calcDerivative2(item_5, 1)
  calcJacobian(item_6, 0, resp = 1)
  calcHessian(item_6, 0, resp = 1)

  # Manuscript figures

  config_science <- createStaticTestConfig(itemSelection = list(method = "MAXINFO", targetLocation = c(-1, 0, 1)))
  solution <- ATA(config_science, constraints_science, plot = T)
  solution$selected

  setEPS()
  postscript("R/science_maxinfo.eps", width = 8, height = 3.25)
  solution$plot
  dev.off()


  config_science <- createStaticTestConfig(itemSelection = list(method = "TCC", targetValue = c(15, 20, 25), targetLocation = c(-1, 0, 1)))
  solution <- ATA(config_science, constraints_science, plot = T)
  solution$selected

  setEPS()
  postscript("R/science_tcc.eps", width = 8, height = 3.25)
  solution$plot
  dev.off()

  # Exposure control plot

  set.seed(1)

  true_theta <- runif(7000, min = -3.5, max = 3.5)
  resp_science <- makeTest(itempool_science, infoType = "FISHER", true_theta = true_theta)@Data

  config_science <- config.Shadow(MIP = list(solver = "LPSOLVE"), exposureControl = list(method = "ELIGIBILITY"))

  constraints_science2 <- updateConstraints(constraints_science, off = c(14:20, 32:36))

  solution <- Shadow(itempool_science, config_science, true_theta, constraints_science2, Data = resp_science)

  tt <- rep(NA, 7000)

  for (i in 1:7000) {
    tt[i] <- sum(solution$output[[i]]@solveTime)
  }


  setEPS()
  postscript("R/science.exposure.eps", width = 8, height = 4.25)
  par(mfrow = c(2, 4))
  plotExposure(solution)
  dev.off()


  # Manuscript example: adaptive test assembly

  set.seed(1)

  true_theta <- runif(1, min = -3.5, max = 3.5)
  resp_science <- makeTest(itempool_science, true_theta = true_theta)@Data

  config_science <- config.Shadow()

  solution <- Shadow(itempool_science, config_science,
    true_theta, constraints_science,
    Data = resp_science
  )

  setEPS()
  postscript("R/science.shadowplot.eps", width = 8, height = 9.7)
  p <- plotShadow(solution, constraints_science, examineeID = 1)
  p
  dev.off()

  setEPS()
  postscript("R/science.auditplot.eps", width = 8, height = 4.2)
  p <- plotCAT(solution, examineeID = 1, minTheta = -4, maxTheta = 1.5)
  print(p)
  dev.off()



  # Manuscript example: adaptive test assembly (reading)

  config.reading <- createStaticTestConfig(itemSelection = list(method = "MAXINFO", targetLocation = c(-1, 0, 1)))

  solution <- ATA(config.reading, constraints.reading, plot = T)
  solution$selected
  solution$plot

  set.seed(1)

  true_theta <- runif(1, min = -3.5, max = 3.5)
  resp.reading <- makeTest(itempool.reading, true_theta = true_theta)@Data

  config.reading <- config.Shadow(refreshPolicy = list(
    method = "STIMULUS"
  ))

  solution <- Shadow(itempool.reading, config.reading, NULL, constraints.reading, Data = resp.reading)

  setEPS()
  postscript("R/reading.auditplot.eps", width = 8, height = 4.2)
  p <- plotCAT(solution, 1, maxTheta = 3)
  p
  dev.off()

  setEPS()
  postscript("R/reading.shadowplot.eps", width = 8, height = 9)
  p <- plotShadow(solution, constraints.reading, 1)
  p
  dev.off()


  # Fatigue

  setEPS()
  postscript("R/fatigue.infoplot.eps", width = 8, height = 3.25)
  p <- maxinfoplot(itempool.fatigue, constraints.fatigue)
  print(p)
  dev.off()

  config.fatigue <- createStaticTestConfig(itemSelection = list(method = "TIF", targetValue = c(40, 40, 40), targetLocation = c(0, 1, 2)))

  solution <- ATA(config.fatigue, constraints.fatigue, plot = T)
  solution$selected

  setEPS()
  postscript("R/fatigue.tif.eps", width = 8, height = 3.25)
  solution$plot
  dev.off()


  # Fatigue - generalized shadow test

  set.seed(1)

  true_theta <- runif(1, min = -3.5, max = 3.5)
  respData <- makeTest(itempool.fatigue, true_theta = true_theta)@Data

  config.fatigue <- config.Shadow(refreshPolicy = list(
    method = "POSITION",
    position = c(1, 5, 9)
  ))

  solution <- Shadow(itempool.fatigue, config.fatigue, true_theta, constraints.fatigue, Data = respData)

  setEPS()
  postscript("R/fatigue.auditplot.eps", width = 8, height = 8)
  p <- plotCAT(solution, 1)
  p
  dev.off()

  setEPS()
  postscript("R/fatigue.shadowplot.eps", width = 8, height = 6)
  p <- plotShadow(solution, constraints.fatigue, 1)
  p
  dev.off()

  # Paper Note: ATA with specifying a target amount of information
  # to prevent item exposure and information overuse

}
