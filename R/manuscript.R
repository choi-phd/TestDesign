if (FALSE) {
  # IRT models

  item.1 <- new("item.1pl", difficulty = 0.5)
  item.2 <- new("item.2pl", slope = 1.0, difficulty = 0.5)
  item.3 <- new("item.3pl", slope = 1.0, difficulty = 0.5, guessing = 0.2)
  item.4 <- new("item.pc", threshold = c(-0.5, 0.5), ncat = 3)
  item.5 <- new("item.gpc", slope = 1.0, threshold = c(-0.5, 0.0, 0.5), ncat = 4)
  item.6 <- new("item.gr", slope = 1.0, category = c(-1, 0, 1), ncat = 4)

  calcProb(item.1, 0)
  calcFisher(item.2, 0)
  calcEscore(item.3, 0.5)
  calcDerivative(item.4, 1)
  calcDerivative2(item.5, 1)
  calcJacobian(item.6, 0, resp = 1)
  calcHessian(item.6, 0, resp = 1)

  # Manuscript figures

  config.science <- config.ATA(itemSelection = list(method = "MAXINFO", targetLocation = c(-1, 0, 1)))
  solution <- ATA(config.science, constraints.science, plot = T)
  solution$Selected

  setEPS()
  postscript("R/science.maxinfo.eps", width = 8, height = 3.25)
  solution$plot
  dev.off()


  config.science <- config.ATA(itemSelection = list(method = "TCC", targetValue = c(15, 20, 25), targetLocation = c(-1, 0, 1)))
  solution <- ATA(config.science, constraints.science, plot = T)
  solution$Selected

  setEPS()
  postscript("R/science.tcc.eps", width = 8, height = 3.25)
  solution$plot
  dev.off()

  # Exposure control plot

  set.seed(1)

  trueTheta <- runif(7000, min = -3.5, max = 3.5)
  resp.science <- MakeTest(itempool.science, infoType = "FISHER", trueTheta = trueTheta)@Data

  config.science <- config.Shadow(MIP = list(solver = "LPSOLVE"), exposureControl = list(method = "ELIGIBILITY"))

  constraints.science2 <- UpdateConstraints(constraints.science, off = c(14:20, 32:36))

  solution <- Shadow(itempool.science, config.science, trueTheta, constraints.science2, Data = resp.science)

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

  trueTheta <- runif(1, min = -3.5, max = 3.5)
  resp.science <- MakeTest(itempool.science, trueTheta = trueTheta)@Data

  config.science <- config.Shadow()

  solution <- Shadow(itempool.science, config.science,
    trueTheta, constraints.science,
    Data = resp.science
  )

  setEPS()
  postscript("R/science.shadowplot.eps", width = 8, height = 9.7)
  p <- plotShadow(solution, constraints.science, examineeID = 1)
  p
  dev.off()

  setEPS()
  postscript("R/science.auditplot.eps", width = 8, height = 4.2)
  p <- plotCAT(solution, examineeID = 1, minTheta = -4, maxTheta = 1.5)
  print(p)
  dev.off()



  # Manuscript example: adaptive test assembly (reading)

  config.reading <- config.ATA(itemSelection = list(method = "MAXINFO", targetLocation = c(-1, 0, 1)))

  solution <- ATA(config.reading, constraints.reading, plot = T)
  solution$Selected
  solution$plot

  set.seed(1)

  trueTheta <- runif(1, min = -3.5, max = 3.5)
  resp.reading <- MakeTest(itempool.reading, trueTheta = trueTheta)@Data

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

  config.fatigue <- config.ATA(itemSelection = list(method = "TIF", targetValue = c(40, 40, 40), targetLocation = c(0, 1, 2)))

  solution <- ATA(config.fatigue, constraints.fatigue, plot = T)
  solution$Selected

  setEPS()
  postscript("R/fatigue.tif.eps", width = 8, height = 3.25)
  solution$plot
  dev.off()


  # Fatigue - generalized shadow test

  set.seed(1)

  trueTheta <- runif(1, min = -3.5, max = 3.5)
  respData <- MakeTest(itempool.fatigue, trueTheta = trueTheta)@Data

  config.fatigue <- config.Shadow(refreshPolicy = list(
    method = "POSITION",
    position = c(1, 5, 9)
  ))

  solution <- Shadow(itempool.fatigue, config.fatigue, trueTheta, constraints.fatigue, Data = respData)

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
