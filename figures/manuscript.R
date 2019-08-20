if (FALSE) {
  # IRT models

  item_1 <- new("item_1PL", difficulty = 0.5)
  item_2 <- new("item_2PL", slope = 1.0, difficulty = 0.5)
  item_3 <- new("item_3PL", slope = 1.0, difficulty = 0.5, guessing = 0.2)
  item_4 <- new("item_PC", threshold = c(-0.5, 0.5), ncat = 3)
  item_5 <- new("item_GPC", slope = 1.0, threshold = c(-0.5, 0.0, 0.5), ncat = 4)
  item_6 <- new("item_GR", slope = 1.0, category = c(-1, 0, 1), ncat = 4)

  calcProb(item_1, 0)
  calcFisher(item_2, 0)
  calcEscore(item_3, 0.5)
  calcDerivative(item_4, 1)
  calcDerivative2(item_5, 1)
  calcJacobian(item_6, 0, resp = 1)
  calcHessian(item_6, 0, resp = 1)

  # Manuscript figures

  config_science <- createStaticTestConfig(item_selection = list(method = "MAXINFO", target_location = c(-1, 0, 1)))
  solution <- ATA(config_science, constraints_science, plot = T)
  solution$selected

  setEPS()
  postscript("R/science_maxinfo.eps", width = 8, height = 3.25)
  solution$plot
  dev.off()


  config_science <- createStaticTestConfig(item_selection = list(method = "TCC", target_value = c(15, 20, 25), target_location = c(-1, 0, 1)))
  solution <- ATA(config_science, constraints_science, plot = T)
  solution$selected

  setEPS()
  postscript("R/science_tcc.eps", width = 8, height = 3.25)
  solution$plot
  dev.off()

  # Exposure control plot

  set.seed(1)

  true_theta <- runif(70, min = -3.5, max = 3.5)
  resp_science <- makeTest(itempool_science, info_type = "FISHER", true_theta = true_theta)@data
  config_science <- createShadowTestConfig(MIP = list(solver = "LPSOLVE"), exposure_control = list(method = "ELIGIBILITY"))
  constraints_science <- updateConstraints(constraints_science, off = c(14:20, 32:36))

  solution <- Shadow(itempool_science, config_science, true_theta, constraints_science, data = resp_science)

  setEPS()
  postscript("R/science_exposure.eps", width = 8, height = 4.25)
  par(mfrow = c(2, 4))
  plotExposure(solution)
  dev.off()


  # Manuscript example: adaptive test assembly

  set.seed(1)

  true_theta     <- runif(1, min = -3.5, max = 3.5)
  resp_science   <- makeTest(itempool_science, true_theta = true_theta)@data
  config_science <- createShadowTestConfig()
  solution <- Shadow(itempool_science, config_science,
    true_theta, constraints_science,
    data = resp_science
  )

  setEPS()
  postscript("R/science_shadowplot.eps", width = 8, height = 9.7)
  p <- plotShadow(solution, constraints_science, examinee_id = 1)
  p
  dev.off()

  setEPS()
  postscript("R/science_auditplot.eps", width = 8, height = 4.2)
  p <- plotCAT(solution, examinee_id = 1, min_theta = -4, max_theta = 1.5)
  print(p)
  dev.off()



  # Manuscript example: adaptive test assembly (reading)

  config_reading <- createStaticTestConfig(item_selection = list(method = "MAXINFO", target_location = c(-1, 0, 1)))

  solution <- ATA(config_reading, constraints_reading, plot = T)
  solution$selected
  solution$plot

  set.seed(1)

  true_theta <- runif(1, min = -3.5, max = 3.5)
  resp_reading <- makeTest(itempool_reading, true_theta = true_theta)@data

  config_reading <- createShadowTestConfig(refresh_policy = list(
    method = "STIMULUS"
  ))

  solution <- Shadow(itempool_reading, config_reading, NULL, constraints_reading, data = resp_reading)

  setEPS()
  postscript("R/reading_auditplot.eps", width = 8, height = 4.2)
  p <- plotCAT(solution, 1, max_theta = 3)
  p
  dev.off()

  setEPS()
  postscript("R/reading_shadowplot.eps", width = 8, height = 9)
  p <- plotShadow(solution, constraints_reading, 1)
  p
  dev.off()


  # Fatigue

  setEPS()
  postscript("R/fatigue_infoplot.eps", width = 8, height = 3.25)
  p <- plotMaxInfo(itempool_fatigue, constraints_fatigue)
  print(p)
  dev.off()

  config_fatigue <- createStaticTestConfig(item_selection = list(method = "TIF", target_value = c(40, 40, 40), target_location = c(0, 1, 2)))

  solution <- ATA(config_fatigue, constraints_fatigue, plot = T)
  solution$selected

  setEPS()
  postscript("R/fatigue_tif.eps", width = 8, height = 3.25)
  solution$plot
  dev.off()


  # Fatigue - generalized shadow test

  set.seed(1)

  true_theta <- runif(10, min = -3.5, max = 3.5)
  resp_fatigue <- makeTest(itempool_fatigue, true_theta = true_theta)@data

  config_fatigue <- createShadowTestConfig(refresh_policy = list(
    method = "POSITION",
    position = c(1, 5, 9)
  ))

  solution <- Shadow(itempool_fatigue, config_fatigue, true_theta, constraints_fatigue, data = resp_fatigue)

  setEPS()
  postscript("R/fatigue.auditplot.eps", width = 8, height = 8)
  p <- plotCAT(solution, 1)
  p
  dev.off()

  setEPS()
  postscript("figures/fatigue_shadowplot.eps", width = 8, height = 6)
  p <- plotShadow(solution, constraints_fatigue, 2)
  p
  dev.off()

}
