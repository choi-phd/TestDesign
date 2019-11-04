if (FALSE) {

  # Manuscript figures

  config_science <- createStaticTestConfig(
    item_selection = list(
      method = "MAXINFO",
      target_location = c(-1, 0, 1)
  ))

  solution <- Static(config_science, constraints_science)
  solution$selected[['INDEX']]

  setEPS()
  postscript("figures/science_maxinfo.eps", width = 8, height = 5)
  p <- plotInfo(solution)
  print(p)
  dev.off()


  config_science <- createStaticTestConfig(
    item_selection = list(
      method = "TCC",
      target_value = c(15, 20, 25),
      target_location = c(-1, 0, 1)
  ))

  solution <- Static(config_science, constraints_science)

  solution$selected[["INDEX"]]

  setEPS()
  postscript("figures/science_tcc.eps", width = 8, height = 5)
  p <- plotInfo(solution)
  print(p)
  dev.off()




  setEPS()
  postscript("figures/science_inforange.eps", width = 8, height = 5)
  p <- plotInfo(constraints_science)
  print(p)
  dev.off()

  # Manuscript example: adaptive test assembly

  library(TestDesign)

  set.seed(1)

  true_theta     <- runif(1, min = -3.5, max = 3.5)
  resp_science   <- makeTest(itempool_science, true_theta = true_theta)@data
  config_science <- createShadowTestConfig()
  solution <- Shadow(config_science, constraints_science,
    true_theta, resp_science)

  setEPS()
  postscript("figures/science_auditplot.eps", width = 8, height = 7)
  p <- plotCAT(solution, examinee_id = 1, min_theta = -4, max_theta = 1.5)
  print(p)
  dev.off()

  setEPS()
  postscript("figures/science_shadowplot.eps", width = 8, height = 7)
  p <- plotShadow(solution, examinee_id = 1, simple = TRUE)
  print(p)
  dev.off()


  # Exposure control plot

  set.seed(1)

  true_theta <- runif(7000, min = -3.5, max = 3.5)
  resp_science <- makeTest(itempool_science, info_type = "FISHER", true_theta = true_theta)@data
  config_science <- createShadowTestConfig(exposure_control = list(method = "ELIGIBILITY"))
  constraints_science <- updateConstraints(constraints_science, off = c(14:20, 32:36))

  solution <- Shadow(config_science, constraints_science, true_theta, resp_science)

  tmp = numeric(7000)
  for(i in 1:7000) {
    tmp[i] = sum(solution$output[[i]]@solve_time)
  }
  sum(tmp) / 60
  mean(tmp)

  setEPS()
  postscript("figures/science_exposure.eps", width = 8, height = 5)
  par(mfrow = c(2, 4))
  p = plotExposure(solution, color = "black", color_final = "black")
  print(p)
  dev.off()



  # Manuscript example: fixed-length test assembly (reading)

  config_reading <- createStaticTestConfig(
    item_selection = list(
      method = "TCC",
      target_value = c(10, 20, 25),
      target_location = c(-1, 0, 1)
    ),
    MIP = list(solver = "LPSYMPHONY")
  )

  solution <- Static(config_reading, constraints_reading)
  solution$selected[["INDEX"]]

  setEPS()
  postscript("figures/reading_tif.eps", width = 8, height = 5)
  p <- plotInfo(solution)
  print(p)
  dev.off()


  # Manuscript example: adaptive test assembly (reading)

  set.seed(1)

  true_theta <- runif(1, min = -3.5, max = 3.5)
  resp_reading <- makeTest(itempool_reading, true_theta = true_theta)@data

  config_reading <- createShadowTestConfig(
    refresh_policy = list(
      method = "STIMULUS"
    ),
    MIP = list(solver = "LPSYMPHONY")
  )

  solution <- Shadow(config_reading, constraints_reading, true_theta, resp_reading)

  setEPS()
  postscript("figures/reading_auditplot.eps", width = 8, height = 7)
  p <- plotCAT(solution, 1, max_theta = 3)
  print(p)
  dev.off()

  setEPS()
  postscript("figures/reading_shadowplot.eps", width = 8, height = 7)
  p <- plotShadow(solution, 1, simple = TRUE)
  print(p)
  dev.off()


  # Fatigue

  set.seed(1)

  setEPS()
  postscript("figures/fatigue_infoplot.eps", width = 8, height = 5)
  p <- plotInfo(constraints_fatigue)
  print(p)
  dev.off()

  config_fatigue <- createStaticTestConfig(item_selection = list(method = "TIF", target_value = c(40, 40, 40), target_location = c(0, 1, 2)))

  solution <- Static(config_fatigue, constraints_fatigue)
  solution$selected[["INDEX"]]

  setEPS()
  postscript("figures/fatigue_tif.eps", width = 8, height = 5)
  p <- plotInfo(solution)
  print(p)
  dev.off()


  # Fatigue - generalized shadow test

  set.seed(1)

  true_theta <- runif(10, min = -3.5, max = 3.5)
  resp_fatigue <- makeTest(itempool_fatigue, true_theta = true_theta)@data

  config_fatigue <- createShadowTestConfig(refresh_policy = list(
    method = "POSITION",
    position = c(1, 5, 9)
  ))

  solution <- Shadow(config_fatigue, constraints_fatigue, true_theta, resp_fatigue)

  setEPS()
  postscript("figures/fatigue_auditplot.eps", width = 8, height = 7)
  p <- plotCAT(solution, 2)
  print(p)
  dev.off()

  setEPS()
  postscript("figures/fatigue_shadowplot.eps", width = 8, height = 7)
  p <- plotShadow(solution, 2, simple = FALSE)
  print(p)
  dev.off()


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

}
