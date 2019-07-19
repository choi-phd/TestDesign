if(FALSE){
  library(oat)


  # IRT models

  item.1 = new("item.1pl", difficulty = 0.5)
  item.2 = new("item.2pl", slope = 1.0, difficulty = 0.5)
  item.3 = new("item.3pl", slope = 1.0, difficulty = 0.5, guessing = -0.2)
  item.4 = new("item.pc", threshold = c(-0.5, 0.5), ncat = 3)
  item.5 = new("item.gpc", slope = 1.0, threshold = c(-0.5, 0.0, 0.5), ncat = 4)
  item.6 = new("item.gr", slope = 1.0, category = c(-1, 0, 1), ncat = 4)

  calcProb(item.1, 0)
  calcFisher(item.2, 0)
  calcEscore(item.3, 0.5)
  calcDerivative(item.4, 1)
  calcDerivative2(item.5, 1)
  calcJacobian(item.6, 0, resp = 1)
  calcHessian(item.6, 0, resp = 1)

  # Manuscript figures

  config.science = config.ATA(itemSelection = list(method = "MAXINFO", targetLocation = c(-1, 0, 1)))
  solution = ATA(config.science, constraints.science, plot = T)
  solution$Selected

  setEPS()
  postscript("R/science.maxinfo.eps", width = 8, height = 5)
  solution$plot
  dev.off()


  config.science = config.ATA(itemSelection = list(method = "TCC", targetValue = c(15, 20, 25), targetLocation = c(-1, 0, 1)))
  solution = ATA(config.science, constraints.science, plot = T)
  solution$Selected

  setEPS()
  postscript("R/science.tcc.eps", width = 8, height = 5)
  solution$plot
  dev.off()

  # Manuscript example: adaptive test assembly

  set.seed(1)

  thetaGrid = seq(-3, 3, 1)
  trueTheta = runif(1, min = -3.5, max = 3.5)
  testData = MakeTest(itempool.science, thetaGrid, infoType = "FISHER", trueTheta = trueTheta)
  respData = testData@Data

  config.science = config.Shadow()

  solution = Shadow(itempool.science, config.science,
               trueTheta, constraints.science, Data = respData)

  setEPS()
  postscript("R/science.auditplot.eps", width = 8, height = 4.5)
  p = plotCAT(solution, examineeID = 1)
  print(p)
  dev.off()

  setEPS()
  postscript("R/science.shadowplot.eps", width = 8, height = 9.8)
  p = plotShadow(solution, constraints.science, examineeID = 1)
  p
  dev.off()

  # Manuscript example: adaptive test assembly (reading)

  config.reading = config.ATA(itemSelection = list(method = "MAXINFO", targetLocation = c(-1, 0, 1)))

  solution = ATA(config.reading, constraints.reading, plot = T)
  solution$Selected
  solution$plot

  set.seed(1)

  thetaGrid = seq(-3, 3, 1)
  trueTheta = runif(1, min = -3.5, max = 3.5)
  testData = MakeTest(itempool.reading, thetaGrid, infoType = "FISHER", trueTheta = trueTheta)
  respData = testData@Data

  config.reading = config.Shadow()

  solution = Shadow(itempool.reading, config.reading, trueTheta, constraints.reading, Data = respData)

  setEPS()
  postscript("R/reading.auditplot.eps", width = 8, height = 8)
  p = plotCAT(solution, 1)
  p
  dev.off()

  setEPS()
  postscript("R/reading.shadowplot.eps", width = 8, height = 8.5)
  p = plotShadow(solution, constraints.reading, 1)
  p
  dev.off()


  # Fatigue

  setEPS()
  postscript("R/fatigue.infoplot.eps", width = 8, height = 5)
  p = maxinfoplot(itempool.fatigue, constraints.fatigue)
  print(p)
  dev.off()

  config.fatigue = config.ATA(itemSelection = list(method = "TIF", targetValue = c(40, 40, 40), targetLocation = c(0, 1, 2)))

  solution = ATA(config.fatigue, constraints.fatigue, plot = T)
  solution$Selected

  setEPS()
  postscript("R/fatigue.tif.eps", width = 8, height = 5)
  solution$plot
  dev.off()


  # Fatigue - generalized shadow test

  set.seed(1)

  thetaGrid = seq(-4, 4, 1)
  trueTheta = runif(1, min = -3.5, max = 3.5)
  respData = MakeTest(itempool.fatigue, thetaGrid, infoType = "FISHER", trueTheta = trueTheta)@Data

  config.fatigue = config.Shadow(refreshPolicy = list(
    method = "POSITION",
    position = c(1, 5, 9)
  ))

  solution = Shadow(itempool.fatigue, config.fatigue, trueTheta, constraints.fatigue, Data = respData)

  setEPS()
  postscript("R/fatigue.auditplot.eps", width = 8, height = 8)
  p = plotCAT(solution, 1)
  p
  dev.off()

  setEPS()
  postscript("R/fatigue.shadowplot.eps", width = 8, height = 5)
  p = plotShadow(solution, constraints.fatigue, 1)
  p
  dev.off()


  # Debug

  library(oat)
  set.seed(1)

  thetaGrid = seq(-4, 4, 1)
  trueTheta = runif(1, min = -3.5, max = 3.5)
  resp.science = MakeTest(itempool.science, thetaGrid, infoType = "FISHER",trueTheta = trueTheta)@Data

  config.science = config.Shadow(MIP = list(solver = "SYMPHONY"), exposureControl = list(method = "ELIGIBILITY"))

  c1 = UpdateConstraints(constraints.science, off = 14)

  solution = Shadow(itempool.science, config.science, trueTheta, constraints.science, Data = resp.science)

  object = itempool.science
  config = config.science
  Constraints = constraints2.science
  Data = resp.science
  prior = NULL
  priorPar = NULL
  session = NULL




  # Paper Note: ATA with specifying a target amount of information
  # to prevent item exposure and information overuse

  # Item Content Parsing

  itempoolA = LoadItemPool("../Shadow examples/item_pool_Fatigue.csv")
  itemattribA = LoadItemAttrib("../Shadow examples/item_attrib_Fatigue.csv", itempoolA)
  constA = LoadConstraints("../Shadow examples/constraints_Fatigue_3d.csv", itempoolA, itemattribA)

  conf = new("ATA.config")
  conf@MIP$solver = "symphony"

  thetas = c(1,2)

  conf@itemSelection$method = "MAXINFO"
  conf@itemSelection$targetLocation = thetas
  conf@itemSelection$targetWeight = rep(1, length(thetas))

  fit = ATA(conf, constA, T)
  idx = which(fit$MIP$solution == 1)
  n = itemattribA[idx,][['ID']]

  library(readxl)
  d = read_excel("../Shadow examples/bank_Fatigue.xls")
  class(d)[1] == "tbl_df"
  idx.from.contents = d$ID %in% n
  paste0(d[idx.from.contents,][['ID']], ") ", d[idx.from.contents,][['Content']])

  # Stimulus based ATA

  itempool = LoadItemPool("data-raw/item_params_stimbased.csv")
  itemattrib = LoadItemAttrib("data-raw/item_attribs_stimbased.csv", itempool)
  stimattrib = LoadStAttrib("data-raw/stim_attribs_stimbased.csv", itemattrib)
  const = LoadConstraints("data-raw/constraints_stimbased.csv", itempool, itemattrib, stimattrib)

  conf = new("ATA.config")
  conf@MIP$solver = "symphony"

  thetas = c(1,2)
  target = c(10,15)

  conf@itemSelection$method = "MAXINFO"
  conf@itemSelection$targetLocation = thetas
  conf@itemSelection$targetValue    = target
  conf@itemSelection$targetWeight = rep(1, length(thetas))

  config = conf
  Constraints = const
  plot = T
  plotrange = c(-3,3)

  fit = ATA(conf, const, T)
  fit$plot



  # Stimulus based SHADOW

  itempool = LoadItemPool("data-raw/item_params_stimbased.csv")
  itemattrib = LoadItemAttrib("data-raw/item_attribs_stimbased.csv", itempool)
  stimattrib = LoadStAttrib("data-raw/stim_attribs_stimbased.csv", itemattrib)
  const = LoadConstraints("data-raw/constraints_stimbased.csv", itempool, itemattrib, stimattrib)

  thetaGrid = seq(-3, 3, 1)
  trueTheta = runif(1, min = -3.5, max = 3.5)
  testData = MakeTest(itempool, thetaGrid, infoType = "FISHER", trueTheta = trueTheta)
  respData = testData@Data

  nSegment = 7
  plotConfig = c(2, 4)

  conf = new("Shadow.config")
  conf@exposureControl$fadingFactor = .999
  conf@exposureControl$accelerationFactor = 2
  conf@exposureControl$method = "ELIGIBILITY"
  conf@exposureControl$diagnosticStats = TRUE
  conf@interimTheta$method = "EAP"
  conf@interimTheta$priorDist = "NORMAL"
  conf@interimTheta$priorPar = c(0, 2)
  conf@finalTheta$method = "EAP"
  conf@finalTheta$priorDist = "NORMAL"
  conf@finalTheta$priorPar = c(0, 2)

  object = itempool
  config = conf
  Constraints = const
  prior = NULL
  priorPar = c(0,1)
  Data = respData

  fit = try(Shadow(itempool, conf, trueTheta, Constraints = const, prior = NULL, priorPar = c(0, 1), Data = respData))


  names(fit)
  fit$output[[1]]@trueTheta

  class(fit$output)
  class(fit)


  # ATA maxinfo plot
  itempoolA = LoadItemPool("data-raw/par_Pool_A.csv")
  itemattribA = LoadItemAttrib("data-raw/attrib_Pool_A.csv", itempoolA)
  constA = LoadConstraints("data-raw/constraints_1.csv", itempoolA, itemattribA)

  conf = new("ATA.config")
  conf@MIP$solver = "symphony"

  idx.nitems = which(toupper(constA$Constraints[['WHAT']]) == "ITEM" &
                     toupper(constA$Constraints[['CONDITION']]) == "")
  n.items = constA$Constraints[idx.nitems,]['LB'][1,1]



  continuum = seq(-3, 3, .5)
  max.info = min.info = continuum * 0

  for(i in 1:length(continuum)){
    max.info[i] = sum(sort(calcFisher(itempoolA, continuum[i]), T)[1:n.items])
    min.info[i] = sum(sort(calcFisher(itempoolA, continuum[i]), F)[1:n.items])
  }

  plot(0, 0, type = 'n', xlim = c(-3,3), ylim = c(0,max(max.info)),
       xlab = 'theta', ylab = 'Information')
  lines(continuum, max.info, lty = 2, lwd = 2)
  lines(continuum, min.info, lty = 2, lwd = 2)
  grid()

  thetas = c(1,2)
  target = c(15,20)

  conf@itemSelection$method = "MAXINFO"
  conf@itemSelection$targetLocation = thetas
  conf@itemSelection$targetValue = target
  conf@itemSelection$targetWeight = rep(1, length(thetas))
}


