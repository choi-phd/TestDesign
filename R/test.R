if(FALSE){
  library(oat)

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
  trueTheta = runif(10, min = -3.5, max = 3.5)
  testData = MakeTest(itempool.science, thetaGrid, infoType = "FISHER", trueTheta = trueTheta)
  respData = testData@Data

  config.science = config.Shadow()

  solution = Shadow(itempool.science, config.science,
               trueTheta, constraints.science, Data = respData)

  setEPS()
  postscript("example-plotcat.eps", width = 8, height = 8)
  p = plotCAT(solution, examineeID = 1)
  print(p)
  dev.off()

  setEPS()
  postscript("R/science.shadowplot.eps", width = 8, height = 10)
  p = plotShadow(solution, constraints.science, examineeID = 1)
  p
  dev.off()

  # Manuscript example: adaptive test assembly (reading)

  set.seed(1)

  thetaGrid = seq(-3, 3, 1)
  trueTheta = runif(1, min = -3.5, max = 3.5)
  testData = MakeTest(itempool.reading, thetaGrid, infoType = "FISHER", trueTheta = trueTheta)
  respData = testData@Data

  conf_rd = config.ATA(itemSelection = list(method = "TCC",
                                         targetLocation = c(-1, 0, 1),
                                         targetValue = c(15,20,25)))

  fit = ATA(conf, constrnt_rd, plot = T)
  fit$Selected
  fit$plot

  set.seed(1)

  thetaGrid = seq(-3, 3, 1)
  trueTheta = runif(1, min = -3.5, max = 3.5)
  testData = MakeTest(itempool.reading, thetaGrid, infoType = "FISHER", trueTheta = trueTheta)
  respData = testData@Data

  config.reading = config.Shadow()

  solution = Shadow(itempool.reading, config.reading, trueTheta, constraints.reading, Data = respData)

  setEPS()
  postscript("example-plotcat.eps", width = 8, height = 8)
  p = plotCAT(solution, 1)
  p
  dev.off()

  setEPS()
  postscript("R/reading.shadowplot.eps", width = 8, height = 10)
  p = plotShadow(solution, constraints.reading, 1)
  p
  dev.off()




  # Example 3
  write.csv(par_fatigue, "par_ft.csv", row.names = F)
  write.csv(item_attrib_fatigue, "item_attrib_ft.csv", row.names = F)
  write.csv(constraints_fatigue, "constraints_ft.csv", row.names = F)

  itempool_ft = LoadItemPool("par_ft.csv")
  itemattr_ft = LoadItemAttrib("item_attrib_ft.csv", itempool_ft)
  constrnt_ft = LoadConstraints("constraints_ft.csv", itempool_ft, itemattr_ft)


  thetas = c(0)
  target = c(15)

  conf@itemSelection$method = "TCC"
  conf@itemSelection$targetLocation = thetas
  conf@itemSelection$targetValue = target
  conf@itemSelection$targetWeight = rep(1, length(thetas))

  config = conf
  Constraints = constA


  fit = ATA(conf, constA, T)
  fit$plot
  fit$Selected


  thetas = c(0,1)
  target = c(10,15)

  conf@itemSelection$method = "TCC"
  conf@itemSelection$targetLocation = thetas
  conf@itemSelection$targetValue = target
  conf@itemSelection$targetWeight = rep(1, length(thetas))

  fit = ATA(conf, constA, T)



  thetas = c(0,1)
  target = c(17,20)

  conf@itemSelection$method = "TIF"
  conf@itemSelection$targetLocation = thetas
  conf@itemSelection$targetValue = target
  conf@itemSelection$targetWeight = rep(1, length(thetas))

  fit = ATA(conf, constA, T)
  fit$plot


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


