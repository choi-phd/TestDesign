if(FALSE){
  library(Shadow)
  
  # Debug
  
  itempool = LoadItemPool("par_Pool_300.csv", se.file.csv = "se_Pool_300.csv")
  itemattr = LoadItemAttrib("attrib_Pool_300.csv", itempool)
  constrnt = LoadConstraints("constraints_1.csv", itempool, itemattr)
  
  config = config.Shadow(interimTheta = list(method = "FB"),
                         finalTheta = list(method = "FB"))
  
  set.seed(1)
  
  thetaGrid = seq(-3, 3, 1)
  trueTheta = runif(1, min = -3.5, max = 3.5)
  testData = MakeTest(itempool, thetaGrid, infoType = "FISHER", trueTheta = trueTheta)
  respData = testData@Data
  
  object = itempool
  Constraints = constrnt
  prior = NULL
  priorPar = c(0, 1)
  Data = NULL
  
  fit = Shadow(itempool, config,
               trueTheta, constrnt, Data = respData)
  
  
  # now run Shadow line by line.
  # Error in theta_FB_single(nSample, currentTheta, currentSE, iparList[[currentItem]],  : 
  # could not find function "theta_FB_single"
  
  # Example 1
  
  write.csv(par_science, "par_sc.csv", row.names = F)
  write.csv(item_attrib_science, "item_attrib_sc.csv", row.names = F)
  write.csv(constraints_science, "constraints_sc.csv", row.names = F)
  
  itempool_sc = LoadItemPool("par_sc.csv")
  itemattr_sc = LoadItemAttrib("item_attrib_sc.csv", itempool_sc)
  constrnt_sc = LoadConstraints("constraints_sc.csv", itempool_sc, itemattr_sc)
  
  conf_sc = config.ATA(itemSelection = list(method = "MAXINFO",targetLocation = c(-1, 0, 1)))
  
  fit = ATA(conf, constrnt_sc, plot = T)
  
  fit$Selected
  
  setEPS()
  postscript("example-1.eps", width = 8, height = 5)
  fit$plot
  dev.off()
  
  set.seed(1)
  
  thetaGrid = seq(-3, 3, 1)
  trueTheta = runif(1, min = -3.5, max = 3.5)
  testData = MakeTest(itempool_sc, thetaGrid, infoType = "FISHER", trueTheta = trueTheta)
  respData = testData@Data
  
  conf2_sc = config.Shadow()
  
  fit = Shadow(itempool_sc, conf2_sc,
               trueTheta, constrnt_sc, Data = respData)
  
  setEPS()
  postscript("example-plotcat.eps", width = 8, height = 8)
  p = plotCAT(fit$output[[1]])
  p
  dev.off()
  
  setEPS()
  postscript("example-plotshadow.eps", width = 8, height = 8)
  p = plotShadow(fit$output[[1]], constrnt_sc)
  p
  dev.off()
  
  # Example 2
  
  write.csv(par_reading, "par_rd.csv", row.names = F)
  write.csv(item_attrib_reading, "item_attrib_rd.csv", row.names = F)
  write.csv(stimulus_attrib_reading, "stimulus_attrib_rd.csv", row.names = F)
  write.csv(constraints_reading, "constraints_rd.csv", row.names = F)
  
  itempool_rd = LoadItemPool("par_rd.csv")
  itemattr_rd = LoadItemAttrib("item_attrib_rd.csv", itempool_rd)
  stimattr_rd = LoadStAttrib("stimulus_attrib_rd.csv", itemattr_rd)
  constrnt_rd = LoadConstraints("constraints_rd.csv",
                                   itempool_rd, itemattr_rd, stimattr_rd)
  
  conf_rd = config.ATA(itemSelection = list(method = "TCC",
                                         targetLocation = c(-1, 0, 1),
                                         targetValue = c(15,20,25)))
  
  fit = ATA(conf, constrnt_rd, plot = T)
  fit$Selected
  fit$plot
  
  set.seed(1)
  
  thetaGrid = seq(-3, 3, 1)
  trueTheta = runif(1, min = -3.5, max = 3.5)
  testData = MakeTest(itempool_rd, thetaGrid, infoType = "FISHER", trueTheta = trueTheta)
  respData = testData@Data
  
  conf2_rd = config.Shadow()
  
  fit = Shadow(itempool_rd, conf2_rd,
               trueTheta, constrnt_rd, Data = respData)
  
  setEPS()
  postscript("example-plotcat.eps", width = 8, height = 8)
  p = plotCAT(fit$output[[1]])
  p
  dev.off()
  
  plotShadow(fit$output[[1]], constrnt_sc)
  
  
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


