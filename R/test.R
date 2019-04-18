if(FALSE){
  library(Shadow)
  
  # ATA testing (Github repository version), working fine
  itempoolA = LoadItemPool("data-raw/item_params_A.csv")
  iparPosteriorSample_which(itempoolA, 10)
  
  itemattribA = LoadItemAttrib("data-raw/item_attribs_A.csv", itempoolA)
  constA = LoadConstraints("data-raw/constraints_0.csv", itempoolA, itemattribA)
  
  conf = new("ATA.config")
  conf@MIP$solver = "symphony"
  
  thetas = c(1,2)
  target = c(15,20)
  
  conf@itemSelection$method = "MAXINFO"
  conf@itemSelection$targetLocation = thetas
  conf@itemSelection$targetValue = target
  conf@itemSelection$targetWeight = rep(1, length(thetas))
  
  
  fit = ATA(conf, constA, T)
  fit$plot

  
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


