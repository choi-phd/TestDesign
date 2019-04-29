#' @import Rcpp methods
#' @importFrom Rcpp evalCpp
#' @importFrom utils read.csv
#' @importFrom methods new validObject
#' @importFrom Rdpack reprompt
NULL

#' Load item parameters from a .csv file or a data.frame and create an item.pool class
#' 
#' @param file.csv File path of a .csv file containing item parameters. The file content should not have column names.
#' @param ipar A data.frame created from a .csv file.
#' @param se.file.csv File path of a .csv file containing standard errors.
#' @return An \linkS4class{item.pool} object.
#' @export
#' @examples  
#' \dontrun{
#' itemPool.1 = LoadItemPool("C:/sample.csv")
#' ipar.1 = read.csv("C:/sample.csv")
#' itemPool.1 = LoadItemPool(ipar = ipar.1)
#' }

LoadItemPool = function(file.csv, ipar = NULL, se.file.csv = NULL) {
  if (is.null(ipar)) {
    ipar = read.csv(file.csv, header = FALSE, as.is = TRUE)
  }
  pool = new("item.pool")
  ni = nrow(ipar)
  pool@index = 1:ni
  pool@ID = as.character(ipar[[1]]) 
  model = ipar[[2]]
  NCAT = numeric(ni)
  parms = vector(mode = "list", length = ni)
  nfields = rowSums(!is.na(ipar))
  valid = logical(ni)
  pool@ipar = matrix(NA, nrow = ni, ncol = max(nfields) - 2)
  if (!is.null(se.file.csv)) {
    ipar.se = read.csv(se.file.csv, header = FALSE, as.is = TRUE)
    loadSE = TRUE
    SEs = matrix(NA, nrow = ni, ncol = max(nfields) - 2)
  } else {
    loadSE = FALSE
  }
  for (i in 1:ni) {
    if (model[i] == 1 | model[i] == "1PL") { 
      NCAT[i] = 2
      b = ipar[[3]][i]
      pool@model[i] = "item.1pl"
      parms[[i]] = new("item.1pl", difficulty = b)
      valid[i] = TRUE
      pool@ipar[i, 1] = b
      if (loadSE) {
        SEs[i, 1] = ipar.se[[3]][i]
      }
    } else if (model[i] == 2 | model[i] == "2PL") { 
      NCAT[i] = 2
      a = ipar[[3]][i]
      b = ipar[[4]][i]
      if (a > 0) {
        pool@model[i] = "item.2pl"
        parms[[i]] = new("item.2pl", slope = a, difficulty = b)
        valid[i] = TRUE
        pool@ipar[i, 1:2] = c(a, b)
        if (loadSE) {
          SEs[i, 1:2] = c(ipar.se[[3]][i], ipar.se[[4]][i])
        }
      }
    } else if (model[i] == 3 | model[i] == "3PL") {
      NCAT[i] = 2
      a = ipar[[3]][i]
      b = ipar[[4]][i]
      c = ipar[[5]][i]
      if (a > 0 && c >= 0 && c < 1) {
        pool@model[i] = "item.3pl"
        parms[[i]] = new("item.3pl", slope = a, difficulty = b, guessing = c)
        valid[i] = TRUE
        pool@ipar[i, 1:3] = c(a, b, c)
        if (loadSE) {
          SEs[i, 1:3] = c(ipar.se[[3]][i], ipar.se[[4]][i], ipar.se[[5]][i])
        }
      }
    } else if (model[i] == 4 | model[i] == "PC") {
      NCAT[i] = nfields[i] - 1
      b = as.numeric(ipar[i, 3:nfields[i]])
      pool@model[i] = "item.pc"
      parms[[i]] = new("item.pc", threshold = b, ncat = NCAT[i])
      valid[i] = TRUE
      pool@ipar[i, 1:(NCAT[i] - 1)] = b
      if (loadSE) {
          SEs[i, 1:(NCAT[i] - 1)] = ipar.se[i, 3:nfields[i]]
      }
    } else if (model[i] == 5 | model[i] == "GPC") {
      NCAT[i] = nfields[i] - 2
      a = as.numeric(ipar[[3]][i])
      b = as.numeric(ipar[i, 4:nfields[i]])
      if (a > 0) {
        pool@model[i] = "item.gpc"
        parms[[i]] = new("item.gpc", slope = a, threshold = b, ncat = NCAT[i])
        valid[i] = TRUE
        pool@ipar[i, 1:NCAT[i]] = c(a, b)
        if (loadSE) {
            SEs[i, 1:NCAT[i]] = c(ipar.se[[3]][i], as.numeric(ipar.se[i, 4:nfields[i]]))
        }
      }
    } else if (model[i] == 6 | model[i] == "GR") {
      NCAT[i] = nfields[i] - 2
      a = as.numeric(ipar[[3]][i])
      b = as.numeric(ipar[i, 4:nfields[i]]) 
      if (a > 0 && (!is.unsorted(b))) {
        pool@model[i] = "item.gr"
        parms[[i]] = new("item.gr", slope = a, category = b, ncat = NCAT[i])
        valid[i] = TRUE
        pool@ipar[i, 1:NCAT[i]] = c(a, b)
        if (loadSE) {
          SEs[i, 1:NCAT[i]] = c(ipar[[3]][i], as.numeric(ipar.se[i, 4:nfields[i]]))
        }
      }
    } else stop(paste("Item", i, ": unknown IRT model specified - valid models are 1: 1PL, 2: 2PL, 3: 3PL, 4: PC, 5: GPC, or 6: GR"))
  }
  if (sum(!valid) > 0) stop(paste("Check the parameters for the following item(s):", paste((1:ni)[!valid], collapse = ", "), "\n"))
  pool@ni = ni
  pool@maxCat = max(NCAT)
  pool@NCAT = NCAT
  pool@parms = parms
  
  if (loadSE) {
    pool@SEs = SEs
  }

  if (max(rowSums(!is.na(pool@ipar))) != max(nfields) - 2) {
    pool@ipar = pool@ipar[, 1:max(rowSums(!is.na(pool@ipar)))]
  }
  if (validObject(pool)) return(pool)
}
