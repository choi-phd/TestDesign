#' Load item paramaters
#'
#' Read item parameters from a .csv file or a data.frame and create an \linkS4class{item.pool} class.
#'
#' @param file.csv File path of a .csv file containing item parameters. The file content should not have column names.
#' @param ipar A data.frame created from a .csv file.
#' @param se.file.csv File path of a .csv file containing standard errors.
#' @return An \linkS4class{item.pool} object.
#'
#' @seealso \link{dataset.science} for example usage.
#' @export
LoadItemPool = function(file.csv, ipar = NULL, se.file.csv = NULL) {
  if (is.null(ipar)) {
    ipar = read.csv(file.csv, header = TRUE, as.is = TRUE)
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
    ipar.se = read.csv(se.file.csv, header = TRUE, as.is = TRUE)
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
  if (sum(!valid) > 0) {
    stop(paste("Check the parameters for the following item(s):", paste((1:ni)[!valid], collapse = ", "), "\n"))
  }

  pool@ni = ni
  pool@maxCat = max(NCAT)
  pool@NCAT = NCAT
  pool@parms = parms

  if (loadSE) {
    pool@SEs = SEs
  } else {
    pool@SEs = pool@ipar * 0
  }
  if (max(rowSums(!is.na(pool@ipar))) != max(nfields) - 2) {
    pool@ipar = pool@ipar[, 1:max(rowSums(!is.na(pool@ipar)))]
  }

  if (validObject(pool)) {
    return(pool)
  }
}

#' Load item attributes
#'
#' Read item attributes from specified file.
#'
#' @param file.csv Character. The name of the file containing item attributes.
#' @param pool An \code{\linkS4class{item.pool}} object. Use \code{\link{LoadItemPool}} for this.
#'
#' @return A \code{data.frame} containing parsed dataset.
#'
#' @seealso \link{dataset.science} for example usage.
#' @export

LoadItemAttrib = function(file.csv, pool) {
  if (is.null(pool) || class(pool) != "item.pool") {
    stop("pool is missing or not of class \"item.pool\"")
  }
  ItemAttrib = read.csv(file.csv, header = TRUE, as.is = TRUE)
  names(ItemAttrib) = toupper(names(ItemAttrib))
  if (pool@ni != nrow(ItemAttrib)) {
    stop("nrow of item attrib file not equal to pool@ni")
  }
  if (!("ID" %in% names(ItemAttrib))) {
    stop("no column name \"ID\" found in ItemAttrib")
  }
  if (is.numeric(ItemAttrib$ID)) {
    ItemAttrib$ID = as.character(ItemAttrib$ID)
  }
  if (any(ItemAttrib$ID %in% c("", " ", "NA", "N/A"))) {
    stop("invalid ID entries were found in ItemAttrib")
  }
  if (length(unique(ItemAttrib$ID)) != nrow(ItemAttrib)) {
    stop("duplicate ID entries were found in item attrib")
  } else if (!all(sort(pool@ID) == sort(ItemAttrib$ID))) {
    stop("ID entries in item attrib do not match pool@ID")
  } else if (!all(pool@ID == ItemAttrib$ID)) {
    ItemAttrib = merge(data.frame(ID = pool@ID), ItemAttrib, by = "ID")[, names(ItemAttrib)] #re-ordering cols in attrib
  }
  ItemAttrib = data.frame(cbind(INDEX = 1:nrow(ItemAttrib), ItemAttrib))
  if ("STID" %in% names(ItemAttrib)) {
    if (any(ItemAttrib$STID %in% c("", " ", "N/A"))) {
      ItemAttrib$STID[ItemAttrib$STID %in% c("", " ", "N/A")] = NA
    }
  }
  return(ItemAttrib)
}

#' Load set/stimulus/passage attributes
#'
#' Read set, stimulus, or passage attributes from specified file.
#'
#' @param file.csv Character. The name of the file containing item attributes.
#' @param ItemAttrib A \code{data.frame} containing item attributes. Use \code{\link{LoadItemAttrib}} for this.
#'
#' @return A \code{data.frame} containing stimulus attributes.
#'
#' @seealso \link{dataset.reading} for example usage.
#'
#' @export

LoadStAttrib = function(file.csv, ItemAttrib) {
  StAttrib = read.csv(file.csv, header = TRUE, as.is = TRUE)
  names(StAttrib) = toupper(names(StAttrib))
  if (!("STID" %in% names(StAttrib))) {
    stop("no column name \"STID\" found in set/stimulus/passage attrib")
  }
  if (is.numeric(StAttrib$STID)) {
    StAttrib$STID = as.character(StAttrib$STID)
  }
  if (!("STIDNTEX" %in% names(StAttrib))) {
    StAttrib = data.frame(cbind(STINDEX = 1:nrow(StAttrib), StAttrib))
  }
  if (!("STID" %in% names(ItemAttrib))) {
    stop("no column name \"STID\" found in ItemAttrib")
  }
  if (any(StAttrib$STID %in% c("", " ", NA))) {
    stop("invalid STID found in StAttrib")
  }
  if (length(unique(StAttrib$STID)) != nrow(StAttrib)) {
    stop("duplicate ID entries were found in set/stimulus attrib")
  } else if (!(all(StAttrib$STID %in% unique(ItemAttrib$STID)))) {
    stop("not all STID found in ItemAttrib")
  } else if (any(ItemAttrib$STID %in% c("", " ", "NA", "N/A"))) {
    #if items with no valid STID present (e.g., discrete items)
    if (!all(sort(unique(ItemAttrib$STID[!(ItemAttrib$STID %in% c(NA, "", " ", "N/A"))])) == sort(StAttrib$STID))) {
      stop("StAttrib must include all STID in ItemAttrib")
    }
  }
  return(StAttrib)
}

#' Load constraints
#'
#' Read constraints from specified file.
#'
#' Use \code{vignette("constraints")} for instructions on how to create a constraint set object.
#'
#' @param file.csv Character. The name of the file containing specifications for constraints.
#' @param pool An \code{item.pool} object.
#' @param ItemAttrib A \code{data.frame} containing item attributes. Use \code{\link{LoadItemAttrib}} for this.
#' @param StAttrib (Optional) A \code{data.frame} containing stimulus attributes. Use \code{\link{LoadStAttrib}} for this.
#'
#' @return A list containing the parsed constraints, to be used in \code{\link{ATA}} and \code{\link{Shadow}}.
#'
#' @seealso \link{dataset.science} for example usage.
#'
#' @export

LoadConstraints = function(file.csv, pool, ItemAttrib, StAttrib = NULL) {
  if (class(pool) != "item.pool") {
    stop("pool must be of class \"item.pool\"")
  }

  # Read file ------------------------------------------------------------
  Constraints = read.csv(file.csv, header = TRUE)
  if ("ONOFF" %in% toupper(names(Constraints))) {
    ONOFF = TRUE
    Constraints = read.csv(file.csv, header = TRUE,
                           as.is = TRUE, colClasses = c("character","character","character","character",
                                                        "numeric","numeric", "character"),
                           stringsAsFactors = FALSE)
    Constraints$ONOFF = toupper(Constraints$ONOFF)
  } else {
    ONOFF = FALSE
    Constraints = read.csv(file.csv, header = TRUE,
                           as.is = TRUE, colClasses = c("character","character","character","character",
                                                        "numeric","numeric"),
                           stringsAsFactors = FALSE)
  }
  # Validation -----------------------------------------------------------
  # Validation: Column names
  names(Constraints) = toupper(names(Constraints))
  if (!all(names(Constraints) %in% c("CONSTRAINT", "TYPE", "WHAT", "CONDITION", "LB", "UB", "ONOFF"))) {
    stop("Column names must be CONSTRAINT, TYPE, WHAT, CONDITION, LB, UB, and ONOFF")
  }
  # Validation: Bounds
  if (!any(is.na(Constraints$LB) | is.na(Constraints$UB))) {
    if (any(Constraints$LB > Constraints$UB)) {
      stop("invalid LB/UB specified for one or more constraints (LB > UB)")
    }
  } else if (any(is.na(Constraints$LB) + is.na(Constraints$UB) == 1)) {
    stop("LB and UB should be both specified at the same time, or both omitted")
  }
  # Validation: Constraint types
  Constraints$TYPE = toupper(Constraints$TYPE)
  Constraints$WHAT = toupper(Constraints$WHAT)
  if (!all(Constraints$TYPE %in% c("NUMBER", "COUNT", "ALLORNONE", "ALL OR NONE", "IIF", "MUTUALLYEXCLUSIVE", "MUTUALLY EXCLUSIVE", "XOR", "ENEMY", "SUM", "AVERAGE", "MEAN", "INCLUDE", "EXCLUDE", "NOT", "ORDER"))) {
    stop("invalid TYPE specified")
  }
  # Validation: Pool
  ni = pool@ni
  ns = 0
  x = numeric(ni)
  nc = nrow(Constraints)
  if (nrow(ItemAttrib) != ni) {
    stop("nrow of ItemAttrib not equal to pool@ni")
  }
  if (!all(pool@ID == ItemAttrib$ID)) {
    stop("item IDs in pool and ItemAttrib not matching")
  }
  ListConstraints = vector(mode = "list", length = nc)
  ItemConstraints = which(Constraints$WHAT == "ITEM")
  StimulusConstraints = which(Constraints$WHAT %in% c("STIMULUS", "PASSAGE", "SET", "TESTLET"))
  ItemOrder     = ItemOrderBy     = NULL
  StimulusOrder = StimulusOrderBy = NULL
  if (length(StimulusConstraints) > 0) {
    if (is.null(StAttrib)) {
      stop("for stimulus-based, StAttrib must not be NULL")
    }
    if (!("STID" %in% names(ItemAttrib))) {
      stop("for stimulus-based, ItemAttrib must include \"STID\" ")
    }
    setBased = TRUE
    ID = c(ItemAttrib$ID, StAttrib$STID)
    ns = nrow(StAttrib)
    nv = ni + ns
    item.id.by.stimulus = split(ItemAttrib$ID, as.factor(ItemAttrib$STID))
    item.index.by.stimulus = lapply(item.id.by.stimulus, function(x) which(ItemAttrib$ID %in% x))
    item.index.by.stimulus = lapply(StAttrib$STID, function(x) item.index.by.stimulus[[x]])
    if (any(ItemAttrib$STID %in% c("", " ", "N/A", "n/a"))) {
      ItemAttrib$STID[ItemAttrib$STID %in% c("", " ", "N/A", "n/a")] = NA
    }
    stimulus.id.by.item = ItemAttrib$STID
    stimulus.index.by.item = sapply(stimulus.id.by.item, function(x) which(StAttrib$STID == x))
    if (any(toupper(Constraints$CONDITION) %in% c("PER STIMULUS", "PER PASSAGE", "PER SET", "PER TESTLET"))) {
      common.stimulus.length = TRUE
    } else if (all(c("LB", "UB") %in% names(StAttrib))) {
      common.stimulus.length = FALSE
      stimulus.length.LB = StAttrib$LB
      stimulus.length.UB = StAttrib$UB
      if (any(is.na(c(stimulus.length.LB, stimulus.length.UB))) || any(stimulus.length.LB > stimulus.length.UB) || any(c(stimulus.length.LB, stimulus.length.UB) < 0)) {
        stop("LB/UB in StAttrib contains NA or improper values")
      }
    } else {
      stop("StAttrib should contain columns LB and UB; otherwise, CONDITION should include \"PER STIMULUS\" ")
    }
  } else if (length(ItemConstraints) > 0) {
    setBased = FALSE
    nv = ni
    ID = ItemAttrib$ID
    item.index.by.stimulus = NULL
    stimulus.index.by.item = NULL
  } else {
    stop("Constraints must include at least one \"ITEM\" under WHAT; for stimulus-based, LB/UB should be specified for each stimulus.")
  }
  for (index in ItemConstraints) {
    ListConstraints[[index]] = new("constraint")
    ListConstraints[[index]]@CONSTRAINT = Constraints$CONSTRAINT[index]
    ConstraintTypeIsValid = FALSE
    ListConstraints[[index]]@suspend = Constraints$ONOFF[index] == "OFF"
    if (Constraints$TYPE[index] %in% c("NUMBER", "COUNT")) {
      ConstraintTypeIsValid = TRUE
      if (toupper(Constraints$CONDITION[index]) %in% c("", " ", "PER TEST", "TEST")) {
        test.length.LB = round(Constraints$LB[index])
        test.length.UB = round(Constraints$UB[index])
        if (any(c(test.length.LB, test.length.UB) < 0) || test.length.LB > test.length.UB) {
          stop(sprintf("CONSTRAINT %s has invalid LB/UB", index))
        } else if (test.length.LB == test.length.UB) {
          testLength = test.length.UB
          ListConstraints[[index]]@mat = matrix(0, nrow = 1, ncol = nv)
          ListConstraints[[index]]@mat[1, 1:ni] = 1
          ListConstraints[[index]]@dir = "=="
          ListConstraints[[index]]@rhs = test.length.UB
        } else {
          stop("LB and UB for ITEM NUMBER must be set equal.")
        }
        if (setBased && !common.stimulus.length) {
          n.LB.eq.UB = sum(stimulus.length.LB == stimulus.length.UB)
          n.LB.ne.UB = sum(stimulus.length.LB != stimulus.length.UB)
          tmp.mat = matrix(0, nrow = ns + n.LB.ne.UB, ncol = nv)
          tmp.dir = character(ns + n.LB.ne.UB)
          tmp.rhs = rep(0, ns + n.LB.ne.UB)
          tmp.index = 1
          for (s in 1:ns) {
            if (stimulus.length.LB[s] == stimulus.length.UB[s]) {
              tmp.mat[tmp.index, item.index.by.stimulus[[s]]] = 1
              tmp.mat[tmp.index, ni + s] = -stimulus.length.UB[s]
              tmp.dir[tmp.index] = "=="
              tmp.index = tmp.index + 1
            } else if (stimulus.length.LB[s] < stimulus.length.UB[s]) {
              tmp.mat[c(tmp.index, tmp.index + 1), item.index.by.stimulus[[s]]] = 1
              tmp.mat[tmp.index, ni + s] = -stimulus.length.LB[s]
              tmp.mat[tmp.index + 1, ni + s] = -stimulus.length.UB[s]
              tmp.dir[tmp.index] = ">="
              tmp.dir[tmp.index + 1] = "<="
              tmp.index = tmp.index + 2
            } else {
              stop(sprintf("stimulus %s contains improper LB/UB (LB > UB)", s))
            }
          }
          ListConstraints[[index]]@mat = rbind(ListConstraints[[index]]@mat, tmp.mat)
          ListConstraints[[index]]@dir = c(ListConstraints[[index]]@dir, tmp.dir)
          ListConstraints[[index]]@rhs = c(ListConstraints[[index]]@rhs, tmp.rhs)
        }
      } else if (toupper(Constraints$CONDITION[index]) %in% c("PER STIMULUS", "PER PASSAGE", "PER SET", "PER TESTLET")) {
        if (!setBased) {
          stop(sprintf("Constraints must include at least one \"STIMULUS\" under WHAT for CONDITION: %s", toupper(Constraints$CONDITION[index])))
        }
        stimulus.length.LB = round(Constraints$LB[index])
        stimulus.length.UB = round(Constraints$UB[index])
        if (any(c(stimulus.length.LB, stimulus.length.UB) < 0) || stimulus.length.LB > stimulus.length.UB) {
          stop(sprintf("CONSTRAINT %s has invalid LB/UB", index))
        } else if (stimulus.length.LB == stimulus.length.UB) {
          ListConstraints[[index]]@mat = matrix(0, nrow = ns, ncol = nv)
          ListConstraints[[index]]@dir = rep("==", ns)
          ListConstraints[[index]]@rhs = rep(0, ns)
          for (s in 1:ns) {
            ListConstraints[[index]]@mat[s, item.index.by.stimulus[[s]]] = 1
            ListConstraints[[index]]@mat[s, ni + s] = -stimulus.length.UB
          }
        } else {
          ListConstraints[[index]]@mat = matrix(0, nrow = ns * 2, ncol = nv)
          ListConstraints[[index]]@dir = rep(c(">=", "<="), ns)
          ListConstraints[[index]]@rhs = rep(0, ns * 2)
          for (s in 1:ns) {
            ListConstraints[[index]]@mat[c(s * 2 - 1, s * 2), item.index.by.stimulus[[s]]] = 1
            ListConstraints[[index]]@mat[c(s * 2 - 1), ni + s] = -stimulus.length.LB
            ListConstraints[[index]]@mat[c(s * 2), ni + s] = -stimulus.length.UB
          }
        }
      } else if (Constraints$CONDITION[index] %in% names(ItemAttrib)) {
        condition = unique(ItemAttrib[Constraints$CONDITION[index]])
        condition = condition[!is.na(condition)]
        if (length(condition) == 0) {
          stop(sprintf("CONSTRAINT %s returned 0 items meeting CONDITION: %s", index, Constraints$CONDITION[index]))
        }
        if (any(c(Constraints$LB[index], Constraints$UB[index]) < 0) || Constraints$LB[index] > Constraints$UB[index]) {
          stop(sprintf("CONSTRAINT %s has invalid LB/UB", index))
        } else if (Constraints$LB[index] == Constraints$UB[index]) {
          ListConstraints[[index]]@mat = matrix(0, nrow = length(condition), ncol = nv)
          ListConstraints[[index]]@dir = rep("==", length(condition))
          ListConstraints[[index]]@rhs = rep(Constraints$UB[index], length(condition))
          for (m in 1:length(condition)) {
            condition.met = which(ItemAttrib[Constraints$CONDITION[index]] == condition[m])
            ListConstraints[[index]]@mat[m, condition.met] = 1
          }
        } else if (Constraints$LB[index] <= Constraints$UB[index]) {
          ListConstraints[[index]]@mat = matrix(0, nrow = 2 * length(condition), ncol = nv)
          ListConstraints[[index]]@dir = rep(c(">=", "<="), length(condition))
          ListConstraints[[index]]@rhs = rep(c(Constraints$LB[index], Constraints$UB[index]), length(condition))
          for (m in 1:length(condition)) {
            condition.met = which(ItemAttrib[Constraints$CONDITION[index]] == condition[m])
            ListConstraints[[index]]@mat[c(m * 2 - 1, m * 2), condition.met] = 1
          }
        }
      } else {
        condition.met = which(with(ItemAttrib, eval(parse(text = Constraints$CONDITION[index]))))
        if (length(condition.met) == 0) {
          stop(sprintf("CONSTRAINT %s returned 0 items meeting CONDITION: %s", index, Constraints$CONDITION[index]))
        }
        if (any(c(Constraints$LB[index], Constraints$UB[index]) < 0) || Constraints$LB[index] > Constraints$UB[index]) {
          stop(sprintf("CONSTRAINT %s has invalid LB/UB", index))
        } else if (Constraints$LB[index] == Constraints$UB[index]) {
          ListConstraints[[index]]@mat = matrix(0, nrow = 1, ncol = nv)
          ListConstraints[[index]]@mat[1, condition.met] = 1
          ListConstraints[[index]]@dir = "=="
          ListConstraints[[index]]@rhs = Constraints$UB[index]
        } else {
          ListConstraints[[index]]@mat = matrix(0, nrow = 2, ncol = nv)
          ListConstraints[[index]]@mat[, condition.met] = 1
          ListConstraints[[index]]@dir = c(">=", "<=")
          ListConstraints[[index]]@rhs = c(Constraints$LB[index], Constraints$UB[index])
        }
      }
    }
    if (Constraints$TYPE[index] %in% c("SUM", "AVERAGE", "MEAN")) {
      ConstraintTypeIsValid = TRUE
      if (!(Constraints$CONDITION[index] %in% names(ItemAttrib))) {
        stop(sprintf("CONSTRAINT %s: %s not found in ItemAttrib:", index, Constraints$CONDITION[index]))
      } else {
        if (any(is.na(ItemAttrib[[Constraints$CONDITION[index]]]))) {
          stop(sprintf("CONSTRAINT %s: %s must not have a missing value", index, Constraints$CONDITION[index]))
        }
        if (any(c(Constraints$LB[index], Constraints$UB[index]) < 0) || Constraints$LB[index] > Constraints$UB[index]) {
          stop(sprintf("CONSTRAINT %s has invalid LB/UB", index))
        } else if (Constraints$LB[index] == Constraints$UB[index]) {
          ListConstraints[[index]]@mat = matrix(0, nrow = 1, ncol = nv)
          ListConstraints[[index]]@dir = "<="
          ListConstraints[[index]]@rhs = Constraints$UB[index]
          if (Constraints$TYPE[index] == "SUM") {
            ListConstraints[[index]]@mat[1, 1:ni] = ItemAttrib[[Constraints$CONDITION[index]]]
          } else if (Constraints$TYPE[index] %in% c("AVERAGE", "MEAN")) {
            ListConstraints[[index]]@mat[1, 1:ni] = ItemAttrib[[Constraints$CONDITION[index]]] / test.length.UB
          }
        } else {
          ListConstraints[[index]]@mat = matrix(0, nrow = 2, ncol = nv)
          ListConstraints[[index]]@dir = c(">=", "<=")
          ListConstraints[[index]]@rhs = c(Constraints$LB[index], Constraints$UB[index])
          if (Constraints$TYPE[index] == "SUM") {
            ListConstraints[[index]]@mat[, 1:ni] = ItemAttrib[[Constraints$CONDITION[index]]]
          } else if (Constraints$TYPE[index] %in% c("AVERAGE", "MEAN")) {
            ListConstraints[[index]]@mat[1, 1:ni] = ItemAttrib[[Constraints$CONDITION[index]]] / test.length.UB
            ListConstraints[[index]]@mat[2, 1:ni] = ItemAttrib[[Constraints$CONDITION[index]]] / test.length.LB
          }
        }
      }
    }
    if (Constraints$TYPE[index] == "INCLUDE") {
      ConstraintTypeIsValid = TRUE
      condition.met = which(with(ItemAttrib, eval(parse(text = Constraints$CONDITION[index]))))
      if (length(condition.met) == 0) {
        stop(sprintf("CONSTRAINT %s is invalid: %s returned 0 items", index, Constraints$CONDITION[index]))
      } else {
        ListConstraints[[index]]@mat = matrix(0, nrow = 1, ncol = nv)
        ListConstraints[[index]]@mat[1, condition.met] = 1
        ListConstraints[[index]]@dir = "=="
        if (setBased) {
          stimulus.to.include = unique(stimulus.index.by.item[condition.met])
          stimulus.to.include = stimulus.to.include[!is.na(stimulus.to.include)]
          ListConstraints[[index]]@mat[1, ni + stimulus.to.include] = 1
          ListConstraints[[index]]@rhs = length(condition.met) + length(stimulus.to.include)
        } else {
          ListConstraints[[index]]@rhs = length(condition.met)
        }
      }
    }
    if (Constraints$TYPE[index] %in% c("EXCLUDE", "NOT", "NOT INCLUDE")) {
      ConstraintTypeIsValid = TRUE
      condition.met = which(with(ItemAttrib, eval(parse(text = Constraints$CONDITION[index]))))
      if (length(condition.met) == 0) {
        stop(sprintf("CONSTRAINT %s returned 0 items meeting CONDITION: %s", index, Constraints$CONDITION[index]))
      } else {
        ListConstraints[[index]]@mat = matrix(0, nrow = 1, ncol = nv)
        ListConstraints[[index]]@mat[1, condition.met] = 1
        ListConstraints[[index]]@dir = "=="
        ListConstraints[[index]]@rhs = 0
      }
    }
    if (Constraints$TYPE[index] %in% c("ALLORNONE", "ALL OR NONE", "IIF")) {
      ConstraintTypeIsValid = TRUE
      condition.met = which(with(ItemAttrib, eval(parse(text = Constraints$CONDITION[index]))))
      n.met = length(condition.met)
      if (n.met < 2) {
        stop(sprintf("CONSTRAINT %s is invalid: %s returned < 2 items", index, Constraints$CONDITION[index]))
      } else {
        ListConstraints[[index]]@mat = matrix(0, nrow = (n.met * (n.met - 1)) / 2, ncol = nv)
        ListConstraints[[index]]@dir = rep("==", (n.met * (n.met - 1)) / 2)
        ListConstraints[[index]]@rhs = rep(0, (n.met * (n.met - 1)) / 2)
        tmp.index = 0
        for (i in condition.met) {
          for (j in condition.met) {
            if (i < j) {
              tmp.index = tmp.index + 1
              ListConstraints[[index]]@mat[tmp.index, c(i, j)] = c(1, -1)
            }
          }
        }
      }
    }
    if (Constraints$TYPE[index] %in% c("MUTUALLYEXCLUSIVE", "MUTUALLY EXCLUSIVE", "XOR", "ENEMY")) {
      ConstraintTypeIsValid = TRUE
      condition.met = which(with(ItemAttrib, eval(parse(text = Constraints$CONDITION[index]))))
      if (length(condition.met) < 2) {
        stop(sprintf("CONSTRAINT %s is invalid: %s returned < 2 items", index, Constraints$CONDITION[index]))
      } else {
        ListConstraints[[index]]@mat = matrix(0, nrow = 1, ncol = nv)
        ListConstraints[[index]]@mat[1, condition.met] = 1
        ListConstraints[[index]]@dir = "<="
        ListConstraints[[index]]@rhs = 1
      }
    }
    if (Constraints$TYPE[index] == "ORDER") {
      ConstraintTypeIsValid = TRUE
      if (!ListConstraints[[index]]@suspend) {
        if (Constraints$CONDITION[index] %in% names(ItemAttrib)) {
          if (any(is.na(ItemAttrib[[Constraints$CONDITION[index]]]))) {
            stop(sprintf("CONSTRAINT %s: %s must not have a missing value", index, Constraints$CONDITION[index]))
          }
          ItemOrder = ItemAttrib[[Constraints$CONDITION[index]]]
          ItemOrderBy = Constraints$CONDITION[index]
        } else {
          stop(sprintf("CONSTRAINT %s is invalid: %s not found in ItemAttrib", index, Constraints$CONDITION[index]))
        }
      }
    }
    if (!ConstraintTypeIsValid) {
      stop(sprintf("CONSTRAINT %s, %s is not a valid constraint type", index, Constraints$TYPE[index]))
    }
    ListConstraints[[index]]@nc = nrow(ListConstraints[[index]]@mat)
  }
  if (setBased) {
    for (index in StimulusConstraints) {
      ListConstraints[[index]] = new("constraint")
      ListConstraints[[index]]@CONSTRAINT = Constraints$CONSTRAINT[index]
      ConstraintTypeIsValid = FALSE
      ListConstraints[[index]]@suspend = Constraints$ONOFF[index] == "OFF"
      if (Constraints$TYPE[index] %in% c("NUMBER", "COUNT")) {
        ConstraintTypeIsValid = TRUE
        if (toupper(Constraints$CONDITION[index]) %in% c("", " ", "PER TEST")) {
          number.stimulus.LB = round(Constraints$LB[index])
          number.stimulus.UB = round(Constraints$UB[index])
          if (number.stimulus.LB == number.stimulus.UB) {
            ListConstraints[[index]]@mat = matrix(0, nrow = 1, ncol = nv)
            ListConstraints[[index]]@mat[1, (ni + 1):nv] = 1
            ListConstraints[[index]]@dir = "=="
            ListConstraints[[index]]@rhs = number.stimulus.UB
          } else if (number.stimulus.LB < number.stimulus.UB) {
            ListConstraints[[index]]@mat = matrix(0, nrow = 2, ncol = nv)
            ListConstraints[[index]]@mat[, (ni + 1):nv] = 1
            ListConstraints[[index]]@dir = c(">=", "<=")
            ListConstraints[[index]]@rhs = c(number.stimulus.LB, number.stimulus.UB)
          } else {
            stop(sprintf("CONSTRAINT %s has invalid LB/UB", index))
          }
        } else if (Constraints$CONDITION[index] %in% names(StAttrib)) {
          condition = unique(StAttrib[Constraints$CONDITION[index]])
          condition = condition[!is.na(condition)]
          if (length(condition) == 0) {
            stop(sprintf("CONSTRAINT %s returned 0 items meeting CONDITION: %s", index, Constraints$CONDITION[index]))
          }
          if (any(c(Constraints$LB[index], Constraints$UB[index]) < 0) || Constraints$LB[index] > Constraints$UB[index]) {
            stop(sprintf("CONSTRAINT %s has invalid LB/UB", index))
          } else if (Constraints$LB[index] == Constraints$UB[index]) {
            ListConstraints[[index]]@mat = matrix(0, nrow = length(condition), ncol = nv)
            ListConstraints[[index]]@dir = rep("==", length(condition))
            ListConstraints[[index]]@rhs = rep(Constraints$UB[index], length(condition))
            for (m in 1:length(condition)) {
              condition.met = which(StAttrib[Constraints$CONDITION[index]] == condition[m])
              ListConstraints[[index]]@mat[m, ni + condition.met] = 1
            }
          } else if (Constraints$LB[index] <= Constraints$UB[index]) {
            ListConstraints[[index]]@mat = matrix(0, nrow = 2 * length(condition), ncol = nv)
            ListConstraints[[index]]@dir = rep(c(">=", "<="), length(condition))
            ListConstraints[[index]]@rhs = rep(c(Constraints$LB[index], Constraints$UB[index]), length(condition))
            for (m in 1:length(condition)) {
              condition.met = which(ItemAttrib[Constraints$CONDITION[index]] == condition[m])
              ListConstraints[[index]]@mat[c(m * 2 - 1, m * 2), ni + condition.met] = 1
            }
          }
        } else {
          condition.met = which(with(StAttrib, eval(parse(text = Constraints$CONDITION[index]))))
          if (length(condition.met) == 0) {
            stop(sprintf("CONSTRAINT %s returned 0 items meeting CONDITION: %s", index, Constraints$CONDITION[index]))
          }
          if (any(c(Constraints$LB[index], Constraints$UB[index]) < 0) || Constraints$LB[index] > Constraints$UB[index]) {
            stop(sprintf("CONSTRAINT %s has invalid LB/UB", index))
          } else if (Constraints$LB[index] == Constraints$UB[index]) {
            ListConstraints[[index]]@mat = matrix(0, nrow = 1, ncol = nv)
            ListConstraints[[index]]@mat[1, ni + condition.met] = 1
            ListConstraints[[index]]@dir = "=="
            ListConstraints[[index]]@rhs = Constraints$UB[index]
          } else {
            ListConstraints[[index]]@mat = matrix(0, nrow = 2, ncol = nv)
            ListConstraints[[index]]@mat[, ni + condition.met] = 1
            ListConstraints[[index]]@dir = c(">=", "<=")
            ListConstraints[[index]]@rhs = c(Constraints$LB[index], Constraints$UB[index])
          }
        }
      }
      if (Constraints$TYPE[index] %in% c("SUM", "AVERAGE", "MEAN")) {
        ConstraintTypeIsValid = TRUE
        if (!(Constraints$CONDITION[index] %in% names(StAttrib))) {
          stop(sprintf("CONSTRAINT %s has invalid: %s not found in ItemAttrib:", index, Constraints$CONDITION[index]))
        } else {
          if (any(is.na(StAttrib[[Constraints$CONDITION[index]]]))) {
            stop(sprintf("CONSTRAINT %s: %s must not have a missing value", index, Constraints$CONDITION[index]))
          }
          if (any(c(Constraints$LB[index], Constraints$UB[index]) < 0) || Constraints$LB[index] > Constraints$UB[index]) {
            stop(sprintf("CONSTRAINT %s has invalid LB/UB", index))
          } else if (Constraints$LB[index] == Constraints$UB[index]) {
            ListConstraints[[index]]@mat = matrix(0, nrow = 1, ncol = nv)
            ListConstraints[[index]]@dir = "<="
            ListConstraints[[index]]@rhs = Constraints$UB[index]
            if (Constraints$TYPE[index] == "SUM") {
              ListConstraints[[index]]@mat[1, (ni + 1):nv] = StAttrib[[Constraints$CONDITION[index]]]
            } else if (Constraints$TYPE[index] %in% c("AVERAGE", "MEAN")) {
              ListConstraints[[index]]@mat[1, (ni + 1):nv] = StAttrib[[Constraints$CONDITION[index]]] / number.stimulus.UB
            }
          } else {
            ListConstraints[[index]]@mat = matrix(0, nrow = 2, ncol = nv)
            ListConstraints[[index]]@dir = c(">=", "<=")
            ListConstraints[[index]]@rhs = c(Constraints$LB[index], Constraints$UB[index])
            if (Constraints$TYPE[index] == "SUM") {
              ListConstraints[[index]]@mat[, (ni + 1):nv] = StAttrib[[Constraints$CONDITION[index]]]
            } else if (Constraints$TYPE[index] %in% c("AVERAGE", "MEAN")) {
              ListConstraints[[index]]@mat[1, (ni + 1):nv] = StAttrib[[Constraints$CONDITION[index]]] / number.stimulus.UB
              ListConstraints[[index]]@mat[2, (ni + 1):nv] = StAttrib[[Constraints$CONDITION[index]]] / number.stimulus.LB
            }
          }
        }
      }
      if (Constraints$TYPE[index] == "INCLUDE") {
        ConstraintTypeIsValid = TRUE
        condition.met = which(with(StAttrib, eval(parse(text = Constraints$CONDITION[index]))))
        if (length(condition.met) == 0) {
          stop(sprintf("CONSTRAINT %s is invalid: %s returned 0 items", index, Constraints$CONDITION[index]))
        } else {
          ListConstraints[[index]]@mat = matrix(0, nrow = 1, ncol = nv)
          ListConstraints[[index]]@mat[1, ni + condition.met] = 1
          ListConstraints[[index]]@dir = "=="
          ListConstraints[[index]]@rhs = length(condition.met)
        }
      }
      if (Constraints$TYPE[index] %in% c("EXCLUDE", "NOT", "NOT INCLUDE")) {
        ConstraintTypeIsValid = TRUE
        condition.met = which(with(StAttrib, eval(parse(text = Constraints$CONDITION[index]))))
        if (length(condition.met) == 0) {
          stop(sprintf("CONSTRAINT %s returned 0 items meeting CONDITION: %s", index, Constraints$CONDITION[index]))
        } else {
          ListConstraints[[index]]@mat = matrix(0, nrow = 1, ncol = nv)
          ListConstraints[[index]]@mat[1, ni + condition.met] = 1
          ListConstraints[[index]]@dir = "=="
          ListConstraints[[index]]@rhs = 0
          for (s in condition.met) {
            ListConstraints[[index]]@mat[1, item.index.by.stimulus[[s]]] = 1
          }
        }
      }
      if (Constraints$TYPE[index] %in% c("ALLORNONE", "ALL OR NONE", "IIF")) {
        ConstraintTypeIsValid = TRUE
        condition.met = which(with(StAttrib, eval(parse(text = Constraints$CONDITION[index]))))
        n.met = length(condition.met)
        if (n.met < 2) {
          stop(sprintf("CONSTRAINT %s is invalid: %s returned < 2 stimuli", index, Constraints$CONDITION[index]))
        } else {
          ListConstraints[[index]]@mat = matrix(0, nrow = (n.met * (n.met - 1)) / 2, ncol = nv)
          ListConstraints[[index]]@dir = rep("==", (n.met * (n.met - 1)) / 2)
          ListConstraints[[index]]@rhs = rep(0, (n.met * (n.met - 1)) / 2)
          tmp.index = 0
          for (i in condition.met) {
            for (j in condition.met) {
              if (i < j) {
                tmp.index = tmp.index + 1
                ListConstraints[[index]]@mat[tmp.index, ni + c(i, j)] = c(1, -1)
              }
            }
          }
        }
      }
      if (Constraints$TYPE[index] %in% c("MUTUALLYEXCLUSIVE", "MUTUALLY EXCLUSIVE", "XOR", "ENEMY")) {
        ConstraintTypeIsValid = TRUE
        condition.met = which(with(StAttrib, eval(parse(text = Constraints$CONDITION[index]))))
        if (length(condition.met) < 2) {
          stop(sprintf("CONSTRAINT %s is invalid: %s returned < 2 stimuli", index, Constraints$CONDITION[index]))
        } else {
          ListConstraints[[index]]@mat = matrix(0, nrow = 1, ncol = nv)
          ListConstraints[[index]]@mat[1, ni + condition.met] = 1
          ListConstraints[[index]]@dir = "<="
          ListConstraints[[index]]@rhs = 1
        }
      }
      if (Constraints$TYPE[index] == "ORDER") {
        ConstraintTypeIsValid = TRUE
        if (!ListConstraints[[index]]@suspend) {
          if (Constraints$CONDITION[index] %in% names(StAttrib)) {
            if (any(is.na(StAttrib[[Constraints$CONDITION[index]]]))) {
              stop(sprintf("CONSTRAINT %s: %s must not have a missing value", index, Constraints$CONDITION[index]))
            }
            StimulusOrder = StAttrib[[Constraints$CONDITION[index]]]
            StimulusOrderBy = Constraints$CONDITION[index]
          } else {
            stop(sprintf("CONSTRAINT %s is invalid: %s not found in StAttrib", index, Constraints$CONDITION[index]))
          }
        }
      }
      if (!ConstraintTypeIsValid) {
        stop(sprintf("CONSTRAINT %s, %s is not a valid constraint type", index, Constraints$TYPE[index]))
      }
      ListConstraints[[index]]@nc = nrow(ListConstraints[[index]]@mat)
    }
  }
  INDEX = NULL
  MAT = NULL
  DIR = NULL
  RHS = NULL
  for (index in 1:nc) {
    if (Constraints$TYPE[index] != "ORDER" && !ListConstraints[[index]]@suspend) {
      ListConstraints[[index]]@nc = nrow(ListConstraints[[index]]@mat)
      MAT = rbind(MAT, ListConstraints[[index]]@mat)
      DIR = c(DIR, ListConstraints[[index]]@dir)
      RHS = c(RHS, ListConstraints[[index]]@rhs)
      INDEX = c(INDEX, rep(Constraints$CONSTRAINT[index], ListConstraints[[index]]@nc))
    }
  }
  out = list(Constraints = Constraints, ListConstraints = ListConstraints, pool = pool, ItemAttrib = ItemAttrib, StAttrib = StAttrib,
             testLength = testLength, nv = nv, ni = ni, ns = ns, ID = ID, INDEX = INDEX, MAT = MAT, DIR = DIR, RHS = RHS, setBased = setBased,
             ItemOrder = ItemOrder, ItemOrderBy = ItemOrderBy, StimulusOrder = StimulusOrder, StimulusOrderBy = StimulusOrderBy,
             ItemIndexByStimulus = item.index.by.stimulus, StimulusIndexByItem = stimulus.index.by.item)
  return(out)
}

#' Update constraints
#'
#' Update the onstraints list
#'
#' @param object a list object returned from \code{\link{LoadConstraints}}
#' @param on a vector of constraints index to turn on.
#' @param off a vector of constraints index to turn off.
#'
#' @return An updated list of constraints to be used in \code{\link{ATA}} and \code{\link{Shadow}}.
#'
#' @export

UpdateConstraints = function(object, on = NULL, off = NULL) {
  nc = nrow(object$Constraints)
  if (length(intersect(on, off)) > 0) {
    stop("the on- and off-vectors cannot contain a common constraint index")
  }
  if (!"ONOFF" %in% names(object$Constraints)) {
    object$Constraints$ONOFF = ""
  }
  if (!is.null(on)) {
    if (any(!is.element(on, 1:nc))) {
      stop("the on-vector contains an invalid constraint index")
    }
    for (index in on) {
      object$ListConstraints[[index]]@suspend = FALSE
      object$Constraints[index, "ONOFF"] = ""
    }
  }
  if (!is.null(off)) {
    if (any(!is.element(off, 1:nc))) {
      stop("the off-vector contains an invalid constraint index")
    }
    for (index in off) {
      object$ListConstraints[[index]]@suspend = TRUE
      object$Constraints[index, "ONOFF"] = "OFF"
    }
  }
  INDEX = NULL
  MAT = NULL
  DIR = NULL
  RHS = NULL
  for (index in 1:nc) {
    if (object$Constraints$TYPE[index] != "ORDER" && !object$ListConstraints[[index]]@suspend) {
      object$ListConstraints[[index]]@nc = nrow(object$ListConstraints[[index]]@mat)
      MAT = rbind(MAT, object$ListConstraints[[index]]@mat)
      DIR = c(DIR, object$ListConstraints[[index]]@dir)
      RHS = c(RHS, object$ListConstraints[[index]]@rhs)
      INDEX = c(INDEX, rep(object$Constraints$CONSTRAINT[index], object$ListConstraints[[index]]@nc))
    }
  }
  object$INDEX = INDEX
  object$MAT = MAT
  object$DIR = DIR
  object$RHS = RHS
  return(object)
}

#' Build constraints
#'
#' Read constraints from specified files.
#'
#' @param pool An \code{item.pool} object. Use \code{\link{LoadItemPool}} for this.
#' @param file.Constraints Character. The name of the file containing constraint specifications.
#' @param file.ItemAttrib Character. The name of the file containing item attributes.
#' @param file.StAttrib (Optional) Character. The name of the file containing set attributes.
#'
#' @return A list containing the parsed constraints, to be used in \code{\link{ATA}} and \code{\link{Shadow}}.
#'
#' @export

BuildConstraints = function(pool, file.Constraints, file.ItemAttrib, file.StAttrib = NULL) {
  ItemAttrib = LoadItemAttrib(file.ItemAttrib, pool)
  if (!is.null(file.StAttrib)) {
    StAttrib = LoadStAttrib(file.StAttrib, ItemAttrib)
  } else {
    StAttrib = NULL
  }
  Constraints = LoadConstraints(file.Constraints, pool, ItemAttrib, StAttrib)
  return(Constraints)
}
