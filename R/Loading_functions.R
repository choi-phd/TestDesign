#' Load item paramaters
#'
#' Read item parameters from a .csv file or a data.frame and create an \linkS4class{item.pool} class.
#'
#' @param file File path of a .csv file containing item parameters. The file content should not have column names.
#' @param ipar A data.frame created from a .csv file.
#' @param se.file File path of a .csv file containing standard errors.
#' @return An \linkS4class{item.pool} object.
#'
#' @seealso \link{dataset.science} for example usage.
#' @export
loadItemPool <- function(file, ipar = NULL, se.file = NULL) {
  if (is.null(ipar)) {
    ipar <- read.csv(file, header = TRUE, as.is = TRUE)
  }
  pool       <- new("item.pool")
  ni         <- nrow(ipar)
  pool@index <- 1:ni
  pool@id    <- as.character(ipar[[1]])
  model      <- ipar[[2]]
  NCAT       <- numeric(ni)
  parms      <- vector(mode = "list", length = ni)
  nfields    <- rowSums(!is.na(ipar))
  valid      <- logical(ni)
  pool@ipar  <- matrix(NA, nrow = ni, ncol = max(nfields) - 2)
  if (!is.null(se.file)) {
    ipar.se <- read.csv(se.file, header = TRUE, as.is = TRUE)
    loadSE  <- TRUE
    SEs     <- matrix(NA, nrow = ni, ncol = max(nfields) - 2)
  } else {
    loadSE <- FALSE
  }
  for (i in 1:ni) {
    if (model[i] == 1 | model[i] == "1PL") {
      NCAT[i] <- 2
      b <- ipar[[3]][i]
      pool@model[i] <- "item.1pl"
      parms[[i]] <- new("item.1pl", difficulty = b)
      valid[i] <- TRUE
      pool@ipar[i, 1] <- b
      if (loadSE) {
        SEs[i, 1] <- ipar.se[[3]][i]
      }
    } else if (model[i] == 2 | model[i] == "2PL") {
      NCAT[i] <- 2
      a <- ipar[[3]][i]
      b <- ipar[[4]][i]
      if (a > 0) {
        pool@model[i] <- "item.2pl"
        parms[[i]] <- new("item.2pl", slope = a, difficulty = b)
        valid[i] <- TRUE
        pool@ipar[i, 1:2] <- c(a, b)
        if (loadSE) {
          SEs[i, 1:2] <- c(ipar.se[[3]][i], ipar.se[[4]][i])
        }
      }
    } else if (model[i] == 3 | model[i] == "3PL") {
      NCAT[i] <- 2
      a <- ipar[[3]][i]
      b <- ipar[[4]][i]
      c <- ipar[[5]][i]
      if (a > 0 && c >= 0 && c < 1) {
        pool@model[i] <- "item.3pl"
        parms[[i]] <- new("item.3pl", slope = a, difficulty = b, guessing = c)
        valid[i] <- TRUE
        pool@ipar[i, 1:3] <- c(a, b, c)
        if (loadSE) {
          SEs[i, 1:3] <- c(ipar.se[[3]][i], ipar.se[[4]][i], ipar.se[[5]][i])
        }
      }
    } else if (model[i] == 4 | model[i] == "PC") {
      NCAT[i] <- nfields[i] - 1
      b <- as.numeric(ipar[i, 3:nfields[i]])
      pool@model[i] <- "item.pc"
      parms[[i]] <- new("item.pc", threshold = b, ncat = NCAT[i])
      valid[i] <- TRUE
      pool@ipar[i, 1:(NCAT[i] - 1)] <- b
      if (loadSE) {
        SEs[i, 1:(NCAT[i] - 1)] <- ipar.se[i, 3:nfields[i]]
      }
    } else if (model[i] == 5 | model[i] == "GPC") {
      NCAT[i] <- nfields[i] - 2
      a <- as.numeric(ipar[[3]][i])
      b <- as.numeric(ipar[i, 4:nfields[i]])
      if (a > 0) {
        pool@model[i] <- "item.gpc"
        parms[[i]] <- new("item.gpc", slope = a, threshold = b, ncat = NCAT[i])
        valid[i] <- TRUE
        pool@ipar[i, 1:NCAT[i]] <- c(a, b)
        if (loadSE) {
          SEs[i, 1:NCAT[i]] <- c(ipar.se[[3]][i], as.numeric(ipar.se[i, 4:nfields[i]]))
        }
      }
    } else if (model[i] == 6 | model[i] == "GR") {
      NCAT[i] <- nfields[i] - 2
      a <- as.numeric(ipar[[3]][i])
      b <- as.numeric(ipar[i, 4:nfields[i]])
      if (a > 0 && (!is.unsorted(b))) {
        pool@model[i] <- "item.gr"
        parms[[i]] <- new("item.gr", slope = a, category = b, ncat = NCAT[i])
        valid[i] <- TRUE
        pool@ipar[i, 1:NCAT[i]] <- c(a, b)
        if (loadSE) {
          SEs[i, 1:NCAT[i]] <- c(ipar[[3]][i], as.numeric(ipar.se[i, 4:nfields[i]]))
        }
      }
    } else {
      stop(paste("Item", i, ": unknown IRT model specified - valid models are 1: 1PL, 2: 2PL, 3: 3PL, 4: PC, 5: GPC, or 6: GR"))
    }
  }
  if (sum(!valid) > 0) {
    stop(paste("Check the parameters for the following item(s):", paste((1:ni)[!valid], collapse = ", "), "\n"))
  }

  pool@ni <- ni
  pool@maxCat <- max(NCAT)
  pool@NCAT <- NCAT
  pool@parms <- parms

  if (loadSE) {
    pool@SEs <- SEs
  } else {
    pool@SEs <- pool@ipar * 0
  }
  if (max(rowSums(!is.na(pool@ipar))) != max(nfields) - 2) {
    pool@ipar <- pool@ipar[, 1:max(rowSums(!is.na(pool@ipar)))]
  }

  if (validObject(pool)) {
    return(pool)
  }
}

#' Load item attributes
#'
#' Read item attributes from specified file.
#'
#' @param file Character. The name of the file containing item attributes.
#' @param pool An \code{\linkS4class{item.pool}} object. Use \code{\link{loadItemPool}} for this.
#'
#' @return A \code{data.frame} containing parsed dataset.
#'
#' @seealso \link{dataset.science} for example usage.
#' @export

loadItemAttrib <- function(file, pool) {
  if (is.null(pool) || class(pool) != "item.pool") {
    stop("pool is missing or not of class \"item.pool\"")
  }
  item_attrib <- read.csv(file, header = TRUE, as.is = TRUE)
  names(item_attrib) <- toupper(names(item_attrib))
  if (pool@ni != nrow(item_attrib)) {
    stop("nrow of item attrib file not equal to pool@ni")
  }
  if (!("ID" %in% names(item_attrib))) {
    stop("no column name \"ID\" found in item_attrib")
  }
  if (is.numeric(item_attrib$ID)) {
    item_attrib$ID <- as.character(item_attrib$ID)
  }
  if (any(item_attrib$ID %in% c("", " ", "NA", "N/A"))) {
    stop("invalid ID entries were found in item_attrib")
  }
  if (length(unique(item_attrib$ID)) != nrow(item_attrib)) {
    stop("duplicate ID entries were found in item attrib")
  } else if (!all(sort(pool@id) == sort(item_attrib$ID))) {
    stop("ID entries in item attrib do not match pool@id")
  } else if (!all(pool@id == item_attrib$ID)) {
    item_attrib <- merge(data.frame(ID = pool@id), item_attrib, by = "ID")[, names(item_attrib)] # re-ordering cols in attrib
  }
  item_attrib <- data.frame(cbind(INDEX = 1:nrow(item_attrib), item_attrib))
  if ("STID" %in% names(item_attrib)) {
    if (any(item_attrib$STID %in% c("", " ", "N/A"))) {
      item_attrib$STID[item_attrib$STID %in% c("", " ", "N/A")] <- NA
    }
  }
  return(item_attrib)
}

#' Load set/stimulus/passage attributes
#'
#' Read set, stimulus, or passage attributes from specified file.
#'
#' @param file Character. The name of the file containing item attributes.
#' @param item.attrib A \code{data.frame} containing item attributes. Use \code{\link{loadItemAttrib}} for this.
#'
#' @return A \code{data.frame} containing stimulus attributes.
#'
#' @seealso \link{dataset.reading} for example usage.
#'
#' @export

loadStAttrib <- function(file, item.attrib) {
  st_attrib <- read.csv(file, header = TRUE, as.is = TRUE)
  names(st_attrib) <- toupper(names(st_attrib))
  if (!("STID" %in% names(st_attrib))) {
    stop("no column name \"STID\" found in set/stimulus/passage attrib")
  }
  if (is.numeric(st_attrib$STID)) {
    st_attrib$STID <- as.character(st_attrib$STID)
  }
  if (!("STIDNTEX" %in% names(st_attrib))) {
    st_attrib <- data.frame(cbind(STindex = 1:nrow(st_attrib), st_attrib))
  }
  if (!("STID" %in% names(item.attrib))) {
    stop("no column name \"STID\" found in item.attrib")
  }
  if (any(st_attrib$STID %in% c("", " ", NA))) {
    stop("invalid STID found in st_attrib")
  }
  if (length(unique(st_attrib$STID)) != nrow(st_attrib)) {
    stop("duplicate ID entries were found in set/stimulus attrib")
  } else if (!(all(st_attrib$STID %in% unique(item.attrib$STID)))) {
    stop("not all STID found in item.attrib")
  } else if (any(item.attrib$STID %in% c("", " ", "NA", "N/A"))) {
    # if items with no valid STID present (e.g., discrete items)
    if (!all(sort(unique(item.attrib$STID[!(item.attrib$STID %in% c(NA, "", " ", "N/A"))])) == sort(st_attrib$STID))) {
      stop("st_attrib must include all STID in item.attrib")
    }
  }
  return(st_attrib)
}

#' Load constraints
#'
#' Read constraints from specified file.
#'
#' Use \code{vignette("constraints")} for instructions on how to create a constraint set object.
#'
#' @param file Character. The name of the file containing specifications for constraints.
#' @param pool An \code{item.pool} object.
#' @param item.attrib A \code{data.frame} containing item attributes. Use \code{\link{loadItemAttrib}} for this.
#' @param st.attrib (Optional) A \code{data.frame} containing stimulus attributes. Use \code{\link{loadStAttrib}} for this.
#'
#' @return A list containing the parsed constraints, to be used in \code{\link{ATA}} and \code{\link{Shadow}}.
#'
#' @seealso \link{dataset.science} for example usage.
#'
#' @export

loadConstraints <- function(file, pool, item.attrib, st.attrib = NULL) {
  if (class(pool) != "item.pool") {
    stop("pool must be of class \"item.pool\"")
  }

  # Read file ------------------------------------------------------------
  constraints <- read.csv(file, header = TRUE)
  if ("ONOFF" %in% toupper(names(constraints))) {
    ONOFF <- TRUE
    constraints <- read.csv(file,
      header = TRUE,
      as.is = TRUE, colClasses = c(
        "character", "character", "character", "character",
        "numeric", "numeric", "character"
      ),
      stringsAsFactors = FALSE
    )
    constraints$ONOFF <- toupper(constraints$ONOFF)
  } else {
    ONOFF <- FALSE
    constraints <- read.csv(file,
      header = TRUE,
      as.is = TRUE, colClasses = c(
        "character", "character", "character", "character",
        "numeric", "numeric"
      ),
      stringsAsFactors = FALSE
    )
  }
  # Validation -----------------------------------------------------------
  # Validation: Column names
  names(constraints) <- toupper(names(constraints))
  if (!all(names(constraints) %in% c("CONSTRAINT", "TYPE", "WHAT", "CONDITION", "LB", "UB", "ONOFF"))) {
    stop("Column names must be CONSTRAINT, TYPE, WHAT, CONDITION, LB, UB, and ONOFF")
  }
  # Validation: Bounds
  if (!any(is.na(constraints$LB) | is.na(constraints$UB))) {
    if (any(constraints$LB > constraints$UB)) {
      stop("invalid LB/UB specified for one or more constraints (LB > UB)")
    }
  } else if (any(is.na(constraints$LB) + is.na(constraints$UB) == 1)) {
    stop("LB and UB should be both specified at the same time, or both omitted")
  }
  # Validation: Constraint types
  constraints$TYPE <- toupper(constraints$TYPE)
  constraints$WHAT <- toupper(constraints$WHAT)
  if (!all(constraints$TYPE %in% c("NUMBER", "COUNT", "ALLORNONE", "ALL OR NONE", "IIF", "MUTUALLYEXCLUSIVE", "MUTUALLY EXCLUSIVE", "XOR", "ENEMY", "SUM", "AVERAGE", "MEAN", "INCLUDE", "EXCLUDE", "NOT", "ORDER"))) {
    stop("invalid TYPE specified")
  }
  # Validation: Pool
  ni <- pool@ni
  ns <- 0
  x <- numeric(ni)
  nc <- nrow(constraints)
  if (nrow(item.attrib) != ni) {
    stop("nrow of item.attrib not equal to pool@ni")
  }
  if (!all(pool@id == item.attrib$ID)) {
    stop("item IDs in pool and item.attrib not matching")
  }
  list_constraints <- vector(mode = "list", length = nc)
  item_constraints <- which(constraints$WHAT == "ITEM")
  stim_constraints <- which(constraints$WHAT %in% c("STIMULUS", "PASSAGE", "SET", "TESTLET"))
  item_order <- item_order_by <- NULL
  stim_order <- stim_order_by <- NULL
  if (length(stim_constraints) > 0) {
    if (is.null(st.attrib)) {
      stop("for stimulus-based, st.attrib must not be NULL")
    }
    if (!("STID" %in% names(item.attrib))) {
      stop("for stimulus-based, item.attrib must include \"STID\" ")
    }
    set_based <- TRUE
    id <- c(item.attrib$ID, st.attrib$STID)
    ns <- nrow(st.attrib)
    nv <- ni + ns
    item_id_by_stimulus <- split(item.attrib$ID, as.factor(item.attrib$STID))
    item_index_by_stimulus <- lapply(item_id_by_stimulus, function(x) which(item.attrib$ID %in% x))
    item_index_by_stimulus <- lapply(st.attrib$STID, function(x) item_index_by_stimulus[[x]])
    if (any(item.attrib$STID %in% c("", " ", "N/A", "n/a"))) {
      item.attrib$STID[item.attrib$STID %in% c("", " ", "N/A", "n/a")] <- NA
    }
    stimulus_id_by_item <- item.attrib$STID
    stimulus_index_by_item<- sapply(stimulus_id_by_item, function(x) which(st.attrib$STID == x))
    if (any(toupper(constraints$CONDITION) %in% c("PER STIMULUS", "PER PASSAGE", "PER SET", "PER TESTLET"))) {
      common_stimulus_length <- TRUE
    } else if (all(c("LB", "UB") %in% names(st.attrib))) {
      common_stimulus_length <- FALSE
      stimulus_length_LB <- st.attrib$LB
      stimulus_length_UB <- st.attrib$UB
      if (any(is.na(c(stimulus_length_LB, stimulus_length_UB))) || any(stimulus_length_LB > stimulus_length_UB) || any(c(stimulus_length_LB, stimulus_length_UB) < 0)) {
        stop("LB/UB in st.attrib contains NA or improper values")
      }
    } else {
      stop("st.attrib should contain columns LB and UB; otherwise, CONDITION should include \"PER STIMULUS\" ")
    }
  } else if (length(item_constraints) > 0) {
    set_based <- FALSE
    nv <- ni
    id <- item.attrib$ID
    item_index_by_stimulus <- NULL
    stimulus_index_by_item <- NULL
  } else {
    stop("constraints must include at least one \"ITEM\" under WHAT; for stimulus-based, LB/UB should be specified for each stimulus.")
  }
  for (index in item_constraints) {
    list_constraints[[index]] <- new("constraint")
    list_constraints[[index]]@constraint <- constraints$CONSTRAINT[index]
    constraint_type_is_valid <- FALSE
    list_constraints[[index]]@suspend <- constraints$ONOFF[index] == "OFF"
    if (constraints$TYPE[index] %in% c("NUMBER", "COUNT")) {
      constraint_type_is_valid <- TRUE
      if (toupper(constraints$CONDITION[index]) %in% c("", " ", "PER TEST", "TEST")) {
        test_length_LB <- round(constraints$LB[index])
        test_length_UB <- round(constraints$UB[index])
        if (any(c(test_length_LB, test_length_UB) < 0) || test_length_LB > test_length_UB) {
          stop(sprintf("constraint %s has invalid LB/UB", index))
        } else if (test_length_LB == test_length_UB) {
          test_length <- test_length_UB
          list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
          list_constraints[[index]]@mat[1, 1:ni] <- 1
          list_constraints[[index]]@dir <- "=="
          list_constraints[[index]]@rhs <- test_length_UB
        } else {
          stop("LB and UB for ITEM NUMBER must be set equal.")
        }
        if (set_based && !common_stimulus_length) {
          n.LB.eq.UB <- sum(stimulus_length_LB == stimulus_length_UB)
          n.LB.ne.UB <- sum(stimulus_length_LB != stimulus_length_UB)
          tmp.mat <- matrix(0, nrow = ns + n.LB.ne.UB, ncol = nv)
          tmp.dir <- character(ns + n.LB.ne.UB)
          tmp.rhs <- rep(0, ns + n.LB.ne.UB)
          tmp.index <- 1
          for (s in 1:ns) {
            if (stimulus_length_LB[s] == stimulus_length_UB[s]) {
              tmp.mat[tmp.index, item_index_by_stimulus[[s]]] <- 1
              tmp.mat[tmp.index, ni + s] <- -stimulus_length_UB[s]
              tmp.dir[tmp.index] <- "=="
              tmp.index <- tmp.index + 1
            } else if (stimulus_length_LB[s] < stimulus_length_UB[s]) {
              tmp.mat[c(tmp.index, tmp.index + 1), item_index_by_stimulus[[s]]] <- 1
              tmp.mat[tmp.index, ni + s] <- -stimulus_length_LB[s]
              tmp.mat[tmp.index + 1, ni + s] <- -stimulus_length_UB[s]
              tmp.dir[tmp.index] <- ">="
              tmp.dir[tmp.index + 1] <- "<="
              tmp.index <- tmp.index + 2
            } else {
              stop(sprintf("stimulus %s contains improper LB/UB (LB > UB)", s))
            }
          }
          list_constraints[[index]]@mat <- rbind(list_constraints[[index]]@mat, tmp.mat)
          list_constraints[[index]]@dir <- c(list_constraints[[index]]@dir, tmp.dir)
          list_constraints[[index]]@rhs <- c(list_constraints[[index]]@rhs, tmp.rhs)
        }
      } else if (toupper(constraints$CONDITION[index]) %in% c("PER STIMULUS", "PER PASSAGE", "PER SET", "PER TESTLET")) {
        if (!set_based) {
          stop(sprintf("constraints must include at least one \"STIMULUS\" under WHAT for CONDITION: %s", toupper(constraints$CONDITION[index])))
        }
        stimulus_length_LB <- round(constraints$LB[index])
        stimulus_length_UB <- round(constraints$UB[index])
        if (any(c(stimulus_length_LB, stimulus_length_UB) < 0) || stimulus_length_LB > stimulus_length_UB) {
          stop(sprintf("constraint %s has invalid LB/UB", index))
        } else if (stimulus_length_LB == stimulus_length_UB) {
          list_constraints[[index]]@mat <- matrix(0, nrow = ns, ncol = nv)
          list_constraints[[index]]@dir <- rep("==", ns)
          list_constraints[[index]]@rhs <- rep(0, ns)
          for (s in 1:ns) {
            list_constraints[[index]]@mat[s, item_index_by_stimulus[[s]]] <- 1
            list_constraints[[index]]@mat[s, ni + s] <- -stimulus_length_UB
          }
        } else {
          list_constraints[[index]]@mat <- matrix(0, nrow = ns * 2, ncol = nv)
          list_constraints[[index]]@dir <- rep(c(">=", "<="), ns)
          list_constraints[[index]]@rhs <- rep(0, ns * 2)
          for (s in 1:ns) {
            list_constraints[[index]]@mat[c(s * 2 - 1, s * 2), item_index_by_stimulus[[s]]] <- 1
            list_constraints[[index]]@mat[c(s * 2 - 1), ni + s] <- -stimulus_length_LB
            list_constraints[[index]]@mat[c(s * 2), ni + s] <- -stimulus_length_UB
          }
        }
      } else if (constraints$CONDITION[index] %in% names(item.attrib)) {
        condition <- unique(item.attrib[constraints$CONDITION[index]])
        condition <- condition[!is.na(condition)]
        if (length(condition) == 0) {
          stop(sprintf("constraint %s returned 0 items meeting CONDITION: %s", index, constraints$CONDITION[index]))
        }
        if (any(c(constraints$LB[index], constraints$UB[index]) < 0) || constraints$LB[index] > constraints$UB[index]) {
          stop(sprintf("constraint %s has invalid LB/UB", index))
        } else if (constraints$LB[index] == constraints$UB[index]) {
          list_constraints[[index]]@mat <- matrix(0, nrow = length(condition), ncol = nv)
          list_constraints[[index]]@dir <- rep("==", length(condition))
          list_constraints[[index]]@rhs <- rep(constraints$UB[index], length(condition))
          for (m in 1:length(condition)) {
            condition_met <- which(item.attrib[constraints$CONDITION[index]] == condition[m])
            list_constraints[[index]]@mat[m, condition_met] <- 1
          }
        } else if (constraints$LB[index] <= constraints$UB[index]) {
          list_constraints[[index]]@mat <- matrix(0, nrow = 2 * length(condition), ncol = nv)
          list_constraints[[index]]@dir <- rep(c(">=", "<="), length(condition))
          list_constraints[[index]]@rhs <- rep(c(constraints$LB[index], constraints$UB[index]), length(condition))
          for (m in 1:length(condition)) {
            condition_met <- which(item.attrib[constraints$CONDITION[index]] == condition[m])
            list_constraints[[index]]@mat[c(m * 2 - 1, m * 2), condition_met] <- 1
          }
        }
      } else {
        condition_met <- which(with(item_attrib, eval(parse(text = constraints$CONDITION[index]))))
        if (length(condition_met) == 0) {
          stop(sprintf("constraint %s returned 0 items meeting CONDITION: %s", index, constraints$CONDITION[index]))
        }
        if (any(c(constraints$LB[index], constraints$UB[index]) < 0) || constraints$LB[index] > constraints$UB[index]) {
          stop(sprintf("constraint %s has invalid LB/UB", index))
        } else if (constraints$LB[index] == constraints$UB[index]) {
          list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
          list_constraints[[index]]@mat[1, condition_met] <- 1
          list_constraints[[index]]@dir <- "=="
          list_constraints[[index]]@rhs <- constraints$UB[index]
        } else {
          list_constraints[[index]]@mat <- matrix(0, nrow = 2, ncol = nv)
          list_constraints[[index]]@mat[, condition_met] <- 1
          list_constraints[[index]]@dir <- c(">=", "<=")
          list_constraints[[index]]@rhs <- c(constraints$LB[index], constraints$UB[index])
        }
      }
    }
    if (constraints$TYPE[index] %in% c("SUM", "AVERAGE", "MEAN")) {
      constraint_type_is_valid <- TRUE
      if (!(constraints$CONDITION[index] %in% names(item.attrib))) {
        stop(sprintf("constraint %s: %s not found in item.attrib:", index, constraints$CONDITION[index]))
      } else {
        if (any(is.na(item.attrib[[constraints$CONDITION[index]]]))) {
          stop(sprintf("constraint %s: %s must not have a missing value", index, constraints$CONDITION[index]))
        }
        if (any(c(constraints$LB[index], constraints$UB[index]) < 0) || constraints$LB[index] > constraints$UB[index]) {
          stop(sprintf("constraint %s has invalid LB/UB", index))
        } else if (constraints$LB[index] == constraints$UB[index]) {
          list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
          list_constraints[[index]]@dir <- "<="
          list_constraints[[index]]@rhs <- constraints$UB[index]
          if (constraints$TYPE[index] == "SUM") {
            list_constraints[[index]]@mat[1, 1:ni] <- item.attrib[[constraints$CONDITION[index]]]
          } else if (constraints$TYPE[index] %in% c("AVERAGE", "MEAN")) {
            list_constraints[[index]]@mat[1, 1:ni] <- item.attrib[[constraints$CONDITION[index]]] / test_length_UB
          }
        } else {
          list_constraints[[index]]@mat <- matrix(0, nrow = 2, ncol = nv)
          list_constraints[[index]]@dir <- c(">=", "<=")
          list_constraints[[index]]@rhs <- c(constraints$LB[index], constraints$UB[index])
          if (constraints$TYPE[index] == "SUM") {
            list_constraints[[index]]@mat[, 1:ni] <- item.attrib[[constraints$CONDITION[index]]]
          } else if (constraints$TYPE[index] %in% c("AVERAGE", "MEAN")) {
            list_constraints[[index]]@mat[1, 1:ni] <- item.attrib[[constraints$CONDITION[index]]] / test_length_UB
            list_constraints[[index]]@mat[2, 1:ni] <- item.attrib[[constraints$CONDITION[index]]] / test_length_LB
          }
        }
      }
    }
    if (constraints$TYPE[index] == "INCLUDE") {
      constraint_type_is_valid <- TRUE
      condition_met <- which(with(item.attrib, eval(parse(text = constraints$CONDITION[index]))))
      if (length(condition_met) == 0) {
        stop(sprintf("constraint %s is invalid: %s returned 0 items", index, constraints$CONDITION[index]))
      } else {
        list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
        list_constraints[[index]]@mat[1, condition_met] <- 1
        list_constraints[[index]]@dir <- "=="
        if (set_based) {
          stimulus_to_include <- unique(stimulus_index_by_item[condition_met])
          stimulus_to_include <- stimulus_to_include[!is.na(stimulus_to_include)]
          list_constraints[[index]]@mat[1, ni + stimulus_to_include] <- 1
          list_constraints[[index]]@rhs <- length(condition_met) + length(stimulus_to_include)
        } else {
          list_constraints[[index]]@rhs <- length(condition_met)
        }
      }
    }
    if (constraints$TYPE[index] %in% c("EXCLUDE", "NOT", "NOT INCLUDE")) {
      constraint_type_is_valid <- TRUE
      condition_met <- which(with(item.attrib, eval(parse(text = constraints$CONDITION[index]))))
      if (length(condition_met) == 0) {
        stop(sprintf("constraint %s returned 0 items meeting CONDITION: %s", index, constraints$CONDITION[index]))
      } else {
        list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
        list_constraints[[index]]@mat[1, condition_met] <- 1
        list_constraints[[index]]@dir <- "=="
        list_constraints[[index]]@rhs <- 0
      }
    }
    if (constraints$TYPE[index] %in% c("ALLORNONE", "ALL OR NONE", "IIF")) {
      constraint_type_is_valid <- TRUE
      condition_met <- which(with(item.attrib, eval(parse(text = constraints$CONDITION[index]))))
      n.met <- length(condition_met)
      if (n.met < 2) {
        stop(sprintf("constraint %s is invalid: %s returned < 2 items", index, constraints$CONDITION[index]))
      } else {
        list_constraints[[index]]@mat <- matrix(0, nrow = (n.met * (n.met - 1)) / 2, ncol = nv)
        list_constraints[[index]]@dir <- rep("==", (n.met * (n.met - 1)) / 2)
        list_constraints[[index]]@rhs <- rep(0, (n.met * (n.met - 1)) / 2)
        tmp.index <- 0
        for (i in condition_met) {
          for (j in condition_met) {
            if (i < j) {
              tmp.index <- tmp.index + 1
              list_constraints[[index]]@mat[tmp.index, c(i, j)] <- c(1, -1)
            }
          }
        }
      }
    }
    if (constraints$TYPE[index] %in% c("MUTUALLYEXCLUSIVE", "MUTUALLY EXCLUSIVE", "XOR", "ENEMY")) {
      constraint_type_is_valid <- TRUE
      condition_met <- which(with(item.attrib, eval(parse(text = constraints$CONDITION[index]))))
      if (length(condition_met) < 2) {
        stop(sprintf("constraint %s is invalid: %s returned < 2 items", index, constraints$CONDITION[index]))
      } else {
        list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
        list_constraints[[index]]@mat[1, condition_met] <- 1
        list_constraints[[index]]@dir <- "<="
        list_constraints[[index]]@rhs <- 1
      }
    }
    if (constraints$TYPE[index] == "ORDER") {
      constraint_type_is_valid <- TRUE
      if (!list_constraints[[index]]@suspend) {
        if (constraints$CONDITION[index] %in% names(item.attrib)) {
          if (any(is.na(item.attrib[[constraints$CONDITION[index]]]))) {
            stop(sprintf("constraint %s: %s must not have a missing value", index, constraints$CONDITION[index]))
          }
          item_order <- item.attrib[[constraints$CONDITION[index]]]
          item_order_by <- constraints$CONDITION[index]
        } else {
          stop(sprintf("constraint %s is invalid: %s not found in item.attrib", index, constraints$CONDITION[index]))
        }
      }
    }
    if (!constraint_type_is_valid) {
      stop(sprintf("constraint %s, %s is not a valid constraint type", index, constraints$TYPE[index]))
    }
    list_constraints[[index]]@nc <- nrow(list_constraints[[index]]@mat)
  }
  if (set_based) {
    for (index in stim_constraints) {
      list_constraints[[index]] <- new("constraint")
      list_constraints[[index]]@constraint <- constraints$CONSTRAINT[index]
      constraint_type_is_valid <- FALSE
      list_constraints[[index]]@suspend <- constraints$ONOFF[index] == "OFF"
      if (constraints$TYPE[index] %in% c("NUMBER", "COUNT")) {
        constraint_type_is_valid <- TRUE
        if (toupper(constraints$CONDITION[index]) %in% c("", " ", "PER TEST")) {
          number.stimulus.LB <- round(constraints$LB[index])
          number.stimulus.UB <- round(constraints$UB[index])
          if (number.stimulus.LB == number.stimulus.UB) {
            list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
            list_constraints[[index]]@mat[1, (ni + 1):nv] <- 1
            list_constraints[[index]]@dir <- "=="
            list_constraints[[index]]@rhs <- number.stimulus.UB
          } else if (number.stimulus.LB < number.stimulus.UB) {
            list_constraints[[index]]@mat <- matrix(0, nrow = 2, ncol = nv)
            list_constraints[[index]]@mat[, (ni + 1):nv] <- 1
            list_constraints[[index]]@dir <- c(">=", "<=")
            list_constraints[[index]]@rhs <- c(number.stimulus.LB, number.stimulus.UB)
          } else {
            stop(sprintf("constraint %s has invalid LB/UB", index))
          }
        } else if (constraints$CONDITION[index] %in% names(st.attrib)) {
          condition <- unique(st.attrib[constraints$CONDITION[index]])
          condition <- condition[!is.na(condition)]
          if (length(condition) == 0) {
            stop(sprintf("constraint %s returned 0 items meeting CONDITION: %s", index, constraints$CONDITION[index]))
          }
          if (any(c(constraints$LB[index], constraints$UB[index]) < 0) || constraints$LB[index] > constraints$UB[index]) {
            stop(sprintf("constraint %s has invalid LB/UB", index))
          } else if (constraints$LB[index] == constraints$UB[index]) {
            list_constraints[[index]]@mat <- matrix(0, nrow = length(condition), ncol = nv)
            list_constraints[[index]]@dir <- rep("==", length(condition))
            list_constraints[[index]]@rhs <- rep(constraints$UB[index], length(condition))
            for (m in 1:length(condition)) {
              condition_met <- which(st.attrib[constraints$CONDITION[index]] == condition[m])
              list_constraints[[index]]@mat[m, ni + condition_met] <- 1
            }
          } else if (constraints$LB[index] <= constraints$UB[index]) {
            list_constraints[[index]]@mat <- matrix(0, nrow = 2 * length(condition), ncol = nv)
            list_constraints[[index]]@dir <- rep(c(">=", "<="), length(condition))
            list_constraints[[index]]@rhs <- rep(c(constraints$LB[index], constraints$UB[index]), length(condition))
            for (m in 1:length(condition)) {
              condition_met <- which(item.attrib[constraints$CONDITION[index]] == condition[m])
              list_constraints[[index]]@mat[c(m * 2 - 1, m * 2), ni + condition_met] <- 1
            }
          }
        } else {
          condition_met <- which(with(st.attrib, eval(parse(text = constraints$CONDITION[index]))))
          if (length(condition_met) == 0) {
            stop(sprintf("constraint %s returned 0 items meeting CONDITION: %s", index, constraints$CONDITION[index]))
          }
          if (any(c(constraints$LB[index], constraints$UB[index]) < 0) || constraints$LB[index] > constraints$UB[index]) {
            stop(sprintf("constraint %s has invalid LB/UB", index))
          } else if (constraints$LB[index] == constraints$UB[index]) {
            list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
            list_constraints[[index]]@mat[1, ni + condition_met] <- 1
            list_constraints[[index]]@dir <- "=="
            list_constraints[[index]]@rhs <- constraints$UB[index]
          } else {
            list_constraints[[index]]@mat <- matrix(0, nrow = 2, ncol = nv)
            list_constraints[[index]]@mat[, ni + condition_met] <- 1
            list_constraints[[index]]@dir <- c(">=", "<=")
            list_constraints[[index]]@rhs <- c(constraints$LB[index], constraints$UB[index])
          }
        }
      }
      if (constraints$TYPE[index] %in% c("SUM", "AVERAGE", "MEAN")) {
        constraint_type_is_valid <- TRUE
        if (!(constraints$CONDITION[index] %in% names(st.attrib))) {
          stop(sprintf("constraint %s has invalid: %s not found in item.attrib:", index, constraints$CONDITION[index]))
        } else {
          if (any(is.na(st.attrib[[constraints$CONDITION[index]]]))) {
            stop(sprintf("constraint %s: %s must not have a missing value", index, constraints$CONDITION[index]))
          }
          if (any(c(constraints$LB[index], constraints$UB[index]) < 0) || constraints$LB[index] > constraints$UB[index]) {
            stop(sprintf("constraint %s has invalid LB/UB", index))
          } else if (constraints$LB[index] == constraints$UB[index]) {
            list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
            list_constraints[[index]]@dir <- "<="
            list_constraints[[index]]@rhs <- constraints$UB[index]
            if (constraints$TYPE[index] == "SUM") {
              list_constraints[[index]]@mat[1, (ni + 1):nv] <- st.attrib[[constraints$CONDITION[index]]]
            } else if (constraints$TYPE[index] %in% c("AVERAGE", "MEAN")) {
              list_constraints[[index]]@mat[1, (ni + 1):nv] <- st.attrib[[constraints$CONDITION[index]]] / number.stimulus.UB
            }
          } else {
            list_constraints[[index]]@mat <- matrix(0, nrow = 2, ncol = nv)
            list_constraints[[index]]@dir <- c(">=", "<=")
            list_constraints[[index]]@rhs <- c(constraints$LB[index], constraints$UB[index])
            if (constraints$TYPE[index] == "SUM") {
              list_constraints[[index]]@mat[, (ni + 1):nv] <- st.attrib[[constraints$CONDITION[index]]]
            } else if (constraints$TYPE[index] %in% c("AVERAGE", "MEAN")) {
              list_constraints[[index]]@mat[1, (ni + 1):nv] <- st.attrib[[constraints$CONDITION[index]]] / number.stimulus.UB
              list_constraints[[index]]@mat[2, (ni + 1):nv] <- st.attrib[[constraints$CONDITION[index]]] / number.stimulus.LB
            }
          }
        }
      }
      if (constraints$TYPE[index] == "INCLUDE") {
        constraint_type_is_valid <- TRUE
        condition_met <- which(with(st.attrib, eval(parse(text = constraints$CONDITION[index]))))
        if (length(condition_met) == 0) {
          stop(sprintf("constraint %s is invalid: %s returned 0 items", index, constraints$CONDITION[index]))
        } else {
          list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
          list_constraints[[index]]@mat[1, ni + condition_met] <- 1
          list_constraints[[index]]@dir <- "=="
          list_constraints[[index]]@rhs <- length(condition_met)
        }
      }
      if (constraints$TYPE[index] %in% c("EXCLUDE", "NOT", "NOT INCLUDE")) {
        constraint_type_is_valid <- TRUE
        condition_met <- which(with(st.attrib, eval(parse(text = constraints$CONDITION[index]))))
        if (length(condition_met) == 0) {
          stop(sprintf("constraint %s returned 0 items meeting CONDITION: %s", index, constraints$CONDITION[index]))
        } else {
          list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
          list_constraints[[index]]@mat[1, ni + condition_met] <- 1
          list_constraints[[index]]@dir <- "=="
          list_constraints[[index]]@rhs <- 0
          for (s in condition_met) {
            list_constraints[[index]]@mat[1, item_index_by_stimulus[[s]]] <- 1
          }
        }
      }
      if (constraints$TYPE[index] %in% c("ALLORNONE", "ALL OR NONE", "IIF")) {
        constraint_type_is_valid <- TRUE
        condition_met <- which(with(st.attrib, eval(parse(text = constraints$CONDITION[index]))))
        n.met <- length(condition_met)
        if (n.met < 2) {
          stop(sprintf("constraint %s is invalid: %s returned < 2 stimuli", index, constraints$CONDITION[index]))
        } else {
          list_constraints[[index]]@mat <- matrix(0, nrow = (n.met * (n.met - 1)) / 2, ncol = nv)
          list_constraints[[index]]@dir <- rep("==", (n.met * (n.met - 1)) / 2)
          list_constraints[[index]]@rhs <- rep(0, (n.met * (n.met - 1)) / 2)
          tmp.index <- 0
          for (i in condition_met) {
            for (j in condition_met) {
              if (i < j) {
                tmp.index <- tmp.index + 1
                list_constraints[[index]]@mat[tmp.index, ni + c(i, j)] <- c(1, -1)
              }
            }
          }
        }
      }
      if (constraints$TYPE[index] %in% c("MUTUALLYEXCLUSIVE", "MUTUALLY EXCLUSIVE", "XOR", "ENEMY")) {
        constraint_type_is_valid <- TRUE
        condition_met <- which(with(st.attrib, eval(parse(text = constraints$CONDITION[index]))))
        if (length(condition_met) < 2) {
          stop(sprintf("constraint %s is invalid: %s returned < 2 stimuli", index, constraints$CONDITION[index]))
        } else {
          list_constraints[[index]]@mat <- matrix(0, nrow = 1, ncol = nv)
          list_constraints[[index]]@mat[1, ni + condition_met] <- 1
          list_constraints[[index]]@dir <- "<="
          list_constraints[[index]]@rhs <- 1
        }
      }
      if (constraints$TYPE[index] == "ORDER") {
        constraint_type_is_valid <- TRUE
        if (!list_constraints[[index]]@suspend) {
          if (constraints$CONDITION[index] %in% names(st.attrib)) {
            if (any(is.na(st.attrib[[constraints$CONDITION[index]]]))) {
              stop(sprintf("constraint %s: %s must not have a missing value", index, constraints$CONDITION[index]))
            }
            stim_order <- st.attrib[[constraints$CONDITION[index]]]
            stim_order_by <- constraints$CONDITION[index]
          } else {
            stop(sprintf("constraint %s is invalid: %s not found in st.attrib", index, constraints$CONDITION[index]))
          }
        }
      }
      if (!constraint_type_is_valid) {
        stop(sprintf("constraint %s, %s is not a valid constraint type", index, constraints$TYPE[index]))
      }
      list_constraints[[index]]@nc <- nrow(list_constraints[[index]]@mat)
    }
  }
  index <- NULL
  mat <- NULL
  dir <- NULL
  rhs <- NULL
  for (index in 1:nc) {
    if (constraints$TYPE[index] != "ORDER" && !list_constraints[[index]]@suspend) {
      list_constraints[[index]]@nc <- nrow(list_constraints[[index]]@mat)
      mat <- rbind(mat, list_constraints[[index]]@mat)
      dir <- c(dir, list_constraints[[index]]@dir)
      rhs <- c(rhs, list_constraints[[index]]@rhs)
      index <- c(index, rep(constraints$CONSTRAINT[index], list_constraints[[index]]@nc))
    }
  }
  out <- list(
    constraints = constraints, listConstraints = list_constraints, pool = pool, itemAttrib = item.attrib, stAttrib = st.attrib,
    testLength = test_length, nv = nv, ni = ni, ns = ns, id = id, index = index, mat = mat, dir = dir, rhs = rhs, set_based = set_based,
    itemOrder = item_order, itemOrderBy = item_order_by, stimOrder = stim_order, stimOrderBy = stim_order_by,
    itemIndexByStimulus = item_index_by_stimulus, stimulusIndexByItem = stimulus_index_by_item)
  return(out)
}

#' Update constraints
#'
#' Update the onstraints list
#'
#' @param object a list object returned from \code{\link{loadConstraints}}
#' @param on a vector of constraints index to turn on.
#' @param off a vector of constraints index to turn off.
#'
#' @return An updated list of constraints to be used in \code{\link{ATA}} and \code{\link{Shadow}}.
#'
#' @export

updateConstraints <- function(object, on = NULL, off = NULL) {
  nc <- nrow(object$constraints)
  if (length(intersect(on, off)) > 0) {
    stop("the on- and off-vectors cannot contain a common constraint index")
  }
  if (!"ONOFF" %in% names(object$constraints)) {
    object$constraints$ONOFF <- ""
  }
  if (!is.null(on)) {
    if (any(!is.element(on, 1:nc))) {
      stop("the on-vector contains an invalid constraint index")
    }
    for (index in on) {
      object$ListConstraints[[index]]@suspend <- FALSE
      object$constraints[index, "ONOFF"] <- ""
    }
  }
  if (!is.null(off)) {
    if (any(!is.element(off, 1:nc))) {
      stop("the off-vector contains an invalid constraint index")
    }
    for (index in off) {
      object$ListConstraints[[index]]@suspend <- TRUE
      object$constraints[index, "ONOFF"] <- "OFF"
    }
  }
  index <- NULL
  mat <- NULL
  dir <- NULL
  rhs <- NULL
  for (index in 1:nc) {
    if (object$constraints$TYPE[index] != "ORDER" && !object$ListConstraints[[index]]@suspend) {
      object$ListConstraints[[index]]@nc <- nrow(object$ListConstraints[[index]]@mat)
      mat <- rbind(mat, object$ListConstraints[[index]]@mat)
      dir <- c(dir, object$ListConstraints[[index]]@dir)
      rhs <- c(rhs, object$ListConstraints[[index]]@rhs)
      index <- c(index, rep(object$constraints$CONSTRAINT[index], object$ListConstraints[[index]]@nc))
    }
  }
  object$index <- index
  object$mat <- mat
  object$dir <- dir
  object$rhs <- rhs
  return(object)
}

#' Build constraints
#'
#' Read constraints from specified files.
#'
#' @param pool An \code{item.pool} object. Use \code{\link{loadItemPool}} for this.
#' @param file.constraints Character. The name of the file containing constraint specifications.
#' @param file.item.attrib Character. The name of the file containing item attributes.
#' @param file.st.attrib (Optional) Character. The name of the file containing set attributes.
#'
#' @return A list containing the parsed constraints, to be used in \code{\link{ATA}} and \code{\link{Shadow}}.
#'
#' @export

buildConstraints <- function(pool, file.constraints, file.item.attrib, file.st.attrib = NULL) {
  item_attrib <- loadItemAttrib(file.item.attrib, pool)
  if (!is.null(file.st.attrib)) {
    st_attrib <- loadStAttrib(file.st.attrib, item_attrib)
  } else {
    st_attrib <- NULL
  }
  constraints <- loadConstraints(file.constraints, pool, item_attrib, st_attrib)
  return(constraints)
}
