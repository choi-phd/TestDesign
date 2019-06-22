#' @param resp A numeric vector of item responses.
#' @param ncat A numeric vector of the number of response categories by item.
#' @param model A numeric vector indicating the IRT models of each item (1: 1PL, 2: 2PL, 3: 3PL, 4: PC, 5: GPC, 6: GR).
#' @param prior The type of prior distribution (1: normal, 2: uniform).
#' @param prior_parm A numeric vector of hyperparameters for the prior distribution, c(mu, sigma) or c(ll, ul).
