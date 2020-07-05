#' @include shadow_functions.R
NULL

#' @noRd
initializeTheta <- function(config, constants, posterior_record) {
  nj <- constants$nj
  if (!is.null(config@item_selection$initial_theta)) {
    theta <- rep(config@item_selection$initial_theta, nj)
  } else {
    theta <- as.vector(posterior_record$posterior %*% matrix(constants$theta_q, ncol = 1))
  }
  return(theta)
}
