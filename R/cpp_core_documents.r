#' @include item_class.R
NULL

#' (C++) Calculate item response probability
#'
#' \code{p_*()} and \code{array_p_*()} are C++ functions for calculating item response probability.
#'
#' \code{p_*()} functions accept a single theta value, and \code{array_p_*()} functions accept multiple theta values.
#'
#' Supports unidimensional and multidimensional models.
#'
#' \itemize{
#'   \item{\code{p_1pl()}, \code{array_p_1pl()}}: 1PL models
#'   \item{\code{p_2pl()}, \code{array_p_2pl()}}: 2PL models
#'   \item{\code{p_3pl()}, \code{array_p_3pl()}}: 3PL models
#'   \item{\code{p_pc()}, \code{array_p_pc()}}: PC (partial credit) models
#'   \item{\code{p_gpc()}, \code{array_p_gpc()}}: GPC (generalized partial credit) models
#'   \item{\code{p_gr()}, \code{array_p_gr()}}: GR (graded response) models
#'   \item{\code{p_m_2pl()}, \code{array_p_m_2pl()}}: multidimensional 2PL models
#'   \item{\code{p_m_3pl()}, \code{array_p_m_3pl()}}: multidimensional 3PL models
#'   \item{\code{p_m_gpc()}, \code{array_p_m_gpc()}}: multidimensional GPC models
#'   \item{\code{p_m_gr()}, \code{array_p_m_gr()}}: multidimensional GR models
#' }
#'
#' @param x the theta value. The number of columns should correspond to the number of dimensions.
#' For \code{array_*()} functions, the number of theta values must correspond to the number of rows.
#' @param a the \emph{a}-parameter.
#' @param b,d the difficulty parameter. \code{b} is used for unidimensional items, and \code{d} is used for multidimensional items.
#' @param c the \emph{c}-parameter.
#'
#' @examples
#' x <- 0.5
#'
#' p_1pl(x, 1)
#' p_2pl(x, 1, 2)
#' p_3pl(x, 1, 2, 0.25)
#' p_pc(x, c(0, 1))
#' p_gpc(x, 2, c(0, 1))
#' p_gr(x, 2, c(0, 2))
#'
#' x <- matrix(seq(0.1, 0.5, 0.1)) # three theta values, unidimensional
#'
#' array_p_1pl(x, 1)
#' array_p_2pl(x, 1, 2)
#' array_p_3pl(x, 1, 2, 0.25)
#' array_p_pc(x, c(0, 1))
#' array_p_gpc(x, 2, c(0, 1))
#' array_p_gr(x, 2, c(0, 2))
#'
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @name p_item
#' @order 0
NULL

#' (C++) Calculate expected scores
#'
#' \code{e_*()} and \code{array_e_*()} are C++ functions for calculating expected scores.
#'
#' \code{e_*()} functions accept a single theta value, and \code{array_p_*()} functions accept multiple theta values.
#'
#' Supports unidimensional and multidimensional models.
#'
#' \itemize{
#'   \item{\code{e_1pl()}, \code{array_e_1pl()}}: 1PL models
#'   \item{\code{e_2pl()}, \code{array_e_2pl()}}: 2PL models
#'   \item{\code{e_3pl()}, \code{array_e_3pl()}}: 3PL models
#'   \item{\code{e_pc()}, \code{array_e_pc()}}: PC (partial credit) models
#'   \item{\code{e_gpc()}, \code{array_e_gpc()}}: GPC (generalized partial credit) models
#'   \item{\code{e_gr()}, \code{array_e_gr()}}: GR (graded response) models
#'   \item{\code{e_m_2pl()}, \code{array_e_m_2pl()}}: multidimensional 2PL models
#'   \item{\code{e_m_3pl()}, \code{array_e_m_3pl()}}: multidimensional 3PL models
#'   \item{\code{e_m_gpc()}, \code{array_e_m_gpc()}}: multidimensional GPC models
#'   \item{\code{e_m_gr()}, \code{array_e_m_gr()}}: multidimensional GR models
#' }
#'
#' @param x the theta value. The number of columns should correspond to the number of dimensions.
#' For \code{array_*()} functions, the number of theta values must correspond to the number of rows.
#' @param a the \emph{a}-parameter.
#' @param b,d the difficulty parameter. \code{b} is used for unidimensional items, and \code{d} is used for multidimensional items.
#' @param c the \emph{c}-parameter.
#'
#' @examples
#' x <- 0.5
#'
#' e_1pl(x, 1)
#' e_2pl(x, 1, 2)
#' e_3pl(x, 1, 2, 0.25)
#' e_pc(x, c(0, 1))
#' e_gpc(x, 2, c(0, 1))
#' e_gr(x, 2, c(0, 2))
#'
#' x <- matrix(seq(-3, 3, 1)) # three theta values, unidimensional
#'
#' array_e_1pl(x, 1)
#' array_e_2pl(x, 1, 2)
#' array_e_3pl(x, 1, 2, 0.25)
#' array_e_pc(x, c(0, 1))
#' array_e_gpc(x, 2, c(0, 1))
#' array_e_gr(x, 2, c(0, 2))
#'
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @name e_item
#' @order 0
NULL

#' (C++) Calculate Fisher information
#'
#' \code{info_*()} and \code{array_info_*()} are functions for calculating Fisher information.
#'
#' \code{info_*()} functions accept a single theta value, and \code{array_info_*} functions accept multiple theta values.
#'
#' Supports unidimensional and multidimensional models.
#'
#' \itemize{
#'   \item{\code{info_1pl()}, \code{array_info_1pl()}}: 1PL models
#'   \item{\code{info_2pl()}, \code{array_info_2pl()}}: 2PL models
#'   \item{\code{info_3pl()}, \code{array_info_3pl()}}: 3PL models
#'   \item{\code{info_pc()}, \code{array_info_pc()}}: PC (partial credit) models
#'   \item{\code{info_gpc()}, \code{array_info_gpc()}}: GPC (generalized partial credit) models
#'   \item{\code{info_gr()}, \code{array_info_gr()}}: GR (graded response) models
#'   \item{\code{info_m_2pl()}, \code{array_info_m_2pl()}}: multidimensional 2PL models
#'   \item{\code{info_m_3pl()}, \code{array_info_m_3pl()}}: multidimensional 3PL models
#'   \item{\code{info_m_gpc()}, \code{array_info_m_gpc()}}: multidimensional GPC models
#'   \item{\code{info_m_gr()}, \code{array_info_m_gr()}}: multidimensional GR models
#'   \item{Directional information for a specific angle}
#'   \itemize{
#'     \item{\code{thisdirinfo_m_2pl()}, \code{array_thisdirinfo_m_2pl()}}: multidimensional 2PL models
#'     \item{\code{thisdirinfo_m_3pl()}, \code{array_thisdirinfo_m_3pl()}}: multidimensional 3PL models
#'     \item{\code{thisdirinfo_m_gpc()}, \code{array_thisdirinfo_m_gpc()}}: multidimensional GPC models
#'     \item{\code{thisdirinfo_m_gr()}, \code{array_thisdirinfo_m_gr()}}: multidimensional GR models
#'   }
#' }
#'
#' @param x the theta value. The number of columns should correspond to the number of dimensions.
#' For \code{array_*()} functions, the number of theta values must correspond to the number of rows.
#' @param a the \emph{a}-parameter.
#' @param b,d the difficulty parameter. \code{b} is used for unidimensional items, and \code{d} is used for multidimensional items.
#' @param c the \emph{c}-parameter.
#' @param alpha_vec the alpha angle vector. Used for directional information in \code{thisdirinfo_*()} and \code{array_thisdirinfo_*()}.
#'
#' @examples
#' x <- 0.5
#'
#' info_1pl(x, 1)
#' info_2pl(x, 1, 2)
#' info_3pl(x, 1, 2, 0.25)
#' info_pc(x, c(0, 1))
#' info_gpc(x, 2, c(0, 1))
#' info_gr(x, 2, c(0, 2))
#'
#' x <- matrix(seq(0.1, 0.5, 0.1)) # three theta values, unidimensional
#'
#' array_info_1pl(x, 1)
#' array_info_2pl(x, 1, 2)
#' array_info_3pl(x, 1, 2, 0.25)
#' array_info_pc(x, c(0, 1))
#' array_info_gpc(x, 2, c(0, 1))
#' array_info_gr(x, 2, c(0, 2))
#'
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @name info_item
#' @order 0
NULL

#' (C++) Calculate first derivative of log-likelihood
#'
#' \code{j_*()} and \code{array_j_*()} are C++ functions for calculating the first derivative of the log-likelihood function.
#'
#' \code{j_*()} functions accept a single theta value, and \code{array_j_*()} functions accept multiple theta values.
#'
#' Supports unidimensional and multidimensional models.
#'
#' \itemize{
#'   \item{\code{j_1pl()}, \code{array_j_1pl()}}: 1PL models
#'   \item{\code{j_2pl()}, \code{array_j_2pl()}}: 2PL models
#'   \item{\code{j_3pl()}, \code{array_j_3pl()}}: 3PL models
#'   \item{\code{j_pc()}, \code{array_j_pc()}}: PC (partial credit) models
#'   \item{\code{j_gpc()}, \code{array_j_gpc()}}: GPC (generalized partial credit) models
#'   \item{\code{j_gr()}, \code{array_j_gr()}}: GR (graded response) models
#'   \item{\code{j_m_2pl()}, \code{array_j_m_2pl()}}: multidimensional 2PL models
#'   \item{\code{j_m_3pl()}, \code{array_j_m_3pl()}}: multidimensional 3PL models
#'   \item{\code{j_m_gpc()}, \code{array_j_m_gpc()}}: multidimensional GPC models
#'   \item{\code{j_m_gr()}, \code{array_j_m_gr()}}: multidimensional GR models
#' }
#'
#' @param x the theta value. The number of columns should correspond to the number of dimensions.
#' For \code{array_*()} functions, the number of theta values must correspond to the number of rows.
#' @param a the \emph{a}-parameter.
#' @param b,d the difficulty parameter. \code{b} is used for unidimensional items, and \code{d} is used for multidimensional items.
#' @param c the \emph{c}-parameter.
#' @param u the response value.
#'
#' @examples
#' u <- 1
#'
#' x <- 0.5
#' j_1pl(x, 1, u)
#' j_2pl(x, 1, 2, u)
#' j_3pl(x, 1, 2, 0.25, u)
#' j_pc(x, c(0, 1), u)
#' j_gpc(x, 2, c(0, 1), u)
#' j_gr(x, 2, c(0, 2), u)
#'
#' x <- matrix(seq(-3, 3, 1)) # three theta values, unidimensional
#' array_j_1pl(x, 1, u)
#' array_j_2pl(x, 1, 2, u)
#' array_j_3pl(x, 1, 2, 0.25, u)
#' array_j_pc(x, c(0, 1), u)
#' array_j_gpc(x, 2, c(0, 1), u)
#' array_j_gr(x, 2, c(0, 2), u)
#'
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @name j_item
#' @order 0
NULL

#' (C++) Calculate second derivative of log-likelihood
#'
#' \code{h_*()} and \code{array_h_*()} are C++ functions for calculating the second derivative of the log-likelihood function.
#'
#' \code{h_*()} functions accept a single theta value, and \code{array_h_*()} functions accept multiple theta values.
#'
#' Supports unidimensional and multidimensional models.
#'
#' \itemize{
#'   \item{\code{h_1pl()}, \code{array_h_1pl()}}: 1PL models
#'   \item{\code{h_2pl()}, \code{array_h_2pl()}}: 2PL models
#'   \item{\code{h_3pl()}, \code{array_h_3pl()}}: 3PL models
#'   \item{\code{h_pc()}, \code{array_h_pc()}}: PC (partial credit) models
#'   \item{\code{h_gpc()}, \code{array_h_gpc()}}: GPC (generalized partial credit) models
#'   \item{\code{h_gr()}, \code{array_h_gr()}}: GR (graded response) models
#'   \item{\code{h_m_2pl()}, \code{array_h_m_2pl()}}: multidimensional 2PL models
#'   \item{\code{h_m_3pl()}, \code{array_h_m_3pl()}}: multidimensional 3PL models
#'   \item{\code{h_m_gpc()}, \code{array_h_m_gpc()}}: multidimensional GPC models
#'   \item{\code{h_m_gr()}, \code{array_h_m_gr()}}: multidimensional GR models
#' }
#'
#' @param x the theta value. The number of columns should correspond to the number of dimensions.
#' For \code{array_*()} functions, the number of theta values must correspond to the number of rows.
#' @param a the \emph{a}-parameter.
#' @param b,d the difficulty parameter. \code{b} is used for unidimensional items, and \code{d} is used for multidimensional items.
#' @param c the \emph{c}-parameter.
#' @param u the response value.
#'
#' @examples
#' u <- 1
#'
#' x <- 0.5
#' h_1pl(x, 1, u)
#' h_2pl(x, 1, 2, u)
#' h_3pl(x, 1, 2, 0.25, u)
#' h_pc(x, c(0, 1), u)
#' h_gpc(x, 2, c(0, 1), u)
#' h_gr(x, 2, c(0, 2), u)
#'
#' x <- matrix(seq(-3, 3, 1)) # three theta values, unidimensional
#' array_h_1pl(x, 1, u)
#' array_h_2pl(x, 1, 2, u)
#' array_h_3pl(x, 1, 2, 0.25, u)
#' array_h_pc(x, c(0, 1), u)
#' array_h_gpc(x, 2, c(0, 1), u)
#' array_h_gr(x, 2, c(0, 2), u)
#'
#' @template 1pl-ref
#' @template 2pl-ref
#' @template 3pl-ref
#' @template pc-ref
#' @template gpc-ref
#' @template gr-ref
#'
#' @name h_item
#' @order 0
NULL

#' Calculate alpha angles from a-parameters
#'
#' \code{a_to_alpha} is a function for converting an a-parameter vector to an alpha angle vector.
#' The returned values are in the radian metric.
#'
#' @param a the \emph{a}-parameter vector.
#'
#' @examples
#' a_to_alpha(c(1, 1))
#'
#' @name a_to_alpha
NULL
