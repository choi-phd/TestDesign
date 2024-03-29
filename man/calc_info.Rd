% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R, R/cpp_calc_documents.r
\name{calc_info}
\alias{calc_info}
\alias{calc_info_matrix}
\title{(C++) For multiple items, calculate Fisher information}
\usage{
calc_info(x, item_parm, ncat, model)

calc_info_matrix(x, item_parm, ncat, model)
}
\arguments{
\item{x}{the theta value. This must be a column vector in matrix form for \code{calc_info_matrix()}.}

\item{item_parm}{a matrix containing item parameters. Each row should represent an item.}

\item{ncat}{a vector containing the number of response categories of each item.}

\item{model}{a vector indicating item models of each item, using \itemize{
  \item{\code{1}}: 1PL model
  \item{\code{2}}: 2PL model
  \item{\code{3}}: 3PL model
  \item{\code{4}}: PC model
  \item{\code{5}}: GPC model
  \item{\code{6}}: GR model
}}
}
\description{
\code{calc_info()} and \code{calc_info_matrix()} are functions for calculating Fisher information.
These functions are designed for multiple items.
}
\details{
\code{calc_info()} accepts a single theta value, and \code{calc_info_matrix()} accepts multiple theta values.

Currently supports unidimensional models.
}
\examples{
# item parameters
item_parm <- matrix(c(
  1, NA,   NA,
  1,  2,   NA,
  1,  2, 0.25,
  0,  1,   NA,
  2,  0,    1,
  2,  0,    2),
  nrow = 6,
  byrow = TRUE
)

ncat  <- c(2, 2, 2, 3, 3, 3)
model <- c(1, 2, 3, 4, 5, 6)

# single theta example
x <- 0.5
calc_info(x, item_parm, ncat, model)

# multiple thetas example
x <- matrix(seq(0.1, 0.5, 0.1)) # column vector in matrix form
calc_info_matrix(x, item_parm, ncat, model)

}
\references{
Rasch, G. (1960). \emph{Probabilistic models for some intelligence and attainment tests.}
Copenhagen: Danish Institute for Educational Research.

Lord, F. M. (1952). A theory of test scores (Psychometric Monograph No. 7). Richmond, VA: Psychometric Corporation.

Birnbaum, A. (1957). \emph{Efficient design and use of tests of mental ability for various decision-making problems}
(Series Report No. 58-16. Project No. 7755-23).
Randolph Air Force Base, TX: USAF School of Aviation Medicine.

Birnbaum, A. (1958). \emph{On the estimation of mental ability}
(Series Report No. 15. Project No. 7755-23).
Randolph Air Force Base, TX: USAF School of Aviation Medicine.

Birnbaum, A. (1958). \emph{Further considerations of efficiency in tests of a mental ability}
(Series Report No. 17. Project No. 7755-23).
Randolph Air Force Base, TX: USAF School of Aviation Medicine.

Birnbaum, A. (1968). Some latent trait models and their use in inferring an examinee's ability.
In Lord, F. M., Novick, M. R. (eds.), \emph{Statistical Theories of Mental Test Scores}, 397-479.
Reading, MA: Addison-Wesley.

Masters, G. N. (1982). A Rasch model for partial credit scoring.
\emph{Psychometrika, 47}(2), 149-174.

Andrich, D. (1978). A rating formulation for ordered response categories.
\emph{Psychometrika, 43}(4), 561-573.

Muraki, E. (1992). A generalized partial credit model: Application of an EM algorithm.
\emph{Applied Psychological Measurement, 16}(2), 159-176.

Samejima, F. (1969). Estimation of latent ability using a response pattern of graded scores.
\emph{Psychometrika Monograph, 17}.
}
