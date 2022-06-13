#include "item_functions.h"

//' @rdname e_item
//' @export
// [[Rcpp::export]]
double e_1pl(
  const arma::rowvec& x,
  const double& b
) {
  return p_1pl(x, b);
}

//' @rdname e_item
//' @export
// [[Rcpp::export]]
double e_2pl(
  const arma::rowvec& x,
  const double& a,
  const double& b
) {
  return p_2pl(x, a, b);
}

//' @rdname e_item
//' @export
// [[Rcpp::export]]
double e_3pl(
  const arma::rowvec& x,
  const double& a,
  const double& b,
  const double& c
) {
  return p_3pl(x, a, b, c);
}

//' @rdname e_item
//' @export
// [[Rcpp::export]]
double e_pc(
  const arma::rowvec& x,
  const arma::rowvec& b
) {
  arma::colvec k = arma::regspace(0, b.n_elem);
  arma::rowvec p = p_pc(x, b);
  return arma::as_scalar(p * k);
}

//' @rdname e_item
//' @export
// [[Rcpp::export]]
double e_gpc(
  const arma::rowvec& x,
  const double& a,
  const arma::rowvec& b
) {
  arma::colvec k = arma::regspace(0, b.n_elem);
  arma::rowvec p = p_gpc(x, a, b);
  return arma::as_scalar(p * k);
}

//' @rdname e_item
//' @export
// [[Rcpp::export]]
double e_gr(
  const arma::rowvec& x,
  const double& a,
  const arma::rowvec& b
) {
  arma::colvec k = arma::regspace(0, b.n_elem);
  arma::rowvec p = p_gr(x, a, b);
  return arma::as_scalar(p * k);
}
