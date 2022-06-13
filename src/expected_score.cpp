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

//' @rdname e_item
//' @export
// [[Rcpp::export]]
arma::colvec array_e_1pl(
  const arma::mat& x,
  const double& b
) {

  int nx = x.n_rows;
  arma::colvec e_array(nx);

  for (int j = 0; j < nx; j++) {
    e_array(j) = e_1pl(x.row(j), b);
  }

  return e_array;

}

//' @rdname e_item
//' @export
// [[Rcpp::export]]
arma::colvec array_e_2pl(
  const arma::mat& x,
  const double& a,
  const double& b
) {

  int nx = x.n_rows;
  arma::colvec e_array(nx);

  for (int j = 0; j < nx; j++) {
    e_array(j) = e_2pl(x.row(j), a, b);
  }

  return e_array;

}

//' @rdname e_item
//' @export
// [[Rcpp::export]]
arma::colvec array_e_3pl(
  const arma::mat& x,
  const double& a,
  const double& b,
  const double& c
) {

  int nx = x.n_rows;
  arma::colvec e_array(nx);

  for (int j = 0; j < nx; j++) {
    e_array(j) = e_3pl(x.row(j), a, b, c);
  }

  return e_array;

}

//' @rdname e_item
//' @export
// [[Rcpp::export]]
arma::colvec array_e_pc(
  const arma::mat& x,
  const arma::rowvec& b
) {

  int nx = x.n_rows;
  arma::colvec e_array(nx);

  for (int j = 0; j < nx; j++) {
    e_array(j) = e_pc(x.row(j), b);
  }

  return e_array;

}

//' @rdname e_item
//' @export
// [[Rcpp::export]]
arma::colvec array_e_gpc(
  const arma::mat& x,
  const double& a,
  const arma::rowvec& b
) {

  int nx = x.n_rows;
  arma::colvec e_array(nx);

  for (int j = 0; j < nx; j++) {
    e_array(j) = e_gpc(x.row(j), a, b);
  }

  return e_array;

}

//' @rdname e_item
//' @export
// [[Rcpp::export]]
arma::colvec array_e_gr(
  const arma::mat& x,
  const double& a,
  const arma::rowvec& b
) {

  int nx = x.n_rows;
  arma::colvec e_array(nx);

  for (int j = 0; j < nx; j++) {
    e_array(j) = e_gr(x.row(j), a, b);
  }

  return e_array;

}
