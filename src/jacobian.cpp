#include "expected_score.h"
#include "item_probability.h"
#include "jacobian.h"

//' @rdname j_item
//' @export
// [[Rcpp::export]]
double j_1pl(
  const arma::rowvec& x,
  const double& b,
  const double& u
) {
  return (u - e_1pl(x, b));
}

//' @rdname j_item
//' @export
// [[Rcpp::export]]
double j_2pl(
  const arma::rowvec& x,
  const double& a,
  const double& b,
  const double& u
) {
  return (a * (u - e_2pl(x, a, b)));
}

//' @rdname j_item
//' @export
// [[Rcpp::export]]
double j_3pl(
  const arma::rowvec& x,
  const double& a,
  const double& b,
  const double& c,
  const double& u
) {
  double e = e_3pl(x, a, b, c);
  return (a * (u - e) * (e - c) / (e * (1.0 - c)));
}

//' @rdname j_item
//' @export
// [[Rcpp::export]]
double j_pc(
  const arma::rowvec& x,
  const arma::rowvec& b,
  const double& u
) {
  return (u - e_pc(x, b));
}

//' @rdname j_item
//' @export
// [[Rcpp::export]]
double j_gpc(
  const arma::rowvec& x,
  const double& a,
  const arma::rowvec& b,
  const double& u
) {
  return (a * (u - e_gpc(x, a, b)));
}

//' @rdname j_item
//' @export
// [[Rcpp::export]]
double j_gr(
  const arma::rowvec& x,
  const double& a,
  const arma::rowvec& b,
  const double& u
) {

  int nk = b.n_elem + 1;

  arma::rowvec p = p_gr(x, a, b);

  arma::rowvec p_star(nk + 1);
  p_star(0) = 1;
  p_star(nk) = 0;

  for (int k = 1; k < nk; k++) {
    p_star(k) = p_star(k - 1) - p(k - 1);
  }

  double o = ((p_star(u) * (1 - p_star(u))) - (p_star(u + 1) * (1 - p_star(u + 1))));
  return (a * o / p(u));

}

//' @rdname j_item
//' @export
// [[Rcpp::export]]
arma::colvec array_j_1pl(
  const arma::mat& x,
  const double& b,
  const double& u
) {

  int nx = x.n_rows;
  arma::colvec j_array(nx);

  for (int j = 0; j < nx; j++) {
    j_array(j) = j_1pl(x.row(j), b, u);
  }

  return j_array;

}

//' @rdname j_item
//' @export
// [[Rcpp::export]]
arma::colvec array_j_2pl(
  const arma::mat& x,
  const double& a,
  const double& b,
  const double& u
) {

  int nx = x.n_rows;
  arma::colvec j_array(nx);

  for (int j = 0; j < nx; j++) {
    j_array(j) = j_2pl(x.row(j), a, b, u);
  }

  return j_array;

}

//' @rdname j_item
//' @export
// [[Rcpp::export]]
arma::colvec array_j_3pl(
  const arma::mat& x,
  const double& a,
  const double& b,
  const double& c,
  const double& u
) {

  int nx = x.n_rows;
  arma::colvec j_array(nx);

  for (int j = 0; j < nx; j++) {
    j_array(j) = j_3pl(x.row(j), a, b, c, u);
  }

  return j_array;

}

//' @rdname j_item
//' @export
// [[Rcpp::export]]
arma::colvec array_j_pc(
  const arma::mat& x,
  const arma::rowvec& b,
  const double& u
) {

  int nx = x.n_rows;
  arma::colvec j_array(nx);

  for (int j = 0; j < nx; j++) {
    j_array(j) = j_pc(x.row(j), b, u);
  }

  return j_array;

}

//' @rdname j_item
//' @export
// [[Rcpp::export]]
arma::colvec array_j_gpc(
  const arma::mat& x,
  const double& a,
  const arma::rowvec& b,
  const double& u
) {

  int nx = x.n_rows;
  arma::colvec j_array(nx);

  for (int j = 0; j < nx; j++) {
    j_array(j) = j_gpc(x.row(j), a, b, u);
  }

  return j_array;

}

//' @rdname j_item
//' @export
// [[Rcpp::export]]
arma::colvec array_j_gr(
  const arma::mat& x,
  const double& a,
  const arma::rowvec& b,
  const double& u
) {

  int nx = x.n_rows;
  arma::colvec j_array(nx);

  for (int j = 0; j < nx; j++) {
    j_array(j) = j_gr(x.row(j), a, b, u);
  }

  return j_array;

}
