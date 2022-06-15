#include "item_functions.h"
#include "expected_score.h"

//' @rdname h_item
//' @export
// [[Rcpp::export]]
double h_1pl(
  const arma::rowvec& x,
  const double& b,
  const double& u
) {
  return (-info_1pl(x, b));
}

//' @rdname h_item
//' @export
// [[Rcpp::export]]
double h_2pl(
  const arma::rowvec& x,
  const double& a,
  const double& b,
  const double& u
) {
  return (-info_2pl(x, a, b));
}

//' @rdname h_item
//' @export
// [[Rcpp::export]]
double h_3pl(
  const arma::rowvec& x,
  const double& a,
  const double& b,
  const double& c,
  const double& u
) {
  double p = p_3pl(x, a, b, c);
  return (
    pow(a, 2) * (1 - p) * (p - c) *
    ((c * u) - pow(p, 2)) / (pow(p, 2) * pow(1 - c, 2))
  );
}

//' @rdname h_item
//' @export
// [[Rcpp::export]]
double h_pc(
  const arma::rowvec& x,
  const arma::rowvec& b,
  const double& u
) {
  return (-info_pc(x, b));
}

//' @rdname h_item
//' @export
// [[Rcpp::export]]
double h_gpc(
  const arma::rowvec& x,
  const double& a,
  const arma::rowvec& b,
  const double& u
) {
  return (-info_gpc(x, a, b));
}

//' @rdname h_item
//' @export
// [[Rcpp::export]]
double h_gr(
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

  double o = (
    (p_star(u    ) * (1 - p_star(u    )) * (1 - (2 * p_star(u    )))) -
    (p_star(u + 1) * (1 - p_star(u + 1)) * (1 - (2 * p_star(u + 1))))
  );
  double oo = (
    (p_star(u    ) * (1 - p_star(u    ))) -
    (p_star(u + 1) * (1 - p_star(u + 1)))
  );

  return (
    pow(a, 2) * (
      (o / p(u)) -
      (pow(oo, 2) / pow(p(u), 2))
    )
  );

}
