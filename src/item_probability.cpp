#include "item_probability.h"

// exp(x) / (1 + exp(x)) is equivalent to
// 1 / (1 + exp(-x))

//' @rdname p_item
//' @export
// [[Rcpp::export]]
double p_1pl(
  const arma::rowvec& x,
  const double& b
) {
  double xmul = x(0) - b;
  return 1 / (1 + std::exp(-xmul));
}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
double p_2pl(
  const arma::rowvec& x,
  const double& a,
  const double& b
) {
  double xmul = a * (x(0) - b);
  return 1 / (1 + std::exp(-xmul));
}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
double p_m_2pl(
  const arma::rowvec& x,
  const arma::rowvec& a,
  const double& d
) {
  double xmul = arma::as_scalar(a * x.t() + d);
  return 1 / (1 + std::exp(-xmul));
}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
double p_3pl(
  const arma::rowvec& x,
  const double& a,
  const double& b,
  const double& c
) {
  double xmul = a * (x(0) - b);
  return c + (1 - c) / (1 + std::exp(-xmul));
}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
double p_m_3pl(
  const arma::rowvec& x,
  const arma::rowvec& a,
  const double& d,
  const double& c
) {
  double xmul = arma::as_scalar(a * x.t() + d);
  return c + (1 - c) / (1 + std::exp(-xmul));
}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::rowvec p_pc(
  const arma::rowvec& x,
  const arma::rowvec& b
) {

  int nk = b.n_cols + 1;
  arma::rowvec xpart(nk);
  xpart(0) = x(0);

  for (int k = 1; k < nk; k++) {
    xpart(k) = x(0) - b(k - 1);
  }

  arma::rowvec xmul = cumsum(xpart);
  arma::rowvec num = exp(xmul);
  double denom = sum(num);

  return num / denom;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::rowvec p_gpc(
  const arma::rowvec& x,
  const double& a,
  const arma::rowvec& b
) {

  int nk = b.n_cols + 1;
  arma::rowvec xpart(nk);
  xpart(0) = a * x(0);

  for (int k = 1; k < nk; k++) {
    xpart(k) = a * (x(0) - b(k - 1));
  }

  arma::rowvec xmul = cumsum(xpart);
  arma::rowvec num = exp(xmul);
  double denom = sum(num);

  return num / denom;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::rowvec p_m_gpc(
  const arma::rowvec& x,
  const arma::rowvec& a,
  const arma::rowvec& d
) {

  double ax = arma::as_scalar(a * x.t());
  int nk = d.n_cols + 1;
  arma::rowvec xmul(nk);
  xmul(0) = 0;

  for (int k = 1; k < nk; k++) {
    xmul(k) = k * ax + d(k - 1);
  }

  rowvec num = exp(xmul);
  double denom = sum(num);
  return num / denom;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::rowvec p_gr(
  const arma::rowvec& x,
  const double& a,
  const arma::rowvec& b
) {

  int nk = b.n_cols + 1;
  arma::rowvec p(nk), p_star(nk + 1);
  p_star(0) = 1;
  p_star(nk) = 0;

  for (int k = 1; k < nk; k++) {
    p_star(k) = p_2pl(x, a, b(k - 1));
  }
  for (int k = 0; k < nk; k++) {
    p(k) = p_star(k) - p_star(k + 1);
  }

  return p;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::rowvec p_m_gr(
  const arma::rowvec& x,
  const arma::rowvec& a,
  const arma::rowvec& d
) {

  int nk = d.n_cols + 1;
  arma::rowvec p(nk), p_star(nk + 1);
  p_star(0) = 1;
  p_star(nk) = 0;

  for (int k = 1; k < nk; k++) {
    p_star(k) = p_m_2pl(x, a, d(k - 1));
  }
  for (int k = 0; k < nk; k++) {
    p(k) = p_star(k) - p_star(k + 1);
  }

  return p;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::colvec array_p_1pl(
  const arma::mat& x,
  const double& b
) {

  int nx = x.n_rows;
  arma::colvec p_array(nx);

  for (int j = 0; j < nx; j++) {
    p_array(j) = p_1pl(x.row(j), b);
  }

  return p_array;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::colvec array_p_2pl(
  const arma::mat& x,
  const double& a,
  const double& b
) {

  int nx = x.n_rows;
  arma::colvec p_array(nx);

  for (int j = 0; j < nx; j++) {
    p_array(j) = p_2pl(x.row(j), a, b);
  }

  return p_array;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::colvec array_p_m_2pl(
  const arma::mat& x,
  const arma::rowvec& a,
  const double& d
) {

  int nx = x.n_rows;
  arma::colvec p_array(nx);

  for (int j = 0; j < nx; j++) {
    p_array(j) = p_m_2pl(x.row(j), a, d);
  }

  return p_array;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::colvec array_p_3pl(
  const arma::mat& x,
  const double& a,
  const double& b,
  const double& c
) {

  int nx = x.n_rows;
  arma::colvec p_array(nx);

  for (int j = 0; j < nx; j++) {
    p_array(j) = p_3pl(x.row(j), a, b, c);
  }

  return p_array;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::colvec array_p_m_3pl(
  const arma::mat& x,
  const arma::rowvec& a,
  const double& d,
  const double& c
) {

  int nx = x.n_rows;
  arma::colvec p_array(nx);

  for (int j = 0; j < nx; j++) {
    p_array(j) = p_m_3pl(x.row(j), a, d, c);
  }

  return p_array;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::mat array_p_pc(
  const arma::mat& x,
  const arma::rowvec& b
) {

  int nx = x.n_rows;
  int nk = b.n_cols + 1;
  arma::mat p_array(nx, nk);

  for (int j = 0; j < nx; j++) {
    p_array.row(j) = p_pc(x.row(j), b);
  }

  return p_array;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::mat array_p_gpc(
  const arma::mat& x,
  const double& a,
  const arma::rowvec& b
) {

  int nx = x.n_rows;
  int nk = b.n_cols + 1;
  arma::mat p_array(nx, nk);

  for (int j = 0; j < nx; j++) {
    p_array.row(j) = p_gpc(x.row(j), a, b);
  }

  return p_array;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::mat array_p_m_gpc(
  const arma::mat& x,
  const arma::rowvec& a,
  const arma::rowvec& d
) {

  int nx = x.n_rows;
  int nk = d.n_cols + 1;
  arma::mat p_array(nx, nk);

  for (int j = 0; j < nx; j++) {
    p_array.row(j) = p_m_gpc(x.row(j), a, d);
  }

  return p_array;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::mat array_p_gr(
  const arma::mat& x,
  const double& a,
  const arma::rowvec& b
) {

  int nx = x.n_rows;
  int nk = b.n_cols + 1;
  arma::mat p_array(nx, nk);

  for (int j = 0; j < nx; j++) {
    p_array.row(j) = p_gr(x.row(j), a, b);
  }

  return p_array;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::mat array_p_m_gr(
  const arma::mat& x,
  const arma::rowvec& a,
  const arma::rowvec& d
) {

  int nx = x.n_rows;
  int nk = d.n_cols + 1;
  arma::mat p_array(nx, nk);

  for (int j = 0; j < nx; j++) {
    p_array.row(j) = p_m_gr(x.row(j), a, d);
  }

  return p_array;

}
