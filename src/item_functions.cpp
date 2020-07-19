#include "item_functions.h"

//' @rdname p_item
//' @export
// [[Rcpp::export]]
double p_1pl(const arma::rowvec& x, const double& b) {
  double xmul = x(0);
  return 1 / (1 + std::exp(b - xmul));
}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
double p_2pl(const arma::rowvec& x, const double& a, const double& b) {
  double xmul = x(0);
  return 1 / (1 + std::exp(-a * (xmul - b)));
}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
double p_3pl(const arma::rowvec& x, const double& a, const double& b, const double& c) {
  double xmul = x(0);
  return c + (1 - c) / (1 + std::exp(-a * (xmul - b)));
}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::rowvec p_pc(const arma::rowvec& x, const arma::rowvec& b) {

  double xmul = x(0);
  int nk = b.n_cols + 1;
  rowvec z(nk);
  z(0) = xmul;

  for (int k = 1; k < nk; k++) {
    z(k) = xmul - b(k - 1);
  }

  rowvec zz = cumsum(z);
  rowvec pp = exp(zz);
  double psum = sum(pp);

  return pp / psum;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::rowvec p_gpc(const arma::rowvec& x, const double& a, const arma::rowvec& b) {

  double xmul = x(0);
  int nk = b.n_cols + 1;
  rowvec z(nk);
  z(0) = a * xmul;

  for (int k = 1; k < nk; k++) {
    z(k) = a * (xmul - b(k - 1));
  }

  rowvec zz = cumsum(z);
  rowvec pp = exp(zz);
  double psum = sum(pp);

  return pp / psum;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::rowvec p_gr(const arma::rowvec& x, const double& a, const arma::rowvec& b) {

  int nk = b.n_cols + 1;
  rowvec p(nk), p_star(nk + 1);
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
arma::colvec array_p_1pl(const arma::mat& x, const double& b) {

  int nx = x.n_rows;
  colvec p_array(nx);

  for (int j = 0; j < nx; j++) {
    p_array(j) = p_1pl(x.row(j), b);
  }

  return p_array;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::colvec array_p_2pl(const arma::mat& x, const double& a, const double& b) {

  int nx = x.n_rows;
  colvec p_array(nx);

  for (int j = 0; j < nx; j++) {
    p_array(j) = p_2pl(x.row(j), a, b);
  }

  return p_array;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::colvec array_p_3pl(const arma::mat& x, const double& a, const double& b, const double& c) {

  int nx = x.n_rows;
  colvec p_array(nx);

  for (int j = 0; j < nx; j++) {
    p_array(j) = p_3pl(x.row(j), a, b, c);
  }

  return p_array;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::mat array_p_pc(const arma::mat& x, const arma::rowvec& b) {

  int nx = x.n_rows;
  int nk = b.n_cols + 1;
  mat p_array(nx, nk);

  for (int j = 0; j < nx; j++) {
    p_array.row(j) = p_pc(x.row(j), b);
  }

  return p_array;
}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::mat array_p_gpc(const arma::mat& x, const double& a, const arma::rowvec& b) {

  int nx = x.n_rows;
  int nk = b.n_cols + 1;
  mat p_array(nx, nk);

  for (int j = 0; j < nx; j++) {
    p_array.row(j) = p_gpc(x.row(j), a, b);
  }

  return p_array;

}

//' @rdname p_item
//' @export
// [[Rcpp::export]]
arma::mat array_p_gr(const arma::mat& x, const double& a, const arma::rowvec& b) {

  int nx = x.n_rows;
  int nk = b.n_cols + 1;
  mat p_array(nx, nk);

  for (int j = 0; j < nx; j++) {
    p_array.row(j) = p_gr(x.row(j), a, b);
  }

  return p_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double info_1pl(const arma::rowvec& x, const double& b) {
  double p = p_1pl(x, b);
  return p * (1 - p);
}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double info_2pl(const arma::rowvec& x, const double& a, const double& b) {
  double p = p_2pl(x, a, b);
  return pow(a, 2) * p * (1 - p);
}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double info_3pl(const arma::rowvec& x, const double& a, const double& b, const double& c) {
  double p = p_3pl(x, a, b, c);
  return pow(a, 2) * (1 - p) / p * pow((p - c) / (1 - c), 2);
}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double info_pc(const arma::rowvec& x, const arma::rowvec& b) {

  rowvec p = p_pc(x, b);
  int nk = b.n_cols + 1;
  double const_1 = 0, const_2 = 0;

  for (int i = 0; i < nk; i++) {
    const_1 += i * p(i);
    const_2 += i * i * p(i);
  }

  return const_2 - pow(const_1, 2);

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double info_gpc(const arma::rowvec& x, const double& a, const arma::rowvec& b) {

  rowvec p = p_gpc(x, a, b);
  int nk = b.n_cols + 1;
  double const_1 = 0, const_2 = 0;

  for (int i = 0; i < nk; i++) {
    const_1 += i * p(i);
    const_2 += i * i * p(i);
  }

  return pow(a, 2) * (const_2 - pow(const_1, 2));

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double info_gr(const arma::rowvec& x, const double& a, const arma::rowvec& b) {

  int nk = b.n_cols + 1;
  rowvec p_star(nk + 1);
  p_star(0) = 1;
  p_star(nk) = 0;

  for (int k = 1; k < nk; k++) {
    p_star(k) = p_2pl(x, a, b(k - 1));
  }

  double out = 0;

  for (int k = 0; k < nk; k++) {
    out += (p_star(k) - p_star(k + 1)) * pow(1 - p_star(k) - p_star(k + 1), 2);
  }

  return out *= pow(a, 2);

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_info_1pl(const arma::mat& x, const double& b) {

  int nx = x.n_rows;
  colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = info_1pl(x.row(j), b);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_info_2pl(const arma::mat& x, const double& a, const double& b) {

  int nx = x.n_rows;
  colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = info_2pl(x.row(j), a, b);
  }

  return info_array;
}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_info_3pl(const arma::mat& x, const double& a, const double& b, const double& c) {

  int nx = x.n_rows;
  colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = info_3pl(x.row(j), a, b, c);
  }

  return info_array;

}


//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_info_pc(const arma::mat& x, const arma::rowvec& b) {

  int nx = x.n_rows;
  colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = info_pc(x.row(j), b);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_info_gpc(const arma::mat& x, const double& a, const arma::rowvec& b) {

  int nx = x.n_rows;
  colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = info_gpc(x.row(j), a, b);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_info_gr(const arma::mat& x, const double& a, const arma::rowvec& b) {

  int nx = x.n_rows;
  colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = info_gr(x.row(j), a, b);
  }

  return info_array;

}
