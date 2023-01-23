#include "item_probability.h"
#include "item_information.h"

//' @rdname a_to_alpha
//' @export
// [[Rcpp::export]]
arma::rowvec a_to_alpha(
  const arma::rowvec& a
) {

  int na = a.n_cols;
  arma::rowvec alpha_vec(na);
  arma::rowvec asq = pow(a, 2);

  double num = 0;
  double denom = sqrt(sum(asq));

  for (int i = 0; i < na; i++) {
    num = a(i);
    alpha_vec(i) = acos(num / denom);
  }

  return alpha_vec;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double info_1pl(
  const arma::rowvec& x,
  const double& b
) {
  double p = p_1pl(x, b);
  return p * (1 - p);
}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double info_2pl(
  const arma::rowvec& x,
  const double& a,
  const double& b
) {
  double p = p_2pl(x, a, b);
  return pow(a, 2) * p * (1 - p);
}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::mat info_m_2pl(
  const arma::rowvec& x,
  const arma::rowvec& a,
  const double& d
) {
  double p = p_m_2pl(x, a, d);
  return (a.t() * a) * p * (1 - p);
}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double dirinfo_m_2pl(
  const arma::rowvec& x,
  const arma::rowvec& a,
  const double& d
) {
  arma::rowvec alpha_vec = a_to_alpha(a);
  return thisdirinfo_m_2pl(x, alpha_vec, a, d);
}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double thisdirinfo_m_2pl(
  const arma::rowvec& x,
  const arma::rowvec& alpha_vec,
  const arma::rowvec& a,
  const double& d
) {
  double multiplier = pow(sum(a % cos(alpha_vec)), 2);
  double p = p_m_2pl(x, a, d);
  return multiplier * p * (1 - p);
}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double info_3pl(
  const arma::rowvec& x,
  const double& a,
  const double& b,
  const double& c
) {
  double p = p_3pl(x, a, b, c);
  return pow(a, 2) * (1 - p) / p * pow((p - c) / (1 - c), 2);
}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::mat info_m_3pl(
  const arma::rowvec& x,
  const arma::rowvec& a,
  const double& d,
  const double& c
) {

  double xmul = arma::as_scalar(a * x.t() + d);
  double p = p_m_3pl(x, a, d, c);

  return (a.t() * a) *
    (1 - p) /
    (p * pow(1 + std::exp(-xmul), 2));

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double dirinfo_m_3pl(
  const arma::rowvec& x,
  const arma::rowvec& a,
  const double& d,
  const double& c
) {

  arma::rowvec alpha_vec = a_to_alpha(a);
  return thisdirinfo_m_3pl(x, alpha_vec, a, d, c);

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double thisdirinfo_m_3pl(
  const arma::rowvec& x,
  const arma::rowvec& alpha_vec,
  const arma::rowvec& a,
  const double& d,
  const double& c
) {

  // Bryant (2005) 10.1007/s11336-003-1129-6
  double xmul = arma::as_scalar(a * x.t() + d);

  double multiplier = pow(sum(a % cos(alpha_vec)), 2);
  double p = p_m_3pl(x, a, d, c);

  return multiplier *
    (1 - p) /
    (p * pow(1 + std::exp(-xmul), 2));

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double info_pc(
  const arma::rowvec& x,
  const arma::rowvec& b
) {

  arma::rowvec p = p_pc(x, b);
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
double info_gpc(
  const arma::rowvec& x,
  const double& a,
  const arma::rowvec& b
) {

  arma::rowvec p = p_gpc(x, a, b);
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
arma::mat info_m_gpc(
  const arma::rowvec& x,
  const arma::rowvec& a,
  const arma::rowvec& d
) {

  arma::rowvec p = p_m_gpc(x, a, d);
  int nk = d.n_cols + 1;
  double const_1 = 0, const_2 = 0;

  for (int i = 0; i < nk; i++) {
    const_1 += i * p(i);
    const_2 += i * i * p(i);
  }

  return (a.t() * a) * (const_2 - pow(const_1, 2));

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double dirinfo_m_gpc(
  const arma::rowvec& x,
  const arma::rowvec& a,
  const arma::rowvec& d
) {

  arma::rowvec alpha_vec = a_to_alpha(a);
  return thisdirinfo_m_gpc(x, alpha_vec, a, d);

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double thisdirinfo_m_gpc(
  const arma::rowvec& x,
  const arma::rowvec& alpha_vec,
  const arma::rowvec& a,
  const arma::rowvec& d
) {

  double multiplier = pow(sum(a % cos(alpha_vec)), 2);
  arma::rowvec p = p_m_gpc(x, a, d);
  int nk = d.n_cols + 1;
  double const_1 = 0, const_2 = 0;

  for (int i = 0; i < nk; i++) {
    const_1 += i * p(i);
    const_2 += i * i * p(i);
  }

  return multiplier * (const_2 - pow(const_1, 2));

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double info_gr(
  const arma::rowvec& x,
  const double& a,
  const arma::rowvec& b
) {

  int nk = b.n_cols + 1;
  arma::rowvec p_star(nk + 1);
  p_star(0) = 1;
  p_star(nk) = 0;

  for (int k = 1; k < nk; k++) {
    p_star(k) = p_2pl(x, a, b(k - 1));
  }

  double o = 0;

  for (int k = 0; k < nk; k++) {
    o += (p_star(k) - p_star(k + 1)) * pow(1 - p_star(k) - p_star(k + 1), 2);
  }

  return o *= pow(a, 2);

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::mat info_m_gr(
  const arma::rowvec& x,
  const arma::rowvec& a,
  const arma::rowvec& d
) {

  int nk = d.n_cols + 1;
  arma::rowvec p_star(nk + 1);
  p_star(0) = 1;
  p_star(nk) = 0;

  for (int k = 1; k < nk; k++) {
    p_star(k) = p_m_2pl(x, a, d(k - 1));
  }

  double o = 0;

  for (int k = 0; k < nk; k++) {
    o += (p_star(k) - p_star(k + 1)) * pow(1 - p_star(k) - p_star(k + 1), 2);
  }

  return (a.t() * a) * o;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double dirinfo_m_gr(
  const arma::rowvec& x,
  const arma::rowvec& a,
  const arma::rowvec& d
) {

  arma::rowvec alpha_vec = a_to_alpha(a);
  return thisdirinfo_m_gr(x, alpha_vec, a, d);

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
double thisdirinfo_m_gr(
  const arma::rowvec& x,
  const arma::rowvec& alpha_vec,
  const arma::rowvec& a,
  const arma::rowvec& d
) {

  int nk = d.n_cols + 1;
  arma::rowvec p_star(nk + 1);
  p_star(0) = 1;
  p_star(nk) = 0;

  for (int k = 1; k < nk; k++) {
    p_star(k) = p_m_2pl(x, a, d(k - 1));
  }

  double o = 0;

  for (int k = 0; k < nk; k++) {
    o += (p_star(k) - p_star(k + 1)) * pow(1 - p_star(k) - p_star(k + 1), 2);
  }

  double multiplier = pow(sum(a % cos(alpha_vec)), 2);

  return o *= multiplier;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_info_1pl(
  const arma::mat& x,
  const double& b
) {

  int nx = x.n_rows;
  arma::colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = info_1pl(x.row(j), b);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_info_2pl(
  const arma::mat& x,
  const double& a,
  const double& b
) {

  int nx = x.n_rows;
  arma::colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = info_2pl(x.row(j), a, b);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
List array_info_m_2pl(
  const arma::mat& x,
  const arma::rowvec& a,
  const double& d
) {

  int nx = x.n_rows;
  List info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = info_m_2pl(x.row(j), a, d);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_dirinfo_m_2pl(
  const arma::mat& x,
  const arma::rowvec& a,
  const double& d
) {

  int nx = x.n_rows;
  arma::colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = dirinfo_m_2pl(x.row(j), a, d);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_thisdirinfo_m_2pl(
  const arma::mat& x,
  const arma::rowvec& alpha_vec,
  const arma::rowvec& a,
  const double& d
) {

  int nx = x.n_rows;
  arma::colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = thisdirinfo_m_2pl(x.row(j), alpha_vec, a, d);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_info_3pl(
  const arma::mat& x,
  const double& a,
  const double& b,
  const double& c
) {

  int nx = x.n_rows;
  arma::colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = info_3pl(x.row(j), a, b, c);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
List array_info_m_3pl(
  const arma::mat& x,
  const arma::rowvec& a,
  const double& d,
  const double& c
) {

  int nx = x.n_rows;
  List info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = info_m_3pl(x.row(j), a, d, c);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_dirinfo_m_3pl(
  const arma::mat& x,
  const arma::rowvec& a,
  const double& d,
  const double& c
) {

  int nx = x.n_rows;
  arma::colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = dirinfo_m_3pl(x.row(j), a, d, c);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_thisdirinfo_m_3pl(
  const arma::mat& x,
  const arma::rowvec& alpha_vec,
  const arma::rowvec& a,
  const double& d,
  const double& c
) {

  int nx = x.n_rows;
  arma::colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = thisdirinfo_m_3pl(x.row(j), alpha_vec, a, d, c);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_info_pc(
  const arma::mat& x,
  const arma::rowvec& b
) {

  int nx = x.n_rows;
  arma::colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = info_pc(x.row(j), b);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_info_gpc(
  const arma::mat& x,
  const double& a,
  const arma::rowvec& b
) {

  int nx = x.n_rows;
  arma::colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = info_gpc(x.row(j), a, b);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
List array_info_m_gpc(
  const arma::mat& x,
  const arma::rowvec& a,
  const arma::rowvec& d
) {

  int nx = x.n_rows;
  List info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = info_m_gpc(x.row(j), a, d);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_dirinfo_m_gpc(
  const arma::mat& x,
  const arma::rowvec& a,
  const arma::rowvec& d
) {

  int nx = x.n_rows;
  arma::colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = dirinfo_m_gpc(x.row(j), a, d);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_thisdirinfo_m_gpc(
  const arma::mat& x,
  const arma::rowvec& alpha_vec,
  const arma::rowvec& a,
  const arma::rowvec& d
) {

  int nx = x.n_rows;
  arma::colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = thisdirinfo_m_gpc(x.row(j), alpha_vec, a, d);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_info_gr(
  const arma::mat& x,
  const double& a,
  const arma::rowvec& b
) {

  int nx = x.n_rows;
  arma::colvec info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = info_gr(x.row(j), a, b);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
List array_info_m_gr(
  const arma::mat& x,
  const arma::rowvec& a,
  const arma::rowvec& d
) {

  int nx = x.n_rows;
  List info_array(nx);

  for (int j = 0; j < nx; j++) {
    info_array(j) = info_m_gr(x.row(j), a, d);
  }

  return info_array;

}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_dirinfo_m_gr(
  const arma::mat& x,
  const arma::rowvec& a,
  const arma::rowvec& d
) {
  int nx = x.n_rows;
  colvec info_array(nx);
  for (int j = 0; j < nx; j++) {
    info_array(j) = dirinfo_m_gr(x.row(j), a, d);
  }
  return info_array;
}

//' @rdname info_item
//' @export
// [[Rcpp::export]]
arma::colvec array_thisdirinfo_m_gr(
  const arma::mat& x,
  const arma::rowvec& alpha_vec,
  const arma::rowvec& a,
  const arma::rowvec& d
) {
  int nx = x.n_rows;
  colvec info_array(nx);
  for (int j = 0; j < nx; j++) {
    info_array(j) = thisdirinfo_m_gr(x.row(j), alpha_vec, a, d);
  }
  return info_array;
}
