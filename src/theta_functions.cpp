#include "posterior_functions.h"
#include "theta_functions.h"

//' @rdname theta_EAP
//' @export
// [[Rcpp::export]]
List theta_EAP(
  const arma::mat& theta_grid,
  const arma::mat& item_parm,
  const arma::irowvec& resp,
  const arma::irowvec& ncat,
  const arma::irowvec& model,
  const int& prior,
  const arma::rowvec& prior_parm) {

  // this function is not being called in other functions
  // kept for archive and as a helper function

  int nq = theta_grid.n_rows;

  List o;
  colvec const_term(3, fill::zeros);

  for (int q = 0; q < nq; q++) {
    rowvec x = theta_grid.row(q);
    double pos = calc_posterior(x, item_parm, resp, ncat, model, prior, prior_parm);
    const_term(0) += pos;
    const_term(1) += x(0) * pos;        // unidimensional
    const_term(2) += x(0) * x(0) * pos; // unidimensional
  }

  double theta = const_term(1) / const_term(0);
  o["theta"] = theta;
  o["se"] = sqrt(const_term(2) / const_term(0) - theta * theta);

  return o;

}

//' @rdname theta_EAP
//' @export
// [[Rcpp::export]]
List theta_EAP_matrix(
  const arma::mat& theta_grid,
  const arma::mat& item_parm,
  const arma::imat& resp,
  const arma::irowvec& ncat,
  const arma::irowvec& model,
  const int& prior,
  const arma::rowvec& prior_parm) {

  // this function is not being called in other functions
  // kept for archive and as a helper function

  int nq = theta_grid.n_rows;
  int nj = resp.n_rows;
  List o(nj);

  for (int j = 0; j < nj; j++) {
    rowvec const_term(3);
    irowvec resp_single = resp.row(j);
    for (int q = 0; q < nq; q++) {
      rowvec x = theta_grid.row(q);
      double pos = calc_posterior(x, item_parm, resp_single, ncat, model, prior, prior_parm);
      const_term(0) += pos;
      const_term(1) += x(0) * pos;        // unidimensional
      const_term(2) += x(0) * x(0) * pos; // unidimensional
    }

    List x;
    double theta = const_term(1) / const_term(0);
    x["theta"] = theta;
    x["se"] = sqrt(const_term(2) / const_term(0) - theta * theta);
    o(j) = x;

  }

  return o;

}

//' @rdname theta_EB
//' @export
// [[Rcpp::export]]
arma::mat theta_EB(
  const int& nx,
  const arma::rowvec& theta_init,
  const double& theta_prop,
  const arma::mat& item_parm,
  const arma::irowvec& resp,
  const arma::irowvec& ncat,
  const arma::irowvec& model,
  const int& prior,
  const arma::rowvec& prior_parm) {

  mat out(nx, 1);
  rowvec x_0 = theta_init;
  double density_0 = calc_posterior(x_0, item_parm, resp, ncat, model, prior, prior_parm);

  for (int j = 0; j < nx; j++) {
    GetRNGstate();
    rowvec x_rnorm = Rcpp::rnorm(1);
    PutRNGstate();

    rowvec x_1 = x_rnorm * theta_prop + x_0;
    double density_1 = calc_posterior(x_1, item_parm, resp, ncat, model, prior, prior_parm);
    double prob_alpha = density_1 / density_0;

    GetRNGstate();
    rowvec p_accept = Rcpp::runif(1);
    PutRNGstate();
    if (p_accept(0) < prob_alpha) {
      x_0 = x_1;
      density_0 = density_1;
    }
    out.row(j) = x_0;
  }

  return out;
}

//' @rdname theta_EB
//' @export
// [[Rcpp::export]]
arma::mat theta_EB_single(
  const int& nx,
  const arma::rowvec& theta_init,
  const double& theta_prop,
  const arma::rowvec& item_parm,
  const int& resp,
  const int& ncat,
  const int& model,
  const int& prior,
  const arma::rowvec& prior_parm) {

  mat out(nx, 1);
  rowvec x_0 = theta_init;
  double density_0 = calc_posterior_single(x_0, item_parm, resp, ncat, model, prior, prior_parm);

  for (int j = 0; j < nx; j++) {

    GetRNGstate();
    rowvec x_rnorm = Rcpp::rnorm(1);
    PutRNGstate();

    rowvec x_1 = x_rnorm * theta_prop + x_0;
    double density_1 = calc_posterior_single(x_1, item_parm, resp, ncat, model, prior, prior_parm);
    double prob_alpha = density_1 / density_0;

    GetRNGstate();
    rowvec p_accept = Rcpp::runif(1);
    PutRNGstate();
    if (p_accept(0) < prob_alpha) {
      x_0 = x_1;
      density_0 = density_1;
    }
    out.row(j) = x_0;

  }

  return out;
}

//' @rdname theta_FB
//' @export
// [[Rcpp::export]]
arma::mat theta_FB(
  const int& nx,
  const arma::rowvec& theta_init,
  const double& theta_prop,
  const List& items_list,
  const arma::mat& item_init,
  const arma::irowvec& resp,
  const arma::irowvec& ncat,
  const arma::irowvec& model,
  const int& prior,
  const arma::rowvec& prior_parm) {

  mat out(nx, 1);

  rowvec x_0 = theta_init;
  int ni = item_init.n_rows;
  int nparm = item_init.n_cols;
  mat item_parm = item_init;

  int nk = 0;
  double const_0 = calc_log_likelihood(x_0, item_parm, resp, ncat, model, prior, prior_parm);

  GetRNGstate();

  for (int j = 0; j < nx; j++) {

    GetRNGstate();
    rowvec x_rnorm = Rcpp::rnorm(1);
    PutRNGstate();

    rowvec x_1 = x_rnorm * theta_prop + x_0;

    mat item_parm(ni, nparm);

    for (int m = 0; m < ni; m++) {

      mat item1 = as<mat>(items_list[m]);
      int n3 = item1.n_rows;

      GetRNGstate();
      NumericVector id_runif = Rcpp::runif(1);
      PutRNGstate();

      int ids = (int) floor(id_runif(0)*n3);

      switch (model(m)) {
        case 1:
          nk = model(m);
          break;
        case 2:
          nk = model(m);
          break;
        case 3:
          nk = model(m);
          break;
        case 4:
          nk = ncat(m) - 1;
          break;
        case 5:
          nk = ncat(m);
          break;
        case 6:
          nk = ncat(m);
          break;
      }

      for (int j = 0; j < nk; j++) {
        item_parm(m, j) = item1(ids, j);
      }

    }

    double const_1 = calc_log_likelihood(x_1, item_parm, resp,ncat,model,prior,prior_parm);
    double prob_alpha = exp(const_1 - const_0);

    GetRNGstate();
    rowvec p1 = Rcpp::runif(1);
    PutRNGstate();

    if (p1(0) <= prob_alpha) {
      x_0 = x_1;
      const_0 = const_1;
    }

    out.row(j) = x_0;

  }

  PutRNGstate();

  return out;
}

//' @rdname theta_FB
//' @export
// [[Rcpp::export]]
arma::mat theta_FB_single(
  const int& nx,
  const arma::rowvec& theta_init,
  const double& theta_prop,
  const arma::mat& item_mcmc,
  const arma::rowvec& item_init,
  const int& resp,
  const int& ncat,
  const int& model,
  const int& prior,
  const arma::rowvec& prior_parm) {

  mat out(nx, 1);
  rowvec x_0 = theta_init;
  int ns = item_mcmc.n_rows;

  rowvec item_parm = item_init;
  double const_0 = calc_posterior_single(x_0, item_parm, resp, ncat, model, prior, prior_parm);

  for (int j = 0; j < nx; j++) {

    GetRNGstate();
    rowvec x_rnorm = Rcpp::rnorm(1);
    PutRNGstate();

    rowvec x_1 = x_rnorm * theta_prop + x_0;

    GetRNGstate();
    rowvec id_runif = Rcpp::runif(1);
    PutRNGstate();

    int id = (int) floor(id_runif(0) * ns);
    item_parm = item_mcmc.row(id);

    double const_1 = calc_posterior_single(x_1, item_parm, resp, ncat, model, prior, prior_parm);
    double prob_alpha = const_1 / const_0;

    GetRNGstate();
    rowvec p_accept = Rcpp::runif(1);
    PutRNGstate();
    if (p_accept[0] < prob_alpha) {
      x_0 = x_1;
      const_0 = const_1;
    }
    out.row(j) = x_0;
  }

  return out;

}
