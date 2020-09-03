#include "item_functions.h"
#include "posterior_functions.h"

//' @rdname calc_likelihood
//' @export
// [[Rcpp::export]]
double calc_likelihood (
  const arma::rowvec& x,
  const arma::mat& item_parm,
  const arma::irowvec& resp,
  const arma::irowvec& ncat,
  const arma::irowvec& model) {

  int ni = resp.n_cols;
  double lh = 1;

  for (int i = 0; i < ni; i++) {
    double p;
    switch (model(i)) {
      case 1: {
        double b = item_parm(i, 0);
        p = p_1pl(x, b);
        if (resp(i) == 0) { p = (1 - p); }
        lh *= p;
      }
      break;
      case 2: {
        double a = item_parm(i, 0);
        double b = item_parm(i, 1);
        p = p_2pl(x, a, b);
        if (resp(i) == 0) { p = (1 - p); }
        lh *= p;
      }
      break;
      case 3: {
        double a = item_parm(i, 0);
        double b = item_parm(i, 1);
        double c = item_parm(i, 2);
        p = p_3pl(x, a, b, c);
        if (resp(i) == 0) { p = (1 - p); }
        lh *= p;
      }
      break;
      case 4: {
        rowvec b = item_parm(i, span(0, ncat(i) - 2));
        rowvec pp(ncat(i));
        pp = p_pc(x, b);
        lh *= pp(resp(i));

      }
      break;
      case 5: {
        double a = item_parm(i, 0);
        rowvec b = item_parm(i, span(1, ncat(i) - 1));
        rowvec pp(ncat(i));
        pp = p_gpc(x, a, b);
        lh *= pp(resp(i));
      }
      break;
      case 6: {
        double a = item_parm(i, 0);
        rowvec b = item_parm(i, span(1, ncat(i) - 1));
        rowvec pp(ncat(i));
        pp = p_gr(x, a, b);
        lh *= pp(resp(i));
      }
      break;
    }
  }

  return lh;

}

//' @rdname calc_likelihood
//' @export
// [[Rcpp::export]]
arma::colvec calc_likelihood_function(
  const arma::mat& theta_grid,
  const arma::mat& item_parm,
  const arma::irowvec& resp,
  const arma::irowvec& ncat,
  const arma::irowvec& model) {

  int ni = resp.n_cols;
  int nq = theta_grid.n_rows;
  mat pp(nq, max(ncat));
  colvec lh(nq);
  lh.fill(1);

  for (int i = 0; i < ni; i++) {
    switch (model(i)) {
      case 1: {
        double b = item_parm(i, 0);
        pp.col(1) = array_p_1pl(theta_grid, b);
        pp.col(0) = 1 - pp.col(1);

      }
      break;
      case 2: {
        double a = item_parm(i, 0);
        double b = item_parm(i, 1);
        pp.col(1) = array_p_2pl(theta_grid, a, b);
        pp.col(0) = 1 - pp.col(1);
      }
      break;
      case 3: {
        double a = item_parm(i, 0);
        double b = item_parm(i, 1);
        double c = item_parm(i, 2);
        pp.col(1) = array_p_3pl(theta_grid, a, b, c);
        pp.col(0) = 1 - pp.col(1);
      }
      break;
      case 4: {
        rowvec b = item_parm(i, span(0, ncat(i) - 2));
        pp = array_p_pc(theta_grid,b);
      }
      break;
      case 5: {
        double a = item_parm(i, 0);
        rowvec b = item_parm(i, span(1, ncat(i) - 1));
        pp = array_p_gpc(theta_grid, a, b);
      }
      break;
      case 6: {
        double a = item_parm(i, 0);
        rowvec b = item_parm(i, span(1, ncat(i) - 1));
        pp = array_p_gr(theta_grid, a, b);
      }
      break;
    }

    for (int q = 0; q < nq; q++) {
      lh(q) *= pp(q, resp(i));
    }

  }
  return lh;
}

//' @rdname calc_likelihood
//' @export
// [[Rcpp::export]]
double calc_log_likelihood(
  const arma::rowvec& x,
  const arma::mat& item_parm,
  const arma::irowvec& resp,
  const arma::irowvec& ncat,
  const arma::irowvec& model,
  const int& prior,
  const arma::rowvec& prior_parm) {

  int ni = resp.n_cols;
  double llh = 0;

  for (int i = 0; i < ni; i++) {
    double p;
    switch (model(i)) {
      case 1: {
        double b = item_parm(i, 0);
        p = p_1pl(x, b);
        if (resp(i) == 0) { p = (1 - p); }
        llh += log(p);
      }
      break;
      case 2: {
        double a = item_parm(i, 0);
        double b = item_parm(i, 1);
        p = p_2pl(x, a, b);
        if (resp(i) == 0) { p = (1 - p); }
        llh += log(p);
      }
      break;
      case 3: {
        double a = item_parm(i, 0);
        double b = item_parm(i, 1);
        double c = item_parm(i, 2);
        p = p_3pl(x, a, b, c);
        if (resp(i) == 0) { p = (1 - p); }
        llh += log(p);
      }
      break;
      case 4: {
        rowvec b = item_parm(i, span(0, ncat(i) - 2));
        rowvec pp(ncat(i));
        pp = p_pc(x, b);
        llh += log(pp(resp(i)));
      }
      break;
      case 5: {
        double a = item_parm(i, 0);
        rowvec b = item_parm(i, span(1, ncat(i) - 1));
        rowvec pp(ncat(i));
        pp = p_gpc(x, a, b);
        llh += log(pp(resp(i)));
      }
      break;
      case 6: {
        double a = item_parm(i, 0);
        rowvec b = item_parm(i, span(1, ncat(i) - 1));
        rowvec pp(ncat(i));
        pp = p_gr(x, a, b);
        llh += log(pp(resp(i)));
      }
      break;
    }
  }

  if (prior == 1) { //normal
    llh += -0.5 * pow( (x(0) - prior_parm(0)) / prior_parm(1), 2) - log(prior_parm(1));
  }

  return llh;
}

//' @rdname calc_likelihood
//' @export
// [[Rcpp::export]]
arma::colvec calc_log_likelihood_function(
  const arma::mat& theta_grid,
  const arma::mat& item_parm,
  const arma::irowvec& resp,
  const arma::irowvec& ncat,
  const arma::irowvec& model,
  const int& prior,
  const arma::rowvec& prior_parm) {

  int ni = resp.n_cols;
  int nq = theta_grid.n_rows;
  mat pp(nq, max(ncat));
  colvec llh(nq, fill::zeros);

  for (int i = 0; i < ni; i++) {

    switch (model(i)) {
      case 1: {
        double b = item_parm(i, 0);
        pp.col(1) = array_p_1pl(theta_grid, b);
        pp.col(0) = 1 - pp.col(1);
      }
      break;
      case 2: {
        double a = item_parm(i, 0);
        double b = item_parm(i, 1);
        pp.col(1) = array_p_2pl(theta_grid, a, b);
        pp.col(0) = 1 - pp.col(1);
      }
      break;
      case 3: {
        double a = item_parm(i, 0);
        double b = item_parm(i, 1);
        double c = item_parm(i, 2);
        pp.col(1) = array_p_3pl(theta_grid, a, b, c);
        pp.col(0) = 1 - pp.col(1);
      }
      break;
      case 4: {
        rowvec b = item_parm(i, span(0, ncat(i) - 2));
        pp = array_p_pc(theta_grid, b);
      }
      break;
      case 5: {
        double a = item_parm(i, 0);
        rowvec b = item_parm(i, span(1, ncat(i) - 1));
        pp = array_p_gpc(theta_grid, a, b);
      }
      break;
      case 6: {
        double a = item_parm(i, 0);
        rowvec b = item_parm(i, span(1, ncat(i) - 1));
        pp = array_p_gr(theta_grid, a, b);
      }
      break;
    }

    for (int q = 0; q < nq; q++) {
      llh(q) += log(pp(q, resp(i)));
    }

  }

  if (prior == 1) {
    for (int q = 0; q < nq; q++) {
      llh(q) += -0.5 * pow((theta_grid(q) - prior_parm(0)) / prior_parm(1), 2) - log(prior_parm(1));
    }
  }

  return llh;
}

//' @noRd
double calc_prior_multiplier(
  const double& x,
  const int& prior,
  const arma::rowvec& prior_parm) {

  double mul = 0;
  switch (prior) {
    case 1: // normal
      mul = exp(-0.5 * pow((x - prior_parm(0)) / prior_parm(1), 2)) / prior_parm(1) * 0.3989423; // 1/sqrt(2*MI_PI) = 0.3989423
      break;
    case 2: // uniform
      mul = 1 / (prior_parm(1) - prior_parm(0));
      break;
  }
  return mul;
}

//' Calculate a posterior value of theta
//'
//' Calculate a posterior value of theta.
//'
//' @param x A length-one numeric vector for a theta value.
//' @param item_parm A numeric matrix of item parameters.
//' @template calc-params
// [[Rcpp::export]]
double calc_posterior(
  const arma::rowvec& x,
  const arma::mat& item_parm,
  const arma::irowvec& resp,
  const arma::irowvec& ncat,
  const arma::irowvec& model,
  const int& prior,
  const arma::rowvec& prior_parm) {

  double pos = calc_likelihood(x, item_parm, resp, ncat, model);
  pos *= calc_prior_multiplier(x(0), prior, prior_parm); // unidimensional

  return pos;

}

//' Calculate a posterior distribution of theta
//'
//' Calculate a posterior distribution of theta.
//'
//' @param theta_grid An equi-spaced grid of theta values.
//' @param item_parm A numeric matrix of item parameters.
//' @template calc-params
// [[Rcpp::export]]
arma::colvec calc_posterior_function(
  const arma::mat& theta_grid,
  const arma::mat& item_parm,
  const arma::irowvec& resp,
  const arma::irowvec& ncat,
  const arma::irowvec& model,
  const int& prior,
  const arma::rowvec& prior_parm) {

  int nq = theta_grid.n_rows;
  colvec pos(nq);

  pos = calc_likelihood_function(theta_grid, item_parm, resp, ncat, model);
  for (int q = 0; q < nq; q++) {
    pos *= calc_prior_multiplier(theta_grid(q, 0), prior, prior_parm); // unidimensional
  }

  return pos;

}

//' Calculate a posterior value of theta for a single item
//'
//' Calculate a posterior value of theta for a single item.
//'
//' @param x A length-one numeric vector for a theta value.
//' @param item_parm A numeric vector of item parameters (for one item).
//' @param resp A length-one numeric vector of item responses.
//' @param ncat A length-one numeric vector of the number of response categories by item.
//' @param model A length-one numeric vector of the IRT model by item (1: 1PL, 2: 2PL, 3: 3PL, 4: PC, 5: GPC, 6: GR).
//' @param prior The type of prior distribution (1: normal, 2: uniform).
//' @param prior_parm A numeric vector of hyperparameters for the prior distribution, c(mu, sigma) or c(ll, ul).
//'
// [[Rcpp::export]]
double calc_posterior_single(
  const arma::rowvec& x,
  const arma::rowvec& item_parm,
  const int& resp,
  const int& ncat,
  const int& model,
  const int& prior,
  const arma::rowvec& prior_parm) {
  double pos = 0;

  switch (model) {
    case 1: {
      double b = item_parm(0);
      double p = p_1pl(x, b);
      if (resp == 0) { p = (1 - p); }
      pos = p;
    }
    break;
    case 2: {
      double a = item_parm(0);
      double b = item_parm(1);
      double p = p_2pl(x, a, b);
      if (resp == 0) { p = (1 - p); }
      pos = p;
    }
    break;
    case 3: {
      double a = item_parm(0);
      double b = item_parm(1);
      double c = item_parm(2);
      double p = p_3pl(x, a, b, c);
      if (resp == 0) { p = (1 - p); }
      pos = p;
    }
    break;
    case 4: {
      rowvec b = item_parm(span(0, ncat - 2));
      rowvec p = p_pc(x, b);
      pos = p(resp);
    }
    break;
    case 5: {
      double a = item_parm(0);
      rowvec b = item_parm(span(1, ncat - 1));
      rowvec p = p_gpc(x, a, b);
      pos = p(resp);
    }
    break;
    case 6: {
      double a = item_parm(0);
      rowvec b = item_parm(span(1, ncat - 1));
      rowvec p = p_gr(x, a, b);
      pos = p(resp);
    }
    break;
  }

  pos *= calc_prior_multiplier(x(0), prior, prior_parm); // unidimensional

  return pos;
}
