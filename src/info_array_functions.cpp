#include "item_functions.h"
#include "info_array_functions.h"

//' @rdname calc_info
//' @export
// [[Rcpp::export]]
arma::colvec calc_info(
  const arma::rowvec& x,
  const arma::mat& item_parm,
  const arma::icolvec& ncat,
  const arma::icolvec& model) {

  int ni = item_parm.n_rows;
  colvec info_array(ni);

  for (int i = 0; i < ni; i++) {
    switch (model(i)) {
      case 1: {
        double b = item_parm(i, 0);
        info_array(i) = info_1pl(x, b);
      }
      break;
      case 2: {
        double a = item_parm(i, 0);
        double b = item_parm(i, 1);
        info_array(i) = info_2pl(x, a, b);
      }
      break;
      case 3: {
        double a = item_parm(i, 0);
        double b = item_parm(i, 1);
        double c = item_parm(i, 2);
        info_array(i) = info_3pl(x, a, b, c);
      }
      break;
      case 4: {
        rowvec b = item_parm(i, span(0, ncat(i) - 2));
        info_array(i) = info_pc(x, b);
      }
      break;
      case 5: {
        double a = item_parm(i, 0);
        rowvec b = item_parm(i, span(1, ncat(i) - 1));
        info_array(i) = info_gpc(x, a, b);
      }
      break;
      case 6: {
        double a = item_parm(i, 0);
        rowvec b = item_parm(i, span(1, ncat(i) - 1));
        info_array(i) = info_gr(x, a, b);
      }
      break;
    }
  }

  return info_array;

}

//' @rdname calc_info
//' @export
// [[Rcpp::export]]
arma::mat calc_info_matrix(
  const arma::mat& x,
  const arma::mat& item_parm,
  const arma::icolvec& ncat,
  const arma::icolvec& model) {

  int nx = x.n_rows;
  int ni = item_parm.n_rows;
  arma::mat info_matrix(nx, ni);

  for (int i = 0; i < ni; i++) {
    switch (model(i)) {
      case 1: {
        double b = item_parm(i, 0);
        for (int q = 0; q < nx; q++) {
          info_matrix(q, i) = info_1pl(x.row(q), b);
        }
      }
      break;
      case 2: {
        double a = item_parm(i, 0);
        double b = item_parm(i, 1);
        for (int q = 0; q < nx; q++) {
          info_matrix(q, i) = info_2pl(x.row(q), a, b);
        }
      }
      break;
      case 3: {
        double a = item_parm(i, 0);
        double b = item_parm(i, 1);
        double c = item_parm(i, 2);
        for (int q = 0; q < nx; q++) {
          info_matrix(q, i) = info_3pl(x.row(q), a, b, c);
        }
      }
      break;
      case 4: {
        rowvec b = item_parm(i, span(0, ncat(i) - 2));
        for (int q = 0; q < nx; q++) {
          info_matrix(q, i) = info_pc(x.row(q), b);
        }
      }
      break;
      case 5: {
        double a = item_parm(i, 0);
        rowvec b = item_parm(i, span(1, ncat(i) - 1));
        for (int q = 0; q < nx; q++) {
          info_matrix(q, i) = info_gpc(x.row(q), a, b);
        }
      }
      break;
      case 6: {
        double a = item_parm(i, 0);
        rowvec b = item_parm(i, span(1, ncat(i) - 1));
        for (int q = 0; q < nx; q++) {
          info_matrix(q, i) = info_gr(x.row(q), a, b);
        }
      }
      break;
    }
  }

  return info_matrix;

}

//' Calculate the Fisher information using empirical Bayes
//'
//' Calculate the Fisher information using empirical Bayes.
//'
//' @param x A numeric vector of MCMC sampled theta values.
//' @param item_parm A numeric matrix of item parameters.
//' @template calc-params-mini
// [[Rcpp::export]]
arma::colvec calc_info_EB (
  const arma::mat& x,
  const arma::mat& item_parm,
  const arma::icolvec& ncat,
  const arma::icolvec& model) {

  int nx = x.n_rows;
  int ni = item_parm.n_rows;
  colvec info_array(ni, fill::zeros);
  colvec info(ni, fill::zeros);

  for (int j = 0; j < nx; j++) {
    info.fill(0);
    info = calc_info(x.row(j), item_parm, ncat, model);
    info_array = info_array + info;
  }

  return info_array;

}

//' Calculate the Fisher information using full Bayesian
//'
//' Calculate the Fisher information using full Bayesian.
//'
//' @param x A numeric vector of MCMC sampled theta values.
//' @param items_list A list of item parameter matrices.
//' @template calc-params-mini
//' @param useEAP \code{TRUE} to use the mean of MCMC theta draws.
// [[Rcpp::export]]
arma::colvec calc_info_FB (
  const arma::mat& x,
  const List& items_list,
  const arma::icolvec& ncat,
  const arma::icolvec& model,
  const bool& useEAP = false) {

  int nx = x.n_rows;
  int ni = ncat.n_rows;
  colvec info_array(ni, fill::zeros);

  mat xx = x;
  if (useEAP) {
    double xm = mean(mean(x));
    xx.fill(xm);
  }

  for (int i = 0; i < ni; i++) {

    mat item_parm = as<mat>(items_list[i]);
    int ns = item_parm.n_rows;
    int s = 0;
    double info_sum = 0;

    switch (model(i)) {
      case 1: {
        for (int j = 0; j < nx; j++) {
          double b = item_parm(s, 0);
          info_sum += info_1pl(xx.row(j), b);
          s += 1;
          if (s >= ns) { s = 0; }
        }
      }
      break;
      case 2: {
        for (int j = 0; j < nx; j++) {
          double a = item_parm(s, 0);
          double b = item_parm(s, 1);
          info_sum += info_2pl(xx.row(j), a, b);
          s += 1;
          if (s >= ns) { s = 0; }
        }
      }
      break;
      case 3: {
        for (int j = 0; j < nx; j++) {
          double a = item_parm(s, 0);
          double b = item_parm(s, 1);
          double c = item_parm(s, 2);
          info_sum += info_3pl(xx.row(j), a, b, c);
          s += 1;
          if (s >= ns) { s = 0; }
        }
      }
      break;
      case 4: {
        for (int j = 0; j < nx; j++) {
          rowvec b = item_parm(s, span(0, ncat(i) - 2));
          info_sum += info_pc(xx.row(j), b);
          s += 1;
          if (s >= ns) { s = 0; }
        }
      }
      break;
      case 5: {
        for (int j = 0; j < nx; j++) {
          double a = item_parm(s, 0);
          rowvec b = item_parm(s, span(1, ncat(i) - 1));
          info_sum += info_gpc(xx.row(j), a, b);
          s += 1;
          if (s >= ns) { s = 0; }
        }
      }
      break;
      case 6: {
        for (int j = 0; j < nx; j++) {
          double a = item_parm(s, 0);
          rowvec b = item_parm(s, span(1, ncat(i) - 1));
          info_sum += info_gr(xx.row(j), a, b);
          s += 1;
          if (s >= ns) { s = 0; }
        }
      }
      break;
    }
    info_array(i) = info_sum;
  }

  return info_array / (double)nx;

}

//' Calculate the mutual information using full Bayesian
//'
//' Calculate the mutual information using full Bayesian.
//'
//' @param x A numeric vector of MCMC sampled theta values.
//' @param items_list A list of item parameter matrices.
//' @template calc-params-mini
// [[Rcpp::export]]
arma::colvec calc_MI_FB (
  const arma::rowvec& x,
  const List items_list,
  const arma::icolvec& ncat,
  const arma::icolvec& model) {

  int nx = x.n_rows;
  int ni = ncat.n_cols;
  colvec info_array(ni);

  for (int i = 0; i < ni; i++) {

    mat posterior_k(nx, ncat(i));
    rowvec prob(ncat(i));
    rowvec p(ncat[i]);
    mat item_parm = as<mat>(items_list[i]);
    int ns = item_parm.n_rows;
    int s = 0;
    double info_sum = 0;
    double sumP;

    switch (model(i)) {
      case 1: {
        for (int j = 0; j < nx; j++) {
          double b = item_parm(s, 0);
          p(1) = p_1pl(x.row(j), b);
          p(0) = 1 - p(1);
          posterior_k.row(j) = p;
          s += 1;
          if (s >= ns) { s = 0; }
        }
      }
      break;
      case 2: {
        for (int j = 0; j < nx; j++) {
          double a = item_parm(s, 0);
          double b = item_parm(s, 1);
          p(1) = p_2pl(x.row(j), a, b);
          p(0) = 1 - p(1);
          posterior_k.row(j) = p;
          s += 1;
          if (s >= ns) { s = 0; }
        }
      }
      break;
      case 3: {
        for (int j = 0; j < nx; j++) {
          double a = item_parm(s, 0);
          double b = item_parm(s, 1);
          double c = item_parm(s, 2);
          p(1) = p_3pl(x.row(j), a, b, c);
          p(0) = 1 - p(1);
          posterior_k.row(j) = p;
          s += 1;
          if (s >= ns) { s = 0; }
      }
      break;
      case 4: {
        for (int j = 0; j < nx; j++) {
          rowvec b = item_parm(s, span(0, ncat(i) - 2));
          p = p_pc(x.row(j), b);
          posterior_k.row(j) = p;
          s += 1;
          if (s >= ns) { s = 0; }
        }
      }
      break;
      case 5: {
        for (int j = 0; j < nx; j++) {
          double a = item_parm(s, 0);
          rowvec b = item_parm(s, span(1, ncat(i) - 1));
          p = p_gpc(x.row(j), a, b);
          posterior_k.row(j) = p;
          s += 1;
          if (s >= ns) { s = 0; }
        }
      }
      break;
      case 6: {
        for (int j = 0; j < nx; j++) {
          double a = item_parm(s, 0);
          rowvec b = item_parm(s, span(1, ncat(i) - 1));
          p = p_gr(x.row(j), a, b);
          posterior_k.row(j) = p;
          s += 1;
          if (s >= ns) { s = 0; }
        }
      }
      break;
    }

    prob = sum(posterior_k, 1);
    sumP = sum(prob);
    for (int k = 0; k < ncat(i); k++) {
      prob(k) /= sumP;
      for (int j = 0; j < nx; j++) {
        info_sum += log(posterior_k(j, k) / prob(k));
    }}
    info_array(i) = info_sum;
  }

  }

  return info_array / (double)nx;
}
