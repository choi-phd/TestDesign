#include "item_functions.h"
#include "posterior_functions.h"
#include "theta_functions.h"

//' Calculate an EAP estimate of theta for one examinee
//'
//' Calculate an expected a posterior estimate of theta for one examinee.
//'
//' @param theta_grid An equi-spaced theta grid.
//' @param item_parm A numeric matrix of item parameters.
//' @template calc-params
// [[Rcpp::export]]
NumericVector theta_EAP(
  const NumericVector& theta_grid,
  const NumericMatrix& item_parm,
  const IntegerVector& resp,
  const IntegerVector& ncat,
  const IntegerVector& model,
  const int& prior,
  const NumericVector& prior_parm){
  int nq = theta_grid.size();
  NumericVector out(2);
  NumericVector const_term(3);
  for(int q = 0; q < nq; q++){
    double x = theta_grid[q];
    double pos = calc_posterior(x,item_parm,resp,ncat,model,prior,prior_parm);
    const_term[0] += pos;
    const_term[1] += x*pos;
    const_term[2] += x*x*pos;
  }
  out[0] = const_term[1]/const_term[0];
  out[1] = sqrt(const_term[2]/const_term[0] - out[0]*out[0]);
  return out;
}

//' Calculate EAP estimates of theta for a group of examinees
//'
//' Calculate expected a posteriori estimates of theta for a group of examinees.
//'
//' @param theta_grid An equi-spaced theta grid.
//' @param item_parm A numeric matrix of item parameters.
//' @param Resp A numeric matrix of item responses.
//' @param ncat A numeric vector of the number of response categories by item.
//' @param model A numeric vector of the IRT model by item (1: 1PL, 2: 2PL, 3: 3PL, 4: PC, 5: GPC, 6: GR).
//' @param prior The type of prior distribution (1: normal, 2: uniform).
//' @param prior_parm A numeric vector of hyperparameters for the prior distribution, c(mu, sigma) or c(ll, ul).
//'
// [[Rcpp::export]]
NumericMatrix theta_EAP_matrix(
  const NumericVector& theta_grid,
  const NumericMatrix& item_parm,
  const IntegerMatrix& Resp,
  const IntegerVector& ncat,
  const IntegerVector& model,
  const int& prior,
  const NumericVector& prior_parm){
  int nq = theta_grid.size();
  int nj = Resp.nrow();
  int ni = Resp.ncol();
  NumericMatrix out(nj,2);
  for (int j = 0; j < nj; j++) {
    NumericVector const_term(3);
    IntegerVector resp(ni);
    resp = Resp(j,_);
    for(int q = 0; q < nq; q++){
      double x = theta_grid[q];
      double pos = calc_posterior(x,item_parm,resp,ncat,model,prior,prior_parm);
      const_term[0] += pos;
      const_term[1] += x*pos;
      const_term[2] += x*x*pos;
    }
    out(j,0) = const_term[1]/const_term[0];
    out(j,1) = sqrt(const_term[2]/const_term[0] - out(j,0)*out(j,0));
  }
  return out;
}

//' Calculate an empirical Bayes estimate of theta for one examinee
//'
//' Calculate an empirical Bayes estimate of theta for one examinee.
//'
//' @param nx The number of MCMC draws.
//' @param theta_init A value for initial estimate of theta.
//' @param theta_prop SD of the proposal distribution.
//' @param item_parm A numeric matrix of item parameters.
//' @template calc-params
// [[Rcpp::export]]
NumericVector theta_EB(
  const int& nx,
  const double& theta_init,
  const double& theta_prop,
  const NumericMatrix& item_parm,
  const IntegerVector& resp,
  const IntegerVector& ncat,
  const IntegerVector& model,
  const int& prior,
  const NumericVector& prior_parm){
  NumericVector out(nx);
  double x_0 = theta_init;
  double density_0 = calc_posterior(x_0,item_parm,resp,ncat,model,prior,prior_parm);
  for(int j = 0; j < nx; j++){
    GetRNGstate();
    NumericVector x_rnorm = Rcpp::rnorm(1);
    PutRNGstate();
    double x_1 = x_rnorm[0]*theta_prop + x_0;
    double density_1 = calc_posterior(x_1,item_parm,resp,ncat,model,prior,prior_parm);
    double prob_alpha = density_1/density_0;
    GetRNGstate();
    NumericVector p_accept = Rcpp::runif(1);
    PutRNGstate();
    if (p_accept[0] < prob_alpha){
      x_0 = x_1;
      density_0 = density_1;
    }
    out[j] = x_0;
  }
  return out;
}

//' Calculate an empirical Bayes estimate of theta for a single item
//'
//' Calculate an empirical Bayes estimate of theta for a single item.
//'
//' @param nx The number of MCMC draws.
//' @param theta_init A value for initial estimate of theta.
//' @param theta_prop SD of the proposal distribution.
//' @param item_parm A numeric matrix of item parameters.
//' @template calc-params
// [[Rcpp::export]]
NumericVector theta_EB_single(
  const int& nx,
  const double& theta_init,
  const double& theta_prop,
  const NumericVector& item_parm,
  const int& resp,
  const int& ncat,
  const int& model,
  const int& prior,
  const NumericVector& prior_parm){
  NumericVector out(nx);
  double x_0 = theta_init;
  double density_0 = calc_posterior_single(x_0,item_parm,resp,ncat,model,prior,prior_parm);
  for(int j = 0; j < nx; j++){
    GetRNGstate();
    NumericVector x_rnorm = Rcpp::rnorm(1);
    PutRNGstate();
    double x_1 = x_rnorm[0]*theta_prop + x_0;
    double density_1 = calc_posterior_single(x_1,item_parm,resp,ncat,model,prior,prior_parm);
    double prob_alpha = density_1/density_0;
    GetRNGstate();
    NumericVector p_accept = Rcpp::runif(1);
    PutRNGstate();
    if (p_accept[0] < prob_alpha){
      x_0 = x_1;
      density_0 = density_1;
    }
    out[j] = x_0;
  }
  return out;
}

//' Calculate a fully Bayesian estimate of theta for an examinee
//'
//' Calculate a fully Bayesian estimate of theta for an examinee.
//'
//' @param nx The number of MCMC draws.
//' @param theta_init A value for initial estimate of theta.
//' @param theta_prop SD of the proposal distribution.
//' @param items_list A list of item_parm matrices.
//' @param item_init A matrix of item parameter estimates (one row per item).
//' @template calc-params
// [[Rcpp::export]]
NumericVector theta_FB(
  const int& nx,
  const double& theta_init,
  const double& theta_prop,
  const List& items_list,
  const NumericMatrix& item_init,
  const IntegerVector& resp,
  const IntegerVector& ncat,
  const IntegerVector& model,
  const int& prior,
  const NumericVector& prior_parm){
  NumericVector out(nx);
  GetRNGstate();
  double x_0 = theta_init;
  int ni = item_init.nrow(), nparm = item_init.ncol();
  NumericMatrix item_parm(ni,nparm);
  for(int i = 0; i < ni; i++){
    for(int k = 0; k < nparm; k++){
      item_parm(i,k) = item_init(i,k);
    }
  }
  int nk = 0;
  double const_0 = calc_log_likelihood(x_0,item_parm,resp,ncat,model,prior,prior_parm);
  for(int j = 0; j < nx; j++){
    NumericVector x_rnorm = rnorm(1);
    double x_1 = x_rnorm[0]*theta_prop + x_0;
    NumericMatrix item_parm(ni,nparm);
    for(int m = 0; m < ni; m++){
      NumericMatrix item1 = as<NumericMatrix>(items_list[m]);
      int n3 = item1.nrow();
      GetRNGstate();
      NumericVector id_runif = Rcpp::runif(1);
      PutRNGstate();
      int ids = (int) floor(id_runif[0]*n3);
      if (model[m] <= 3){
        nk = model[m];
      } else if (model[m] == 4){
        nk = ncat[m] - 1;
      } else if (model[m] <= 6){
        nk = ncat[m];
      }
      for(int j = 0; j < nk; j++){
        item_parm(m,j) = item1(ids,j);
      }
    }
    double const_1 = calc_log_likelihood(x_1, item_parm, resp,ncat,model,prior,prior_parm);
    double prob_alpha = exp(const_1 - const_0);
    GetRNGstate();
    NumericVector p1 = Rcpp::runif(1);
    PutRNGstate();
    if (p1[0] <= prob_alpha){
      x_0 = x_1;
      const_0 = const_1;
    }
    out[j] = x_0;
  }
  PutRNGstate();
  return out;
}

//' Calculate a fully Bayesian estimate of theta for a single item
//'
//' Calculate a fully Bayesian estimate of theta for a single item.
//'
//' @param nx The number of MCMC draws.
//' @param theta_init A value for initial estimate of theta.
//' @param theta_prop SD of the proposal distribution.
//' @param item_mcmc A matrix of sampled item parameters for a single item.
//' @param item_init A matrix of item parameter estimates (one row per item).
//' @template calc-params
// [[Rcpp::export]]
NumericVector theta_FB_single(
  const int& nx,
  const double& theta_init,
  const double& theta_prop,
  const NumericMatrix& item_mcmc,
  const NumericVector& item_init,
  const int& resp,
  const int& ncat,
  const int& model,
  const int& prior,
  const NumericVector& prior_parm){
  NumericVector out(nx);
  double x_0 = theta_init;
  int ns = item_mcmc.nrow();
  int nk = 0;
  if (model == 1 || model == 2 || model == 3){
    nk = model;
  } else if (model == 4){
    nk = ncat - 1;
  } else if (model == 5 || model == 6){
    nk = ncat;
  }
  NumericVector item_parm(nk);
  for(int k = 0; k < nk; k++){
    item_parm[k] = item_init[k];
  }
  double const_0 = calc_posterior_single(x_0,item_parm,resp,ncat,model,prior,prior_parm);
  for(int j = 0; j < nx; j++){
    GetRNGstate();
    NumericVector x_rnorm = Rcpp::rnorm(1);
    PutRNGstate();
    double x_1 = x_rnorm[0]*theta_prop + x_0;
    GetRNGstate();
    NumericVector id_runif = Rcpp::runif(1);
    PutRNGstate();
    int ids = (int) floor(id_runif[0]*ns);
    for(int k = 0; k < nk; k++){
      item_parm[k] = item_mcmc(ids,k);
    }
    double const_1 = calc_posterior_single(x_1,item_parm,resp,ncat,model,prior,prior_parm);
    double prob_alpha = const_1/const_0;
    GetRNGstate();
    NumericVector p_accept = Rcpp::runif(1);
    PutRNGstate();
    if (p_accept[0] < prob_alpha){
      x_0 = x_1;
      const_0 = const_1;
    }
    out[j] = x_0;
  }
  return out;
}
