#include "item_functions.h"
#include "posterior_functions.h"

//' Calculate a likelihood value of theta
//'
//' Calculate a likelihood value of theta.
//'
//' @param x Numeric. A single theta value.
//' @param item_parm A numeric matrix of item parameters.
//' @param resp A numeric vector of item responses.
//' @template calc-params-mini
// [[Rcpp::export]]
double calc_likelihood(
  const double& x,
  const NumericMatrix& item_parm,
  const IntegerVector& resp,
  const IntegerVector& ncat,
  const IntegerVector& model){
  int ni = resp.size();
  double lh = 1;
  for(int i = 0; i < ni; i++) {
    double p;
    if (model[i] == 1) {
      double b = item_parm(i,0);
      p = p_1pl(x,b);
      if (resp[i] == 0) { p = (1-p); }
      lh *= p;
    } else if (model[i] == 2) {
      double a = item_parm(i,0);
      double b = item_parm(i,1);
      p = p_2pl(x,a,b);
      if (resp[i] == 0) { p = (1-p); }
      lh *= p;
    } else if (model[i] == 3) {
      double a = item_parm(i,0);
      double b = item_parm(i,1);
      double c = item_parm(i,2);
      p = p_3pl(x,a,b,c);
      if (resp[i] == 0) { p = (1-p); }
      lh *= p;
    } else if (model[i] == 4) {
      NumericVector b(ncat[i]-1);
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k);
      }
      NumericVector pp(ncat[i]);
      pp = p_pc(x,b);
      lh *= pp[resp[i]];
    } else if (model[i] == 5) {
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k+1);
      }
      NumericVector pp(ncat[i]);
      pp = p_gpc(x,a,b);
      lh *= pp[resp[i]];
    } else if (model[i] == 6) {
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k+1);
      }
      NumericVector pp(ncat[i]);
      pp = p_gr(x,a,b);
      lh *= pp[resp[i]];
    }
  }
  return lh;
}

//' Calculate a likelihood function of theta
//'
//' Calculate a likelihood function of theta.
//'
//' @param theta_grid An equi-spaced grid of theta values.
//' @param item_parm A numeric matrix of item parameters.
//' @param resp A numeric vector of item responses.
//' @template calc-params-mini
//'
// [[Rcpp::export]]
NumericVector calc_likelihood_function(
  const NumericVector& theta_grid,
  const NumericMatrix& item_parm,
  const IntegerVector& resp,
  const IntegerVector& ncat,
  const IntegerVector& model){
  int ni = resp.size();
  int nq = theta_grid.size();
  NumericMatrix pp(nq,max(ncat));
  NumericVector lh(nq);
  for (int q = 0; q < nq; q++) {
    lh[q] = 1;
  }
  for(int i = 0; i < ni; i++) {
    if (model[i] == 1) {
      double b = item_parm(i,0);
      pp(_,1) = array_p_1pl(theta_grid,b);
      pp(_,0) = 1 - pp(_,1);
    } else if (model[i] == 2) {
      double a = item_parm(i,0);
      double b = item_parm(i,1);
      pp(_,1) = array_p_2pl(theta_grid,a,b);
      pp(_,0) = 1 - pp(_,1);
    } else if (model[i] == 3) {
      double a = item_parm(i,0);
      double b = item_parm(i,1);
      double c = item_parm(i,2);
      pp(_,1) = array_p_3pl(theta_grid,a,b,c);
      pp(_,0) = 1 - pp(_,1);
    } else if (model[i] == 4) {
      NumericVector b(ncat[i]-1);
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k);
      }
      pp = array_p_pc(theta_grid,b);
    } else if (model[i] == 5) {
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k+1);
      }
      pp = array_p_gpc(theta_grid,a,b);
    } else if (model[i] == 6) {
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k+1);
      }
      pp = array_p_gr(theta_grid,a,b);
    }
    for (int q = 0; q < nq; q++) {
      lh[q] *= pp(q,resp[i]);
    }
  }
  return lh;
}

//' Calculate a log-likelihood value of theta
//'
//' Calculate a log-likelihood value of theta.
//'
//' @param x A length-one numeric vector for a theta value.
//' @param item_parm A numeric matrix of item parameters.
//' @template calc-params
// [[Rcpp::export]]
double calc_log_likelihood(
  const double& x,
  const NumericMatrix& item_parm,
  const IntegerVector& resp,
  const IntegerVector& ncat,
  const IntegerVector& model,
  const int& prior,
  const NumericVector& prior_parm){
  int ni = resp.size();
  double llh = 0;
  for(int i = 0; i < ni; i++) {
    double p;
    if (model[i] == 1) {
      double b = item_parm(i,0);
      p = p_1pl(x,b);
      if (resp[i] == 0) { p = (1-p); }
      llh += log(p);
    } else if (model[i] == 2) {
      double a = item_parm(i,0), b = item_parm(i,1);
      p = p_2pl(x,a,b);
      if (resp[i] == 0) { p = (1-p); }
      llh += log(p);
    } else if (model[i] == 3) {
      double a = item_parm(i,0), b = item_parm(i,1), c = item_parm(i,2);
      p = p_3pl(x,a,b,c);
      if (resp[i] == 0) { p = (1-p); }
      llh += log(p);
    } else if (model[i] == 4) {
      NumericVector b(ncat[i]-1);
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k);
      }
      NumericVector pp(ncat[i]);
      pp = p_pc(x,b);
      llh += log(pp[resp[i]]);
    } else if (model[i] == 5) {
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k+1);
      }
      NumericVector pp(ncat[i]);
      pp = p_gpc(x,a,b);
      llh += log(pp[resp[i]]);
    } else if (model[i] == 6) {
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k+1);
      }
      NumericVector pp(ncat[i]);
      pp = p_gr(x,a,b);
      llh += log(pp[resp[i]]);
    }
  }
  if (prior == 1){ //normal
    llh += -0.5*pow((x-prior_parm[0])/prior_parm[1],2)-log(prior_parm[1]);
  }
  return llh;
}

//' Calculate a log-likelihood function of theta
//'
//' Calculate a log-likelihood function of theta.
//'
//' @param theta_grid An equi-spaced grid of theta values.
//' @param item_parm A numeric matrix of item parameters.
//' @template calc-params
// [[Rcpp::export]]
NumericVector calc_log_likelihood_function(
  const NumericVector& theta_grid,
  const NumericMatrix& item_parm,
  const IntegerVector& resp,
  const IntegerVector& ncat,
  const IntegerVector& model,
  const int& prior,
  const NumericVector& prior_parm){
  int ni = resp.size();
  int nq = theta_grid.size();
  NumericMatrix pp(nq,max(ncat));
  NumericVector llh(nq);
  for(int i = 0; i < ni; i++) {
    if (model[i] == 1){
      double b = item_parm(i,0);
      pp(_,1) = array_p_1pl(theta_grid,b);
      pp(_,0) = 1 - pp(_,1);
    } else if (model[i] == 2){
      double a = item_parm(i,0);
      double b = item_parm(i,1);
      pp(_,1) = array_p_2pl(theta_grid,a,b);
      pp(_,0) = 1 - pp(_,1);
    } else if (model[i] == 3){
      double a = item_parm(i,0);
      double b = item_parm(i,1);
      double c = item_parm(i,2);
      pp(_,1) = array_p_3pl(theta_grid,a,b,c);
      pp(_,0) = 1 - pp(_,1);
    } else if (model[i] == 4){
      NumericVector b(ncat[i]-1);
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k);
      }
      pp = array_p_pc(theta_grid,b);
    } else if (model[i] == 5){
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k+1);
      }
      pp = array_p_gpc(theta_grid,a,b);
    } else if (model[i] == 6){
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k+1);
      }
      pp = array_p_gr(theta_grid,a,b);
    }
    for (int q = 0; q < nq; q++) {
      llh[q] += log(pp(q,resp[i]));
    }
  }
  if (prior == 1){
    for (int q = 0; q < nq; q++) {
      llh[q] += -0.5*pow((theta_grid[q]-prior_parm[0])/prior_parm[1],2)-log(prior_parm[1]);
    }
  }
  return llh;
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
  const double& x,
  const NumericMatrix& item_parm,
  const IntegerVector& resp,
  const IntegerVector& ncat,
  const IntegerVector& model,
  const int& prior,
  const NumericVector& prior_parm){
  double pos = calc_likelihood(x,item_parm,resp,ncat,model);
  switch (prior) {
  case 1: // normal
    pos *= exp(-0.5*pow((x-prior_parm[0])/prior_parm[1],2))/prior_parm[1]*0.3989423; //1/sqrt(2*MI_PI) = 0.3989423
    break;
  case 2: // uniform
    pos *= 1/(prior_parm[1]-prior_parm[0]);
    break;
  }
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
NumericVector calc_posterior_function(
  const NumericVector& theta_grid,
  const NumericMatrix& item_parm,
  const IntegerVector& resp,
  const IntegerVector& ncat,
  const IntegerVector& model,
  const int& prior,
  const NumericVector& prior_parm){
  int nq = theta_grid.size();
  NumericVector pos(nq);
  pos = calc_likelihood_function(theta_grid,item_parm,resp,ncat,model);
  for(int q = 0; q < nq; q++) {
    switch(prior){
    case 1: // normal
      pos[q] *= exp(-0.5*pow((theta_grid[q]-prior_parm[0])/prior_parm[1],2))/prior_parm[1]*0.3989423; //1/sqrt(2*MI_PI) = 0.3989423
      break;
    case 2: // uniform
      pos[q] *= 1/(prior_parm[1]-prior_parm[0]);
      break;
    }
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
  const double& x,
  const NumericVector& item_parm,
  const int& resp,
  const int& ncat,
  const int& model,
  const int& prior,
  const NumericVector& prior_parm){
  double pos = 0;
  if (model == 1){
    double b = item_parm[0];
    double p = p_1pl(x,b);
    if (resp == 0) { p = (1-p); }
    pos = p;
  } else if (model == 2){
    double a = item_parm[0];
    double b = item_parm[1];
    double p = p_2pl(x,a,b);
    if (resp == 0) { p = (1-p); }
    pos = p;
  } else if (model == 3){
    double a = item_parm[0];
    double b = item_parm[1];
    double c = item_parm[2];
    double p = p_3pl(x,a,b,c);
    if (resp == 0) { p = (1-p); }
    pos = p;
  } else if (model == 4) {
    NumericVector b(ncat-1);
    for(int k = 0; k < ncat-1; k++){
      b[k] = item_parm[k];
    }
    NumericVector p(ncat);
    p = p_pc(x,b);
    pos = p[resp];
  } else if (model == 5) {
    double a = item_parm[0];
    NumericVector b(ncat-1);
    for(int k = 0; k < ncat-1; k++){
      b[k] = item_parm[k+1];
    }
    NumericVector p(ncat);
    p = p_gpc(x,a,b);
    pos = p[resp];
  } else if (model == 6) {
    double a = item_parm[0];
    NumericVector b(ncat-1);
    for(int k = 0; k < ncat-1; k++){
      b[k] = item_parm[k+1];
    }
    NumericVector p(ncat);
    p = p_gr(x,a,b);
    pos = p[resp];
  }
  switch(prior) {
  case 1:
    pos *= exp(-0.5*pow((x-prior_parm[0])/prior_parm[1],2))/prior_parm[1]*0.3989423;
    break;
  case 2:
    pos *= 1/(prior_parm[1]-prior_parm[0]);
    break;
  }
  return pos;
}
