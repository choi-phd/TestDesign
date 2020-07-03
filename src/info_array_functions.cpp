#include "item_functions.h"
#include "info_array_functions.h"

//' Calculate the Fisher information matrix for a single theta value and a set of items, potentially with a mixture of different models
//'
//' @param x Numeric. A single theta value.
//' @param item_parm A matrix of item parameters.
//' @template calc-params-mini
// [[Rcpp::export]]
NumericVector calc_info(
  const double& x,
  const NumericMatrix& item_parm,
  const IntegerVector& ncat,
  const IntegerVector& model){
  int ni = item_parm.nrow();
  NumericVector info_array(ni);
  for (int i = 0; i < ni; i++){
    if (model[1] == 1){
      double b = item_parm(i,0);
      info_array[i] = info_1pl(x,b);
    } else if (model[i] == 2){
      double a = item_parm(i,0),b = item_parm(i,1);
      info_array[i] = info_2pl(x,a,b);
    } else if (model[i] == 3){
      double a = item_parm(i,0), b = item_parm(i,1), c = item_parm(i,2);
      info_array[i] = info_3pl(x,a,b,c);
    } else if (model[i] == 4){
      NumericVector b(ncat[i]-1);
      for (int j = 0; j < ncat[i]-1; j++){
        b[j] = item_parm(i,j);
      }
      info_array[i] = info_pc(x,b);
    } else {
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      for (int j = 0; j < ncat[i]-1; j++){
        b[j] = item_parm(i,j+1);
      }
      if (model[i] == 5){
        info_array[i] = info_gpc(x,a,b);
      } else if (model[i] == 6){
        info_array[i] = info_gr(x,a,b);
      }
    }
  }
  return info_array;
}

//' Calculate the Fisher information matrix for a vector of theta values and a set of items, potentially with a mixture of different models
//'
//' @param x Numeric. A vector of theta values.
//' @param item_parm A matrix of item parameters.
//' @template calc-params-mini
// [[Rcpp::export]]
NumericMatrix calc_info_matrix(
  const NumericVector& x,
  const NumericMatrix& item_parm,
  const IntegerVector& ncat,
  const IntegerVector& model){
  int nx = x.size();
  int ni = item_parm.nrow();
  NumericMatrix info_matrix(nx,ni);
  for (int i = 0; i < ni; i++) {
    if (model[i] == 1){
      double b = item_parm(i,0);
      for (int q = 0; q < nx; q++) {
        info_matrix(q,i) = info_1pl(x[q],b);
      }
    } else if (model[i] == 2){
      double a = item_parm(i,0), b = item_parm(i,1);
      for (int q = 0; q < nx; q++) {
        info_matrix(q,i) = info_2pl(x[q],a,b);
      }
    } else if (model[i] == 3){
      double a = item_parm(i,0), b = item_parm(i,1), c = item_parm(i,2);
      for (int q = 0; q < nx; q++) {
        info_matrix(q,i) = info_3pl(x[q],a,b,c);
      }
    } else if (model[i] == 4){
      NumericVector b(ncat[i]-1);
      for (int j = 0; j < ncat[i]-1; j++){
        b[j] = item_parm(i,j);
      }
      for (int q = 0; q < nx; q++) {
        info_matrix(q,i) = info_pc(x[q],b);
      }
    } else {
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      for (int j = 0; j < ncat[i]-1; j++){
        b[j] = item_parm(i,j+1);
      }
      if (model[i] == 5){
        for (int q = 0; q < nx; q++) {
          info_matrix(q,i) = info_gpc(x[q],a,b);
        }
      } else if (model[i] == 6){
        for (int q = 0; q < nx; q++) {
          info_matrix(q,i) = info_gr(x[q],a,b);
        }
      }
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
NumericVector calc_info_EB(
  const NumericVector& x,
  const NumericMatrix& item_parm,
  const IntegerVector& ncat,
  const IntegerVector& model){
  int nx = x.size();
  int ni = item_parm.nrow();
  NumericVector info_array(ni);
  for(int j = 0; j < nx; j++){
    NumericVector info = calc_info(x[j],item_parm,ncat,model);
    for(int i = 0; i < ni; i++){
      info_array[i] += info[i];
    }
  }
  return info_array / (double)nx;
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
NumericVector calc_info_FB(
  NumericVector& x,
  const List& items_list,
  const IntegerVector& ncat,
  const IntegerVector& model,
  const bool& useEAP = false){
  int nx = x.size();
  int ni = ncat.size();
  NumericVector info_array(ni);
  if (useEAP) x = rep(mean(x), nx);
  for(int i = 0; i < ni; i++){
    NumericMatrix item_parm = as<NumericMatrix>(items_list[i]);
    int ns = item_parm.nrow();
    int s = 0;
    double info_sum = 0;
    if (model[i] == 1) {
      for(int j = 0; j < nx; j++) {
        double b = item_parm(s,0);
        info_sum += info_1pl(x[j],b);
        s += 1;
        if (s >= ns) { s = 0; }
      }
    } else if (model[i] == 2) {
      for(int j = 0; j < nx; j++) {
        double a = item_parm(s,0);
        double b = item_parm(s,1);
        info_sum += info_2pl(x[j],a,b);
        s += 1;
        if (s >= ns) { s = 0; }
      }
    } else if (model[i] == 3) {
      for(int j = 0; j < nx; j++) {
        double a = item_parm(s,0);
        double b = item_parm(s,1);
        double c = item_parm(s,2);
        info_sum += info_3pl(x[j],a,b,c);
        s += 1;
        if (s >= ns) { s = 0; }
      }
    } else if (model[i] == 4) {
      for(int j = 0; j < nx; j++) {
        NumericVector b(ncat[i]-1);
        for (int k = 0; k < ncat[i]-1; k++){
          b[k] = item_parm(s,k);
        }
        info_sum += info_pc(x[j],b);
        s += 1;
        if (s >= ns) { s = 0; }
      }
    } else if (model[i] == 5) {
      for(int j = 0; j < nx; j++) {
        double a = item_parm(s,0);
        NumericVector b(ncat[i]-1);
        for (int k = 0; k < ncat[i]-1; k++){
          b[k] = item_parm(s,k+1);
        }
        info_sum += info_gpc(x[j],a,b);
        s += 1;
        if (s >= ns) { s = 0; }
      }
    } else if (model[i] == 6) {
      for(int j = 0; j < nx; j++) {
        double a = item_parm(s,0);
        NumericVector b(ncat[i]-1);
        for (int k = 0; k < ncat[i]-1; k++){
          b[k] = item_parm(s,k+1);
        }
        info_sum += info_gr(x[j],a,b);
        s += 1;
        if (s >= ns) { s = 0; }
      }
    }
    info_array[i] = info_sum;
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
NumericVector calc_MI_FB(
  const NumericVector& x,
  const List& items_list,
  const IntegerVector& ncat,
  const IntegerVector& model){
  int nx = x.size();
  int ni = ncat.size();
  NumericVector info_array(ni);
  for(int i = 0; i < ni; i++){
    NumericMatrix posterior_k(nx, ncat[i]);
    NumericVector prob(ncat[i]);
    NumericVector p(ncat[i]);
    NumericMatrix item_parm = as<NumericMatrix>(items_list[i]);
    int ns = item_parm.nrow();
    int s = 0;
    double info_sum = 0;
    double sumP;
    if (model[i] == 1) {
      for(int j = 0; j < nx; j++) {
        double b = item_parm(s,0);
        p[1] = p_1pl(x[j],b);
        p[0] = 1-p[1];
        posterior_k(j,_) = p;
        s += 1;
        if (s >= ns) { s = 0; }
    }} else if (model[i] == 2) {
      for(int j = 0; j < nx; j++) {
        double a = item_parm(s,0), b = item_parm(s,1);
        p[1] = p_2pl(x[j],a,b);
        p[0] = 1-p[1];
        posterior_k(j,_) = p;
        s += 1;
        if (s >= ns) { s = 0; }
    }} else if (model[i] == 3) {
      for(int j = 0; j < nx; j++) {
        double a = item_parm(s,0);
        double b = item_parm(s,1);
        double c = item_parm(s,2);
        p[1] = p_3pl(x[j],a,b,c);
        p[0] = 1-p[1];
        posterior_k(j,_) = p;
        s += 1;
        if (s >= ns) { s = 0; }
    }} else if (model[i] == 4) {
      for(int j = 0; j < nx; j++) {
        NumericVector b(ncat[i]-1);
        for (int k = 0; k < ncat[i]-1; k++){
          b[k] = item_parm(s,k);
        }
        p = p_pc(x[j],b);
        posterior_k(j,_) = p;
        s += 1;
        if (s >= ns) { s = 0; }
    }} else if (model[i] == 5) {
      for(int j = 0; j < nx; j++) {
        double a = item_parm(s,0);
        NumericVector b(ncat[i]-1);
        for (int k = 0; k < ncat[i]-1; k++){
          b[k] = item_parm(s,k+1);
        }
        p = p_gpc(x[j],a,b);
        posterior_k(j,_) = p;
        s += 1;
        if (s >= ns) { s = 0; }
    }} else if (model[i] == 6) {
      for(int j = 0; j < nx; j++) {
        double a = item_parm(s,0);
        NumericVector b(ncat[i]-1);
        for (int k = 0; k < ncat[i]-1; k++){
          b[k] = item_parm(s,k+1);
        }
        p = p_gr(x[j],a,b);
        posterior_k(j,_) = p;
        s += 1;
        if (s >= ns) { s = 0; }
    }}
    prob = rowSums(posterior_k);
    sumP = sum(prob);
    for(int k = 0; k < ncat[i]; k++) {
      prob[k] /= sumP;
      for(int j = 0; j < nx; j++) {
        info_sum += log(posterior_k(j,k)/prob[k]);
    }}
    info_array[i] = info_sum;
  }
  return info_array / (double)nx;
}
