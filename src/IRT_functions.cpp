#include <Rcpp.h>
using namespace Rcpp;

//' Calculate probability at a single theta (1PL)
//' 
//' Calculate the probability of correct response at a theta value, under the 1PL model.
//' 
//' @template x-single
//' @template 1pl-params
//' @template 1pl-ref
// [[Rcpp::export]]
double p_1pl(const double& x, const double& b){
  return 1/(1+exp(b-x));
}

//' Calculate probability at multiple thetas (1PL)
//' 
//' Calculate the probability of correct response at theta values, under the 1PL model.
//' 
//' @template x-vector
//' @template 1pl-params
//' @template 1pl-ref
// [[Rcpp::export]]
NumericVector array_p_1pl(NumericVector x, const double& b){
  int nx = x.size();
  NumericVector p_array(nx);
  for(int j = 0; j < nx; j++) {
    p_array[j] = p_1pl(x[j],b);
  }
  return p_array;
}

//' Calculate probability at a single theta (2PL)
//' 
//' Calculate the probability of correct response at a theta value, under the 2PL model.
//' 
//' @template x-single
//' @template 2pl-params
//' @template 2pl-ref
// [[Rcpp::export]]
double p_2pl(const double& x, const double& a, const double& b){
  return 1/(1+exp(-a*(x-b)));
}

//' Calculate probability at multiple thetas (2PL)
//' 
//' Calculate the probability of correct response at theta values, under the 2PL model.
//'  
//' @template x-vector
//' @template 2pl-params
//' @template 2pl-ref
// [[Rcpp::export]]
NumericVector array_p_2pl(NumericVector x, const double& a, const double& b){
  int nx = x.size();
  NumericVector p_array(nx);
  for(int j = 0; j < nx; j++) {
    p_array[j] = p_2pl(x[j],a,b);
  }
  return p_array;
}

//' Calculate probability at a single theta (3PL)
//' 
//' Calculate the probability of correct response at a theta value, under the 3PL model.
//' 
//' @template x-single
//' @template 3pl-params
//' @template 3pl-ref
// [[Rcpp::export]]
double p_3pl(const double& x, const double& a, const double& b, const double& c){
  return c+(1-c)/(1+exp(-a*(x-b)));
}

//' Calculate probability at multiple thetas (3PL)
//' 
//' Calculate the probability of correct response at theta values, under the 3PL model.
//' 
//' @template x-vector
//' @template 3pl-params
//' @template 3pl-ref
// [[Rcpp::export]]
NumericVector array_p_3pl(NumericVector x, const double& a, const double& b, const double& c){
  int nx = x.size();
  NumericVector p_array(nx);
  for(int j = 0; j < nx; j++) {
    p_array[j] = p_3pl(x[j],a,b,c);
  }
  return p_array;
}

//' Calculate probability at a single theta (PC)
//' 
//' Calculate the probability of correct response at a theta value, under the partial credit model.
//' 
//' @template x-single
//' @template pc-params
//' @template pc-ref
// [[Rcpp::export]]
NumericVector p_pc(const double& x, NumericVector b){
  int nk = b.size()+1;
  NumericVector z(nk);
  z[0] = x;
  for(int k=1; k<nk; k++) {
    z[k] = x-b[k-1];
  }
  NumericVector zz = cumsum(z);
  NumericVector pp = exp(zz);
  double psum = sum(pp);
  return pp/psum;
}

//' Calculate probability at multiple thetas (PC)
//' 
//' Calculate the probability of correct response at theta values, under the partial credit model.
//' 
//' @template x-vector
//' @template pc-params
//' @template pc-ref
// [[Rcpp::export]]
NumericMatrix array_p_pc(NumericVector x, NumericVector b){
  int nx = x.size();
  int nk = b.size()+1;
  NumericMatrix p_array(nx,nk);
  NumericVector p(nk);
  for(int j = 0; j < nx; j++) {
    p = p_pc(x[j],b);
    for(int k = 0; k < nk; k++) {
      p_array(j,k) = p[k];
    }
  }
  return p_array;
}

//' Calculate probability at a single theta (GPC)
//' 
//' Calculate the probability of correct response at a theta value, under the generalized partial credit model.
//' 
//' @template x-single
//' @template gpc-params
//' @template gpc-ref
// [[Rcpp::export]]
NumericVector p_gpc(const double& x, const double& a, NumericVector b){
  int nk = b.size()+1;
  NumericVector z(nk);
  z[0] = a*x;
  for(int k=1; k<nk; k++) {
    z[k] = a*(x-b[k-1]);
  }
  NumericVector zz = cumsum(z);
  NumericVector pp = exp(zz);
  double psum = sum(pp);
  return pp/psum;
}

//' Calculate probability at multiple thetas (GPC)
//' 
//' Calculate the probability of correct response at theta values, under the generalized partial credit model.
//' 
//' @template x-vector
//' @template gpc-params
//' @template gpc-ref
// [[Rcpp::export]]
NumericMatrix array_p_gpc(NumericVector x, const double& a, NumericVector b){
  int nx = x.size();
  int nk = b.size()+1;
  NumericMatrix p_array(nx,nk);
  NumericVector p(nk);
  for(int j = 0; j < nx; j++) {
    p = p_gpc(x[j],a,b);
    for(int k = 0; k < nk; k++) {
      p_array(j,k) = p[k];
    }
  }
  return p_array;
}

//' Calculate probability at a single theta (GR)
//' 
//' Calculate the probability of correct response at a theta value, under the graded response model.
//' 
//' @template x-single
//' @template gr-params
//' @template gr-ref
// [[Rcpp::export]]
NumericVector p_gr(const double& x, const double& a, NumericVector b){
  int nk = b.size()+1;
  NumericVector p(nk), p_star(nk+1);
  p_star[0] = 1;
  p_star[nk] = 0;
  for(int k = 1; k < nk; k++){
    p_star[k] = p_2pl(x,a,b[k-1]);
  }
  for(int k = 0; k < nk; k++){
    p[k] = p_star[k]-p_star[k+1];
  }
  return p;
}

//' Calculate probability at multiple thetas (GR)
//' 
//' Calculate the probability of correct response at theta values, under the graded response model.
//' 
//' @template x-vector
//' @template gr-params
//' @template gr-ref
// [[Rcpp::export]]
NumericMatrix array_p_gr(NumericVector x, const double& a, NumericVector b){
  int nx = x.size();
  int nk = b.size()+1;
  NumericMatrix p_array(nx,nk);
  NumericVector p(nk);
  for(int j = 0; j < nx; j++) {
    p = p_gr(x[j],a,b);
    for(int k = 0; k < nk; k++) {
      p_array(j,k) = p[k];
    }
  }
  return p_array;
}

//' Calculate Fisher information at a single theta (1PL)
//' 
//' Calculate the Fisher information at a theta value according to the 1PL model.
//' 
//' @template x-single
//' @template 1pl-params
//' @template 1pl-ref
// [[Rcpp::export]]
double info_1pl(const double& x, const double& b){
  double p = p_1pl(x,b);
  return p*(1-p);
}

//' Calculate Fisher information at multiple thetas (1PL)
//' 
//' Calculate the Fisher information at theta values according to the 1PL model.
//' 
//' @template x-vector
//' @template 1pl-params
//' @template 1pl-ref
// [[Rcpp::export]]
NumericVector array_info_1pl(NumericVector x, const double& b){
  int nx = x.size();
  NumericVector info_array(nx);
  for(int j = 0; j < nx; j++) {
    info_array[j] = info_1pl(x[j],b);
  }
  return info_array;
}

//' Calculate Fisher information at a single theta (2PL)
//' 
//' Calculate the Fisher information at a theta value according to the 2PL model.
//' 
//' @template x-single
//' @template 2pl-params
//' @template 2pl-ref
// [[Rcpp::export]]
double info_2pl(const double& x, const double& a, const double& b){
  double p = p_2pl(x,a,b);
  return pow(a,2)*p*(1-p);
}

//' Calculate Fisher information at multiple thetas (2PL)
//' 
//' Calculate the Fisher information at theta values according to the 2PL model.
//' 
//' @template x-vector
//' @template 2pl-params
//' @template 2pl-ref
// [[Rcpp::export]]
NumericVector array_info_2pl(NumericVector x, const double& a, const double& b){
  int nx = x.size();
  NumericVector info_array(nx);
  for(int j = 0; j < nx; j++) {
    info_array[j] = info_2pl(x[j],a,b);
  }
  return info_array;
}

//' Calculate Fisher information at a single theta (3PL)
//' 
//' Calculate the Fisher information at a theta value according to the 3PL model.
//' 
//' @template x-single
//' @template 3pl-params
//' @template 3pl-ref
// [[Rcpp::export]]
double info_3pl(const double& x, const double& a, const double& b, const double& c){
  double p = p_3pl(x,a,b,c);
  return pow(a,2)*(1-p)/p*pow((p-c)/(1-c),2);
}

//' Calculate Fisher information at multiple thetas (3PL)
//' 
//' Calculate the Fisher information at theta values according to the 3PL model.
//' 
//' @template x-vector
//' @template 3pl-params
//' @template 3pl-ref
// [[Rcpp::export]]
NumericVector array_info_3pl(NumericVector x, const double& a, const double& b, const double& c){
  int nx = x.size();
  NumericVector info_array(nx);
  for(int j = 0; j < nx; j++) {
    info_array[j] = info_3pl(x[j],a,b,c);
  }
  return info_array;
}

//' Calculate Fisher information at a single theta (PC)
//' 
//' Calculate the Fisher information at a theta value according to the partial credit model.
//' 
//' @template x-single
//' @template pc-params
//' @template pc-ref
// [[Rcpp::export]]
double info_pc(const double& x, NumericVector b){
  NumericVector p = p_pc(x,b);
  int nk = b.size()+1;
  double const_1 = 0, const_2 = 0;
  for(int i = 0; i < nk; i++){
    const_1 += i*p[i];
    const_2 += i*i*p[i];
  }
  return const_2-pow(const_1,2);
}

//' Calculate Fisher information at multiple thetas (PC)
//' 
//' Calculate the Fisher information at theta values according to the partial credit model.
//' 
//' @template x-vector
//' @template pc-params
//' @template pc-ref
// [[Rcpp::export]]
NumericVector array_info_pc(NumericVector x, NumericVector b){
  int nx = x.size();
  NumericVector info_array(nx);
  for(int j = 0; j < nx; j++) {
    info_array[j] = info_pc(x[j],b);
  }
  return info_array;
}

//' Calculate Fisher information at a single theta (GPC).
//' 
//' Calculate the Fisher information at a theta value according to the generalizied partial credit model.
//' 
//' @template x-single
//' @template gpc-params
//' @template gpc-ref
// [[Rcpp::export]]
double info_gpc(const double& x, const double& a, NumericVector b){
  NumericVector p = p_gpc(x,a,b);
  int nk = b.size()+1;
  double const_1 = 0, const_2 = 0;
  for(int i = 0; i < nk; i++){
    const_1 += i*p[i];
    const_2 += i*i*p[i];
  }
  return pow(a,2)*(const_2-pow(const_1,2));
}

//' Calculate Fisher information at multiple thetas (GPC)
//' 
//' Calculate the Fisher information at theta values according to the generalized partial credit model.
//' 
//' @template x-vector
//' @template gpc-params
//' @template gpc-ref
// [[Rcpp::export]]
NumericVector array_info_gpc(NumericVector x, const double& a, NumericVector b){
  int nx = x.size();
  NumericVector info_array(nx);
  for(int j = 0; j < nx; j++) {
    info_array[j] = info_gpc(x[j],a,b);
  }
  return info_array;
}

//' Calculate Fisher information at a single theta (GR).
//' 
//' Calculate the Fisher information at a theta value according to the graded resposne model.
//' 
//' @template x-single
//' @template gr-params
//' @template gr-ref
// [[Rcpp::export]]
double info_gr(const double& x, const double& a, NumericVector b){
  int nk = b.size()+1;
  NumericVector p_star(nk+1);
  p_star[0] = 1;
  p_star[nk] = 0;
  for(int k = 1; k < nk; k++){
    p_star[k] = p_2pl(x,a,b[k-1]);
  }
  double out = 0;
  for(int k = 0; k < nk; k++){
    out += (p_star[k]-p_star[k+1])*pow(1-p_star[k]-p_star[k+1],2);
  }
  return out *= pow(a,2);
}

//' Calculate Fisher information at multiple thetas (GR)
//' 
//' Calculate the Fisher information at theta values according to the graded response model.
//' 
//' @template x-vector
//' @template gr-params
//' @template gr-ref
// [[Rcpp::export]]
NumericVector array_info_gr(NumericVector x, const double& a, NumericVector b){
  int nx = x.size();
  NumericVector info_array(nx);
  for(int j = 0; j < nx; j++) {
    info_array[j] = info_gr(x[j],a,b);
  }
  return info_array;
}

//' Calculate the Fisher information matrix for a single theta value and a set of items, potentially with a mixture of different models
//' 
//' @param x Numeric. A single theta value.
//' @param item_parm A matrix of item parameters.
//' @template calc-params-mini
// [[Rcpp::export]]
NumericVector calc_info(const double& x,
                        NumericMatrix item_parm,
                        IntegerVector ncat,
                        IntegerVector model){
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
NumericMatrix calc_info_matrix(NumericVector x,
                               NumericMatrix item_parm,
                               IntegerVector ncat,
                               IntegerVector model) {
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
NumericVector calc_info_EB(NumericVector x,
                           NumericMatrix item_parm,
                           IntegerVector ncat,
                           IntegerVector model){
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
NumericVector calc_info_FB(NumericVector x,
                           List items_list,
                           IntegerVector ncat,
                           IntegerVector model,
                           bool useEAP = false){
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
NumericVector calc_MI_FB(NumericVector x,
                         List items_list,
                         IntegerVector ncat,
                         IntegerVector model){
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

//' Calculate a likelihood value of theta
//' 
//' Calculate a likelihood value of theta.
//' 
//' @param x Numeric. A single theta value.
//' @param item_parm A numeric matrix of item parameters.
//' @param resp A numeric vector of item responses.
//' @template calc-params-mini
// [[Rcpp::export]]
double calc_likelihood(const double& x,
                       NumericMatrix item_parm,
                       IntegerVector resp,
                       IntegerVector ncat,
                       IntegerVector model){
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
NumericVector calc_likelihood_function(NumericVector theta_grid,
                                       NumericMatrix item_parm,
                                       IntegerVector resp,
                                       IntegerVector ncat,
                                       IntegerVector model){
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
double calc_log_likelihood(const double& x,
                       NumericMatrix item_parm,
                       IntegerVector resp,
                       IntegerVector ncat,
                       IntegerVector model,
                       const int& prior,
                       NumericVector prior_parm){
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
NumericVector calc_log_likelihood_function(NumericVector theta_grid,
                                       NumericMatrix item_parm,
                                       IntegerVector resp,
                                       IntegerVector ncat,
                                       IntegerVector model,
                                       const int& prior,
                                       NumericVector prior_parm){
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
double calc_posterior(const double& x,
                      NumericMatrix item_parm,
                      IntegerVector resp,
                      IntegerVector ncat,
                      IntegerVector model,
                      const int& prior,
                      NumericVector prior_parm){
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
NumericVector calc_posterior_function(NumericVector theta_grid,
                                      NumericMatrix item_parm,
                                      IntegerVector resp,
                                      IntegerVector ncat,
                                      IntegerVector model,
                                      const int& prior,
                                      NumericVector prior_parm){
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
double calc_posterior_single(const double& x,
                             NumericVector item_parm,
                             const int& resp,
                             const int& ncat,
                             const int& model,
                             const int& prior,
                             NumericVector prior_parm){
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

//' Calculate an EAP estimate of theta for one examinee
//' 
//' Calculate an expected a posterior estimate of theta for one examinee.
//' 
//' @param theta_grid An equi-spaced theta grid.
//' @param item_parm A numeric matrix of item parameters.
//' @template calc-params
// [[Rcpp::export]]
NumericVector theta_EAP(NumericVector theta_grid,
                        NumericMatrix item_parm,
                        IntegerVector resp,
                        IntegerVector ncat,
                        IntegerVector model,
                        const int& prior,
                        NumericVector prior_parm){
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
NumericMatrix theta_EAP_matrix(NumericVector theta_grid,
                               NumericMatrix item_parm,
                               IntegerMatrix Resp,
                               IntegerVector ncat,
                               IntegerVector model,
                               const int& prior,
                               NumericVector prior_parm){
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
NumericVector theta_EB(const int& nx,
                       const double& theta_init,
                       const double& theta_prop,
                       NumericMatrix item_parm,
                       IntegerVector resp,
                       IntegerVector ncat,
                       IntegerVector model,
                       const int& prior,
                       NumericVector prior_parm){
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
NumericVector theta_EB_single(const int& nx,
                              const double& theta_init,
                              const double& theta_prop,
                              NumericVector item_parm,
                              const int& resp,
                              const int& ncat,
                              const int& model,
                              const int& prior,
                              NumericVector prior_parm){
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
NumericVector theta_FB(const int& nx,
                       const double& theta_init,
                       const double& theta_prop,
                       List items_list,
                       NumericMatrix item_init,
                       IntegerVector resp,
                       IntegerVector ncat,
                       IntegerVector model,
                       const int& prior,
                       NumericVector prior_parm){
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
NumericVector theta_FB_single(const int& nx,
                              const double& theta_init,
                              const double& theta_prop,
                              NumericMatrix item_mcmc,
                              NumericVector item_init,
                              const int& resp,
                              const int& ncat,
                              const int& model,
                              const int& prior,
                              NumericVector prior_parm){
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

//' Find the segment to which each theta value belongs
//' 
//' Find the segment to which each theta value belongs.
//' 
//' @param segment A numeric vector of segment cuts.
//' @param x A numeric vector of theta values.
//' 
// [[Rcpp::export]]
IntegerVector find_segment(NumericVector segment,
                          NumericVector x) {
  int ns = segment.size();
  int nx = x.size();
  IntegerVector out(nx);
  for (int j = 0; j < nx; j++) {
    for (int k = 1; k < ns; k++) {
      if (x[j] <= segment[k]) {
        out[j] = k;
        break;
      }
    }
  }
  return out;
}
