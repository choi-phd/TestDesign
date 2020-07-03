#include "item_functions.h"

//' Calculate probability at a single theta (1PL)
//'
//' Calculate the probability of correct response at a theta value, under the 1PL model.
//'
//' @template x-single
//' @template 1pl-params
//' @template 1pl-ref
// [[Rcpp::export]]
double p_1pl(const double& x, const double& b){
  return 1/(1+std::exp(b-x));
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
  return 1/(1+std::exp(-a*(x-b)));
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
  return c+(1-c)/(1+std::exp(-a*(x-b)));
}

//' Calculate probability at a single theta (PC)
//'
//' Calculate the probability of correct response at a theta value, under the partial credit model.
//'
//' @template x-single
//' @template pc-params
//' @template pc-ref
// [[Rcpp::export]]
NumericVector p_pc(const double& x, const NumericVector& b){
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

//' Calculate probability at a single theta (GPC)
//'
//' Calculate the probability of correct response at a theta value, under the generalized partial credit model.
//'
//' @template x-single
//' @template gpc-params
//' @template gpc-ref
// [[Rcpp::export]]
NumericVector p_gpc(const double& x, const double& a, const NumericVector& b){
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

//' Calculate probability at a single theta (GR)
//'
//' Calculate the probability of correct response at a theta value, under the graded response model.
//'
//' @template x-single
//' @template gr-params
//' @template gr-ref
// [[Rcpp::export]]
NumericVector p_gr(const double& x, const double& a, const NumericVector& b){
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

//' Calculate probability at multiple thetas (1PL)
//'
//' Calculate the probability of correct response at theta values, under the 1PL model.
//'
//' @template x-vector
//' @template 1pl-params
//' @template 1pl-ref
// [[Rcpp::export]]
NumericVector array_p_1pl(const NumericVector& x, const double& b){
  int nx = x.size();
  NumericVector p_array(nx);
  for(int j = 0; j < nx; j++) {
    p_array[j] = p_1pl(x[j],b);
  }
  return p_array;
}

//' Calculate probability at multiple thetas (2PL)
//'
//' Calculate the probability of correct response at theta values, under the 2PL model.
//'
//' @template x-vector
//' @template 2pl-params
//' @template 2pl-ref
// [[Rcpp::export]]
NumericVector array_p_2pl(const NumericVector& x, const double& a, const double& b){
  int nx = x.size();
  NumericVector p_array(nx);
  for(int j = 0; j < nx; j++) {
    p_array[j] = p_2pl(x[j],a,b);
  }
  return p_array;
}

//' Calculate probability at multiple thetas (3PL)
//'
//' Calculate the probability of correct response at theta values, under the 3PL model.
//'
//' @template x-vector
//' @template 3pl-params
//' @template 3pl-ref
// [[Rcpp::export]]
NumericVector array_p_3pl(const NumericVector& x, const double& a, const double& b, const double& c){
  int nx = x.size();
  NumericVector p_array(nx);
  for(int j = 0; j < nx; j++) {
    p_array[j] = p_3pl(x[j],a,b,c);
  }
  return p_array;
}

//' Calculate probability at multiple thetas (PC)
//'
//' Calculate the probability of correct response at theta values, under the partial credit model.
//'
//' @template x-vector
//' @template pc-params
//' @template pc-ref
// [[Rcpp::export]]
NumericMatrix array_p_pc(const NumericVector& x, const NumericVector& b){
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

//' Calculate probability at multiple thetas (GPC)
//'
//' Calculate the probability of correct response at theta values, under the generalized partial credit model.
//'
//' @template x-vector
//' @template gpc-params
//' @template gpc-ref
// [[Rcpp::export]]
NumericMatrix array_p_gpc(const NumericVector& x, const double& a, const NumericVector& b){
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

//' Calculate probability at multiple thetas (GR)
//'
//' Calculate the probability of correct response at theta values, under the graded response model.
//'
//' @template x-vector
//' @template gr-params
//' @template gr-ref
// [[Rcpp::export]]
NumericMatrix array_p_gr(const NumericVector& x, const double& a, const NumericVector& b){
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

//' Calculate Fisher information at a single theta (PC)
//'
//' Calculate the Fisher information at a theta value according to the partial credit model.
//'
//' @template x-single
//' @template pc-params
//' @template pc-ref
// [[Rcpp::export]]
double info_pc(const double& x, const NumericVector& b){
  NumericVector p = p_pc(x,b);
  int nk = b.size()+1;
  double const_1 = 0, const_2 = 0;
  for(int i = 0; i < nk; i++){
    const_1 += i*p[i];
    const_2 += i*i*p[i];
  }
  return const_2-pow(const_1,2);
}

//' Calculate Fisher information at a single theta (GPC).
//'
//' Calculate the Fisher information at a theta value according to the generalizied partial credit model.
//'
//' @template x-single
//' @template gpc-params
//' @template gpc-ref
// [[Rcpp::export]]
double info_gpc(const double& x, const double& a, const NumericVector& b){
  NumericVector p = p_gpc(x,a,b);
  int nk = b.size()+1;
  double const_1 = 0, const_2 = 0;
  for(int i = 0; i < nk; i++){
    const_1 += i*p[i];
    const_2 += i*i*p[i];
  }
  return pow(a,2)*(const_2-pow(const_1,2));
}

//' Calculate Fisher information at a single theta (GR).
//'
//' Calculate the Fisher information at a theta value according to the graded resposne model.
//'
//' @template x-single
//' @template gr-params
//' @template gr-ref
// [[Rcpp::export]]
double info_gr(const double& x, const double& a, const NumericVector& b){
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

//' Calculate Fisher information at multiple thetas (1PL)
//'
//' Calculate the Fisher information at theta values according to the 1PL model.
//'
//' @template x-vector
//' @template 1pl-params
//' @template 1pl-ref
// [[Rcpp::export]]
NumericVector array_info_1pl(const NumericVector& x, const double& b){
  int nx = x.size();
  NumericVector info_array(nx);
  for(int j = 0; j < nx; j++) {
    info_array[j] = info_1pl(x[j],b);
  }
  return info_array;
}

//' Calculate Fisher information at multiple thetas (2PL)
//'
//' Calculate the Fisher information at theta values according to the 2PL model.
//'
//' @template x-vector
//' @template 2pl-params
//' @template 2pl-ref
// [[Rcpp::export]]
NumericVector array_info_2pl(const NumericVector& x, const double& a, const double& b){
  int nx = x.size();
  NumericVector info_array(nx);
  for(int j = 0; j < nx; j++) {
    info_array[j] = info_2pl(x[j],a,b);
  }
  return info_array;
}

//' Calculate Fisher information at multiple thetas (3PL)
//'
//' Calculate the Fisher information at theta values according to the 3PL model.
//'
//' @template x-vector
//' @template 3pl-params
//' @template 3pl-ref
// [[Rcpp::export]]
NumericVector array_info_3pl(const NumericVector& x, const double& a, const double& b, const double& c){
  int nx = x.size();
  NumericVector info_array(nx);
  for(int j = 0; j < nx; j++) {
    info_array[j] = info_3pl(x[j],a,b,c);
  }
  return info_array;
}

//' Calculate Fisher information at multiple thetas (PC)
//'
//' Calculate the Fisher information at theta values according to the partial credit model.
//'
//' @template x-vector
//' @template pc-params
//' @template pc-ref
// [[Rcpp::export]]
NumericVector array_info_pc(const NumericVector& x, const NumericVector& b){
  int nx = x.size();
  NumericVector info_array(nx);
  for(int j = 0; j < nx; j++) {
    info_array[j] = info_pc(x[j],b);
  }
  return info_array;
}

//' Calculate Fisher information at multiple thetas (GPC)
//'
//' Calculate the Fisher information at theta values according to the generalized partial credit model.
//'
//' @template x-vector
//' @template gpc-params
//' @template gpc-ref
// [[Rcpp::export]]
NumericVector array_info_gpc(const NumericVector& x, const double& a, const NumericVector& b){
  int nx = x.size();
  NumericVector info_array(nx);
  for(int j = 0; j < nx; j++) {
    info_array[j] = info_gpc(x[j],a,b);
  }
  return info_array;
}

//' Calculate Fisher information at multiple thetas (GR)
//'
//' Calculate the Fisher information at theta values according to the graded response model.
//'
//' @template x-vector
//' @template gr-params
//' @template gr-ref
// [[Rcpp::export]]
NumericVector array_info_gr(const NumericVector& x, const double& a, const NumericVector& b){
  int nx = x.size();
  NumericVector info_array(nx);
  for(int j = 0; j < nx; j++) {
    info_array[j] = info_gr(x[j],a,b);
  }
  return info_array;
}
