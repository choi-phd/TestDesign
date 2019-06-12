#include <Rcpp.h>
using namespace Rcpp;

//' Calculate the probability of a correct response according to the 1pl model
//'
//' @param x a length-one numeric vector for a theta value
//' @param b a length-one numeric vector for the difficulty parameter
//' @template 1pl-ref
//' @export
// [[Rcpp::export]]
double p_1pl(const double& x, const double& b){
  return 1/(1+exp(b-x));
}

//' Calculate the probability of a correct response for a vector of theta values according to the 1pl model
//'
//' @param x a numeric vector of theta values
//' @param b a length-one numeric vector for the difficulty parameter
//' @template 1pl-ref
//' @export
// [[Rcpp::export]]
NumericVector array_p_1pl(NumericVector x, const double& b){
  int nx = x.size();
  NumericVector p_array(nx);
  for(int j = 0; j < nx; j++) {
    p_array[j] = p_1pl(x[j],b);
  }
  return p_array;
}

//' Calculate the probability of a correct response according to the 2pl model
//'
//' @param x a length-one numeric vector for a theta value
//' @param a a length-one numeric vector for the slope parameter
//' @param b a length-one numeric vector for the difficulty parameter
//' @template 2pl-ref
//' @export
// [[Rcpp::export]]
double p_2pl(const double& x, const double& a, const double& b){
  return 1/(1+exp(-a*(x-b)));
}

//' Calculate the probability of a correct response for a vector of theta values according to the 2pl model
//'
//' @param x a numeric vector of theta values
//' @param a a length-one numeric vector for the slope parameter
//' @param b a length-one numeric vector for the difficulty parameter
//' @template 2pl-ref
//' @export
// [[Rcpp::export]]
NumericVector array_p_2pl(NumericVector x, const double& a, const double& b){
  int nx = x.size();
  NumericVector p_array(nx);
  for(int j = 0; j < nx; j++) {
    p_array[j] = p_2pl(x[j],a,b);
  }
  return p_array;
}

//' Calculate the probability of a correct response according to the 3pl model
//'
//' @param x a length-one numeric vector for a theta value
//' @param a a length-one numeric vector for the slope parameter
//' @param b a length-one numeric vector for the difficulty parameter
//' @param c a length-one numeric vector for the guessing parameter
//' @template 3pl-ref
//' @export
// [[Rcpp::export]]
double p_3pl(const double& x, const double& a, const double& b, const double& c){
  return c+(1-c)/(1+exp(-a*(x-b)));
}

//' Calculate the probability of a correct response for a vector of theta values according to the 3pl model
//'
//' @param x a numeric vector of theta values
//' @param a a length-one numeric vector for the slope parameter
//' @param b a length-one numeric vector for the difficulty parameter
//' @param c a length-one numeric vector for the guessing parameter
//' @template 3pl-ref
//' @export
// [[Rcpp::export]]
NumericVector array_p_3pl(NumericVector x, const double& a, const double& b, const double& c){
  int nx = x.size();
  NumericVector p_array(nx);
  for(int j = 0; j < nx; j++) {
    p_array[j] = p_3pl(x[j],a,b,c);
  }
  return p_array;
}

//' Calculate the response probabilities according to the partial credit model
//'
//' @param x a length-one numeric vector for a theta value
//' @param b a numeric vector for the threshold parameters
//' @template pc-ref
//' @export
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

//' Calculate the response probabilities for a vector of theta values according to the partial credit model
//'
//' @param x a numeric vector of theta values
//' @param b a numeric vector for the threshold parameters
//' @template pc-ref
//' @export
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

//' Calculate the response probabilities according to the generalizaed partial credit model
//'
//' @param x a length-one numeric vector for a theta value
//' @param a a length-one numeric vector for the slope parameter
//' @param b a numeric vector for the threshold parameters
//' @template gpc-ref
//' @export
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

//' Calculate the response probabilities for a vector of theta values according to the generalized partial credit model
//'
//' @param x a numeric vector of theta values
//' @param a a length-one numeric vector for the slope parameter
//' @param b a numeric vector for the threshold parameters
//' @template gpc-ref
//' @export
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

//' Calculate the response probabilities according to the graded response model
//'
//' @param x a length-one numeric vector for a theta value
//' @param a a length-one numeric vector for the slope parameter
//' @param b a numeric vector for the category boundary parameters
//' @template gr-ref
//' @export
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

//' Calculate the response probabilities for a vector of theta values according to the graded response model
//'
//' @param x a numeric vector of theta values
//' @param a a length-one numeric vector for the slope parameter
//' @param b a numeric vector for the category boundary parameters
//' @template gr-ref
//' @export
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

//' Calculate the Fisher information according to the 1pl model
//'
//' @param x a length-one numeric vector for a theta value
//' @param b a numeric vector for the threshold parameters
//' @template 1pl-ref
//' @export
// [[Rcpp::export]]
double info_1pl(const double& x, const double& b){
  double p = p_1pl(x,b);
  return p*(1-p);
}

//' Calculate the Fisher information for a vector of theta values according to the 1pl model
//'
//' @param x a vector of theta value
//' @param b a length-one numeric vector for the difficulty parameter
//' @template 1pl-ref
//' @export
// [[Rcpp::export]]
NumericVector array_info_1pl(NumericVector x, const double& b){
  int nx = x.size();
  NumericVector info_array(nx);
  for(int j = 0; j < nx; j++) {
    info_array[j] = info_1pl(x[j],b);
  }
  return info_array;
}

//' Calculate the Fisher information according to the 2pl model
//'
//' @param x a length-one numeric vector for a theta value
//' @param a a length-one numeric vector for the slope parameter
//' @param b a length-one numeric vector for the difficulty parameter
//' @template 2pl-ref
//' @export
// [[Rcpp::export]]
double info_2pl(const double& x, const double& a, const double& b){
  double p = p_2pl(x,a,b);
  return pow(a,2)*p*(1-p);
}

//' Calculate the Fisher information for a vector of theta values according to the 2pl model
//'
//' @param x a vector of theta values
//' @param a a length-one numeric vector for the slope parameter
//' @param b a length-one numeric vector for the difficulty parameter
//' @template 2pl-ref
//' @export
// [[Rcpp::export]]
NumericVector array_info_2pl(NumericVector x, const double& a, const double& b){
  int nx = x.size();
  NumericVector info_array(nx);
  for(int j = 0; j < nx; j++) {
    info_array[j] = info_2pl(x[j],a,b);
  }
  return info_array;
}

//' Calculate the Fisher information according to the 3pl model
//'
//' @param x a length-one numeric vector for a theta value
//' @param a a length-one numeric vector for the slope parameter
//' @param b a length-one numeric vector for the difficulty parameter
//' @param c a length-one numeric vector for the guessing parameter
//' @template 3pl-ref
//' @export
// [[Rcpp::export]]
double info_3pl(const double& x, const double& a, const double& b, const double& c){
  double p = p_3pl(x,a,b,c);
  return pow(a,2)*(1-p)/p*pow((p-c)/(1-c),2);
}

//' Calculate the Fisher information for a vector of theta values according to the 3pl model
//'
//' @param x a vector of theta values
//' @param a a length-one numeric vector for the slope parameter
//' @param b a length-one numeric vector for the difficulty parameter
//' @param c a length-one numeric vector for the guessing parameter
//' @template 3pl-ref
//' @export
// [[Rcpp::export]]
NumericVector array_info_3pl(NumericVector x, const double& a, const double& b, const double& c){
  int nx = x.size();
  NumericVector info_array(nx);
  for(int j = 0; j < nx; j++) {
    info_array[j] = info_3pl(x[j],a,b,c);
  }
  return info_array;
}

//' Calculate the Fisher information according to the partial credit model
//'
//' @param x a length-one numeric vector for a theta value
//' @param b a numeric vector for the threshold parameters
//' @template pc-ref
//' @export
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

//' Calculate the Fisher information for a vector of theta values according to the partial credit model
//'
//' @param x a vector of theta values
//' @param b a numeric vector for the threshold parameters
//' @template pc-ref
//' @export
// [[Rcpp::export]]
NumericVector array_info_pc(NumericVector x, NumericVector b){
  int nx = x.size();
  NumericVector info_array(nx);
  for(int j = 0; j < nx; j++) {
    info_array[j] = info_pc(x[j],b);
  }
  return info_array;
}

//' Calculate the Fisher information according to the generalized partial credit model
//'
//' @param x a length-one numeric vector for a theta value
//' @param a a length-one numeric vector for the slope parameter
//' @param b a numeric vector for the threshold parameters
//' @template gpc-ref
//' @export
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

//' Calculate the Fisher information for a vector of theta values according to the generalized partial credit model
//'
//' @param x a vector of theta values
//' @param a a length-one numeric vector for the slope parameter
//' @param b a numeric vector for the threshold parameters
//' @template gpc-ref
//' @export
// [[Rcpp::export]]
NumericVector array_info_gpc(NumericVector x, const double& a, NumericVector b){
  int nx = x.size();
  NumericVector info_array(nx);
  for(int j = 0; j < nx; j++) {
    info_array[j] = info_gpc(x[j],a,b);
  }
  return info_array;
}

//' Calculate the Fisher information according to the graded response model
//'
//' @param x a length-one numeric vector for a theta value
//' @param a a length-one numeric vector for the slope parameter
//' @param b a numeric vector for the category boundary parameters
//' @template gr-ref
//' @export
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

//' Calculate the Fisher information for a vector of theta values according to the graded response model
//'
//' @param x a vector of theta values
//' @param a a length-one numeric vector for the slope parameter
//' @param b a numeric vector for the category boundary parameters
//' @template gr-ref
//' @export
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
//' @param x a length-one numeric vector for a theta value
//' @param item_parm a matrix of item parameters
//' @param ncat a numeric vector of the number of response categories by item
//' @param model a numeric vector of the IRT model by item (1: 1pl, 2: 2pl, 3: 3pl, 4: pc, 5: gpc, 6: gr)
//' @export
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
//' @param x a numeric vector of theta values
//' @param item_parm a matrix of item parameters
//' @param ncat a numeric vector of the number of response categories by item
//' @param model a numeric vector of the IRT model by item (1: 1pl, 2: 2pl, 3: 3pl, 4: pc, 5: gpc, 6: gr)
//' @export
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

//' calc_info_EB
//'
//' @param x A nice parameter
//' @param item_parm A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
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

//' calc_info_FB
//'
//' @param x A nice parameter
//' @param items_list A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
//' @param useEAP A nice parameter
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

//' calc_MI_FB
//'
//' @param x A nice parameter
//' @param items_list A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
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

    }} else if (model[i] == 5) { // GPCM
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
  return info_array / (double)nx;  //casting is superflous per Rcpp
}

//' calc_likelihood
//'
//' @param x A nice parameter
//' @param item_parm A nice parameter
//' @param resp A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
//'
//' @export
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

//' calc_likelihood_function
//'
//' @param theta_grid A nice parameter
//' @param item_parm A nice parameter
//' @param resp A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
//'
//' @export
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

//' calc_log_likelihood
//'
//' @param x A nice parameter
//' @param item_parm A nice parameter
//' @param resp A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
//' @param prior A nice parameter
//' @param prior_parm A nice parameter
//'
//' @export
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

//' calc_log_likelihood_function
//'
//' @param theta_grid A nice parameter
//' @param item_parm A nice parameter
//' @param resp A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
//' @param prior A nice parameter
//' @param prior_parm A nice parameter
//'
//' @export
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

  if (prior == 1){ //normal
    for (int q = 0; q < nq; q++) {
      llh[q] += -0.5*pow((theta_grid[q]-prior_parm[0])/prior_parm[1],2)-log(prior_parm[1]);
    }
  }
  return llh;
}

//' calc_posterior
//'
//' @param x A nice parameter
//' @param item_parm A nice parameter
//' @param resp A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
//' @param prior A nice parameter
//' @param prior_parm A nice parameter
//'
//' @export
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

//' calc_posterior_function
//'
//' @param theta_grid A nice parameter
//' @param item_parm A nice parameter
//' @param resp A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
//' @param prior A nice parameter
//' @param prior_parm A nice parameter
//'
//' @export
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

//' calc_posterior_single
//'
//' @param x A nice parameter
//' @param item_parm A nice parameter
//' @param resp A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
//' @param prior A nice parameter
//' @param prior_parm A nice parameter
//'
//' @export
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

//' theta_EAP
//'
//' @param theta_grid A nice parameter
//' @param item_parm A nice parameter
//' @param resp A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
//' @param prior A nice parameter
//' @param prior_parm A nice parameter
//'
//' @export
// [[Rcpp::export]]
NumericVector theta_EAP(NumericVector theta_grid,
                        NumericMatrix item_parm,
                        IntegerVector resp,
                        IntegerVector ncat,
                        IntegerVector model,
                        const int& prior,
                        NumericVector prior_parm){
  /*
  * theta_grid: equi-spaced theta grid
  */
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

//' theta_EAP_matrix
//'
//' @param theta_grid A nice parameter
//' @param item_parm A nice parameter
//' @param Resp A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
//' @param prior A nice parameter
//' @param prior_parm A nice parameter
//'
//' @export
// [[Rcpp::export]]
NumericMatrix theta_EAP_matrix(NumericVector theta_grid,
                               NumericMatrix item_parm,
                               IntegerMatrix Resp,
                               IntegerVector ncat,
                               IntegerVector model,
                               const int& prior,
                               NumericVector prior_parm){
  /*
  * theta_grid: equi-spaced theta grid
  */
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

//' theta_EB
//'
//' @param nx A nice parameter
//' @param theta_init A nice parameter
//' @param theta_prop A nice parameter
//' @param item_parm A nice parameter
//' @param resp A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
//' @param prior A nice parameter
//' @param prior_parm A nice parameter
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
  /*
  * Empirical Bayes
  * nx: number of MCMC draws
  * theta_init: initial theta estimate
  * theta_prop: SD of the proposal distribution
  * item_parm: matrix of item parameter estimates (one row per item)
  * resp: vector of item responses
  * ncat: vector of number of response categories by item
  * model: vector of IRT models
  * prior: prior distribution, 1 = normal, 2 = uniform
  * prior_parm: hyper parameters for the prior distribution c(mu, sigma) or c(ll, ul)
  */
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

//' theta_EB_single
//'
//' @param nx A nice parameter
//' @param theta_init A nice parameter
//' @param theta_prop A nice parameter
//' @param item_parm A nice parameter
//' @param resp A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
//' @param prior A nice parameter
//' @param prior_parm A nice parameter
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
  /*
  * Empirical Bayes
  * nx: number of MCMC draws
  * theta_init: initial theta estimate
  * theta_prop: SD of the proposal distribution
  * item_parm: matrix of item parameter estimates (one row per item)
  * resp: vector of item responses
  * ncat: vector of number of response categories by item
  * model: vector of IRT models
  * prior: prior distribution, 1 = normal, 2 = uniform
  * prior_parm: hyper parameters for the prior distribution c(mu, sigma) or c(ll, ul)
  */
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

//' theta_FB
//'
//' @param nx A nice parameter
//' @param theta_init A nice parameter
//' @param theta_prop A nice parameter
//' @param items_list A nice parameter
//' @param item_init A nice parameter
//' @param resp A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
//' @param prior A nice parameter
//' @param prior_parm A nice parameter
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
  /*
  * Fully Bayesian
  * nx: number of MCMC draws
  * theta_init: initial theta estimate
  * theta_prop: SD of the proposal distribution
  * items_list: a list of length ni and each element is a matrix for one item
  * item_init: matrix of item parameter estimates (one row per item)
  * resp: vector of item responses
  * ncat: vector of number of response categories by item
  * model: vector of IRT models
  * prior: prior distribution, 1 = normal, 2 = uniform
  * prior_parm: hyper parameters for the prior distribution c(mu, sigma) or c(ll, ul)
  */
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

//' theta_FB_single
//'
//' @param nx A nice parameter
//' @param theta_init A nice parameter
//' @param theta_prop A nice parameter
//' @param item_mcmc A nice parameter
//' @param item_init A nice parameter
//' @param resp A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
//' @param prior A nice parameter
//' @param prior_parm A nice parameter
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
  /*
  * Fully Bayesian for single item
  * nx: number of MCMC samples
  * theta_init: initial theta estimate
  * theta_prop: SD of the proposal distribution = priorSD x jumpFactor
  * items_mcmc: a matrix of MCMC item parameters samples for one item
  * item_init: matrix of item parameter estimates (one row per item)
  * resp: response for one item
  * ncat: number of response categories for one item
  * model: IRT model for one item
  * prior: prior distribution, 1 = normal, 2 = uniform, should be always 1 in this context
  * prior_parm: hyper parameters for the prior distribution c(mu, sigma) or c(ll, ul)
  */

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

//' findSegment
//'
//' @param segment A nice parameter
//' @param x A nice parameter
//'
//' @export
// [[Rcpp::export]]
IntegerVector findSegment(NumericVector segment,
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
