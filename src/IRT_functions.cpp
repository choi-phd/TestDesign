// [[Rcpp::depends(IRTclass)]]

#include <Rcpp.h>
#include <IRTclass.h>
using namespace Rcpp;

//' calc_info_EB
//' 
//' @param x A nice parameter
//' @param item_parm A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
//' 
//' @export
// [[Rcpp::export]]
NumericVector calc_info_EB(NumericVector x,
                           NumericMatrix item_parm,
                           IntegerVector ncat,
                           IntegerVector model){
  /*
  Emperical Bayes
  x: input vector of theta sampled values
  item_parm: matrix for item parameters
  ncat: number of levels/categories
  */
  int nx = x.size(); //number of theta values sampled
  int ni = item_parm.nrow(); //number of items
  NumericVector info_array(ni); //filled with 0
  for(int j = 0; j < nx; j++){
    NumericVector info = IRTclass::calc_info(x[j],item_parm,ncat,model);
    for(int i = 0; i < ni; i++){
      info_array[i] += info[i];
    }
  }
  return info_array / (double)nx; //casting is superflous per Rcpp
}

//' calc_info_FB
//' 
//' @param x A nice parameter
//' @param items_list A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
//' @param useEAP A nice parameter
//' 
//' @export
// [[Rcpp::export]]
NumericVector calc_info_FB(NumericVector x,
                           List items_list,
                           IntegerVector ncat,
                           IntegerVector model,
                           bool useEAP = false){
  /*
  Fully Bayesian
  x: input vector of theta values
  items_list: a list of length ni and each element is a matrix of parameters for one item 
  ncat: number of levels/categories
  model: vector of IRT models (1: 1pl, 2: 2pl, 3: 3pl, 4: pc, 5: gpc, 6: gr)
  */
  int nx = x.size(); //number of MCMC theta samples
  int ni = ncat.size(); //number of items
  NumericVector info_array(ni); //filled with 0
  if (useEAP) x = rep(mean(x), nx); //use EAP of MCMC sample
  for(int i = 0; i < ni; i++){
    NumericMatrix item_parm = as<NumericMatrix>(items_list[i]);
    int ns = item_parm.nrow(); //number of sampled parameter estimates for item i
    int s = 0; //index for MCMC draws for item i
    double info_sum = 0;
    if (model[i] == 1) { // 1PL model
      for(int j = 0; j < nx; j++) {
        
        double b = item_parm(s,0);
        
        info_sum += IRTclass::info_1pl(x[j],b);
        
        s += 1;
        if (s >= ns) { s = 0; }
      }
    } else if (model[i] == 2) { // 2PL model
      for(int j = 0; j < nx; j++) {
        
        double a = item_parm(s,0);
        double b = item_parm(s,1);
        
        info_sum += IRTclass::info_2pl(x[j],a,b);
        
        s += 1;
        if (s >= ns) { s = 0; }
      }
    } else if (model[i] == 3) { // 3PL model
      for(int j = 0; j < nx; j++) {
        
        double a = item_parm(s,0);
        double b = item_parm(s,1);
        double c = item_parm(s,2);
        
        info_sum += IRTclass::info_3pl(x[j],a,b,c);
        
        s += 1;
        if (s >= ns) { s = 0; }
      }
    } else if (model[i] == 4) { // PC
      for(int j = 0; j < nx; j++) {
        
        NumericVector b(ncat[i]-1);
        
        for (int k = 0; k < ncat[i]-1; k++){
          b[k] = item_parm(s,k);
        }
        
        info_sum += IRTclass::info_pc(x[j],b);
        
        s += 1;
        if (s >= ns) { s = 0; }
      }
    } else if (model[i] == 5) { // GPCM
      for(int j = 0; j < nx; j++) {
        
        double a = item_parm(s,0);
        NumericVector b(ncat[i]-1);
        
        for (int k = 0; k < ncat[i]-1; k++){
          b[k] = item_parm(s,k+1);
        }
        
        info_sum += IRTclass::info_gpc(x[j],a,b);
        
        s += 1;
        if (s >= ns) { s = 0; }
      }
    } else if (model[i] == 6) { // GR
      for(int j = 0; j < nx; j++) {
        
        double a = item_parm(s,0);
        NumericVector b(ncat[i]-1);
        
        for (int k = 0; k < ncat[i]-1; k++){
          b[k] = item_parm(s,k+1);
        }
        
        info_sum += IRTclass::info_gr(x[j],a,b);
        
        s += 1;
        if (s >= ns) { s = 0; }
      }
    }
    info_array[i] = info_sum;
  }
  return info_array / (double)nx;  //casting is superflous per Rcpp
}

//' calc_MI_FB
//' 
//' @param x A nice parameter
//' @param items_list A nice parameter
//' @param ncat A nice parameter
//' @param model A nice parameter
//' 
//' @export
// [[Rcpp::export]]
NumericVector calc_MI_FB(NumericVector x,
                         List items_list,
                         IntegerVector ncat,
                         IntegerVector model){
  /*
  Mutual Information
  Fully Bayesian
  x: input vector of theta values
  items_list: a list of length ni and each element is a matrix of parameters for one item 
  ncat: number of levels/categories
  model: vector of IRT models (1: 1pl, 2: 2pl, 3: 3pl, 4: pc, 5: gpc, 6: gr)
  */
  int nx = x.size(); //number of MCMC theta samples
  int ni = ncat.size(); //number of items
  NumericVector info_array(ni); //filled with 0
  for(int i = 0; i < ni; i++){
    NumericMatrix posterior_k(nx, ncat[i]);
    NumericVector prob(ncat[i]);
    NumericVector p(ncat[i]);
    NumericMatrix item_parm = as<NumericMatrix>(items_list[i]);
    int ns = item_parm.nrow(); //number of sampled parameter estimates for item i
    int s = 0; //index for MCMC draws for item i
    double info_sum = 0;
    double sumP;
    if (model[i] == 1) {  // 1PL model
      for(int j = 0; j < nx; j++) {
        
        double b = item_parm(s,0);
        
        p[1] = IRTclass::p_1pl(x[j],b);
        p[0] = 1-p[1];
        posterior_k(j,_) = p;
        
        s += 1;
        if (s >= ns) { s = 0; }
        
    }} else if (model[i] == 2) {  // 2PL model
      for(int j = 0; j < nx; j++) {
        
        double a = item_parm(s,0), b = item_parm(s,1);
        
        p[1] = IRTclass::p_2pl(x[j],a,b);
        p[0] = 1-p[1];
        posterior_k(j,_) = p;
        
        s += 1;
        if (s >= ns) { s = 0; }
        
    }} else if (model[i] == 3) { // 3PL model
      for(int j = 0; j < nx; j++) {
        
        double a = item_parm(s,0);
        double b = item_parm(s,1);
        double c = item_parm(s,2);
        
        p[1] = IRTclass::p_3pl(x[j],a,b,c);
        p[0] = 1-p[1];
        posterior_k(j,_) = p;
        
        s += 1;
        if (s >= ns) { s = 0; }
        
    }} else if (model[i] == 4) { // PC model
      for(int j = 0; j < nx; j++) {
        
        NumericVector b(ncat[i]-1);
        
        for (int k = 0; k < ncat[i]-1; k++){
          b[k] = item_parm(s,k);
        }
        
        p = IRTclass::p_pc(x[j],b);
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
        
        p = IRTclass::p_gpc(x[j],a,b);
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
        
        p = IRTclass::p_gr(x[j],a,b);
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
      
      p = IRTclass::p_1pl(x,b);
      
      if (resp[i] == 0) { p = (1-p); }
      lh *= p;
      
    } else if (model[i] == 2) {
      
      double a = item_parm(i,0);
      double b = item_parm(i,1);
      
      p = IRTclass::p_2pl(x,a,b);
      
      if (resp[i] == 0) { p = (1-p); }
      lh *= p;
      
    } else if (model[i] == 3) {
      
      double a = item_parm(i,0);
      double b = item_parm(i,1);
      double c = item_parm(i,2);
      
      p = IRTclass::p_3pl(x,a,b,c);
      
      if (resp[i] == 0) { p = (1-p); }
      lh *= p;
      
    } else if (model[i] == 4) {
      
      NumericVector b(ncat[i]-1);
      
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k);
      }
      
      NumericVector pp(ncat[i]);
      pp = IRTclass::p_pc(x,b);
      lh *= pp[resp[i]];
      
    } else if (model[i] == 5) {
      
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k+1);
      }
      
      NumericVector pp(ncat[i]);
      pp = IRTclass::p_gpc(x,a,b);
      lh *= pp[resp[i]];
      
    } else if (model[i] == 6) {
      
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k+1);
      }
      
      NumericVector pp(ncat[i]);
      pp = IRTclass::p_gr(x,a,b);
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
      
      pp(_,1) = IRTclass::array_p_1pl(theta_grid,b);
      pp(_,0) = 1 - pp(_,1);
      
    } else if (model[i] == 2) {
      
      double a = item_parm(i,0);
      double b = item_parm(i,1);
      
      pp(_,1) = IRTclass::array_p_2pl(theta_grid,a,b);
      pp(_,0) = 1 - pp(_,1);
      
    } else if (model[i] == 3) {
      
      double a = item_parm(i,0);
      double b = item_parm(i,1);
      double c = item_parm(i,2);
      
      pp(_,1) = IRTclass::array_p_3pl(theta_grid,a,b,c);
      pp(_,0) = 1 - pp(_,1);
      
    } else if (model[i] == 4) {
      
      NumericVector b(ncat[i]-1);
      
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k);
      }
      
      pp = IRTclass::array_p_pc(theta_grid,b);
      
    } else if (model[i] == 5) {
      
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k+1);
      }
      
      pp = IRTclass::array_p_gpc(theta_grid,a,b);
      
    } else if (model[i] == 6) {
      
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k+1);
      }
      
      pp = IRTclass::array_p_gr(theta_grid,a,b);
      
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
      
      p = IRTclass::p_1pl(x,b);
      
      if (resp[i] == 0) { p = (1-p); }
      llh += log(p);
      
    } else if (model[i] == 2) {
      
      double a = item_parm(i,0), b = item_parm(i,1);
      
      p = IRTclass::p_2pl(x,a,b);
      
      if (resp[i] == 0) { p = (1-p); }
      llh += log(p);
      
    } else if (model[i] == 3) {
      
      double a = item_parm(i,0), b = item_parm(i,1), c = item_parm(i,2);
      
      p = IRTclass::p_3pl(x,a,b,c);
      
      if (resp[i] == 0) { p = (1-p); }
      llh += log(p);
      
    } else if (model[i] == 4) {
      
      NumericVector b(ncat[i]-1);
      
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k);
      }
      
      NumericVector pp(ncat[i]);
      pp = IRTclass::p_pc(x,b);
      llh += log(pp[resp[i]]);
      
    } else if (model[i] == 5) {
      
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k+1);
      }
      
      NumericVector pp(ncat[i]);
      pp = IRTclass::p_gpc(x,a,b);
      llh += log(pp[resp[i]]);
      
    } else if (model[i] == 6) {
      
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k+1);
      }
      
      NumericVector pp(ncat[i]);
      pp = IRTclass::p_gr(x,a,b);
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
      
      pp(_,1) = IRTclass::array_p_1pl(theta_grid,b);
      pp(_,0) = 1 - pp(_,1);
      
    } else if (model[i] == 2){
      
      double a = item_parm(i,0);
      double b = item_parm(i,1);
      
      pp(_,1) = IRTclass::array_p_2pl(theta_grid,a,b);
      pp(_,0) = 1 - pp(_,1);
      
    } else if (model[i] == 3){
      
      double a = item_parm(i,0);
      double b = item_parm(i,1);
      double c = item_parm(i,2);
      
      pp(_,1) = IRTclass::array_p_3pl(theta_grid,a,b,c);
      pp(_,0) = 1 - pp(_,1);
      
    } else if (model[i] == 4){
      
      NumericVector b(ncat[i]-1);
      
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k);
      }
      
      pp = IRTclass::array_p_pc(theta_grid,b);
      
    } else if (model[i] == 5){
      
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k+1);
      }
      
      pp = IRTclass::array_p_gpc(theta_grid,a,b);
      
    } else if (model[i] == 6){
      
      double a = item_parm(i,0);
      NumericVector b(ncat[i]-1);
      
      for(int k = 0; k < ncat[i]-1; k++){
        b[k] = item_parm(i,k+1);
      }
      
      pp = IRTclass::array_p_gr(theta_grid,a,b);
      
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
    double p = IRTclass::p_1pl(x,b);
    
    if (resp == 0) { p = (1-p); }
    pos = p;
    
  } else if (model == 2){
    
    double a = item_parm[0];
    double b = item_parm[1];
    double p = IRTclass::p_2pl(x,a,b);
    
    if (resp == 0) { p = (1-p); }
    pos = p;
    
  } else if (model == 3){
    
    double a = item_parm[0];
    double b = item_parm[1];
    double c = item_parm[2];
    double p = IRTclass::p_3pl(x,a,b,c);
    
    if (resp == 0) { p = (1-p); }
    pos = p;
    
  } else if (model == 4) {
    
    NumericVector b(ncat-1);
    
    for(int k = 0; k < ncat-1; k++){
      b[k] = item_parm[k];
    }
    
    NumericVector p(ncat);
    p = IRTclass::p_pc(x,b);
    
    pos = p[resp];
    
  } else if (model == 5) {
    
    double a = item_parm[0];
    NumericVector b(ncat-1);
    
    for(int k = 0; k < ncat-1; k++){
      b[k] = item_parm[k+1];
    }
    
    NumericVector p(ncat);
    p = IRTclass::p_gpc(x,a,b);
    
    pos = p[resp];
    
  } else if (model == 6) {
    
    double a = item_parm[0];
    NumericVector b(ncat-1);
    
    for(int k = 0; k < ncat-1; k++){
      b[k] = item_parm[k+1];
    }
    
    NumericVector p(ncat);
    p = IRTclass::p_gr(x,a,b);
    
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
//' 
//' @export
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
//' 
//' @export
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
//' 
//' @export
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
//' 
//' @export
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

