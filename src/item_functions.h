#ifndef _ITEM_FUNCTIONS_H
#define _ITEM_FUNCTIONS_H

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

double p_1pl(
  const arma::rowvec&,
  const double&);

double p_2pl(
  const arma::rowvec&,
  const double&,
  const double&);

double p_3pl(
  const arma::rowvec&,
  const double&,
  const double&,
  const double&);

arma::rowvec p_pc(
  const arma::rowvec&,
  const arma::rowvec&);

arma::rowvec p_gpc(
  const arma::rowvec&,
  const double&,
  const arma::rowvec&);

arma::rowvec p_gr(
  const arma::rowvec&,
  const double&,
  const arma::rowvec&);

arma::colvec array_p_1pl(
  const arma::mat&,
  const double&);

arma::colvec array_p_2pl(
  const arma::mat&,
  const double&,
  const double&);

arma::colvec array_p_3pl(
  const arma::mat&,
  const double&,
  const double&,
  const double&);

arma::mat array_p_pc(
  const arma::mat&,
  const arma::rowvec&);

arma::mat array_p_gpc(
  const arma::mat&,
  const double&,
  const arma::rowvec&);

arma::mat array_p_gr(
  const arma::mat&,
  const double&,
  const arma::rowvec&);

double info_1pl(
  const arma::rowvec&,
  const double&);

double info_2pl(
  const arma::rowvec&,
  const double&,
  const double&);

double info_3pl(
  const arma::rowvec&,
  const double&,
  const double&,
  const double&);

double info_pc(
  const arma::rowvec&,
  const arma::rowvec&);

double info_gpc(
  const arma::rowvec&,
  const double&,
  const arma::rowvec&);

double info_gr(
  const arma::rowvec&,
  const double&,
  const arma::rowvec&);

arma::colvec array_info_1pl(
  const arma::mat&,
  const double&);

arma::colvec array_info_2pl(
  const arma::mat&,
  const double&,
  const double&);

arma::colvec array_info_3pl(
  const arma::mat&,
  const double&,
  const double&,
  const double&);

arma::colvec array_info_pc(
  const arma::mat&,
  const arma::rowvec&);

arma::colvec array_info_gpc(
  const arma::mat&,
  const double&,
  const arma::rowvec&);

arma::colvec array_info_gr(
  const arma::mat&,
  const double&,
  const arma::rowvec&);

#endif
