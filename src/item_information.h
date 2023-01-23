#ifndef _ITEM_INFORMATION_H
#define _ITEM_INFORMATION_H

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

arma::rowvec a_to_alpha(
  const arma::rowvec&);

double info_1pl(
  const arma::rowvec&,
  const double&);

double info_2pl(
  const arma::rowvec&,
  const double&,
  const double&);

arma::mat info_m_2pl(
  const arma::rowvec&,
  const arma::rowvec&,
  const double&);

double dirinfo_m_2pl(
  const arma::rowvec&,
  const arma::rowvec&,
  const double&);

double thisdirinfo_m_2pl(
  const arma::rowvec&,
  const arma::rowvec&,
  const arma::rowvec&,
  const double&);

double info_3pl(
  const arma::rowvec&,
  const double&,
  const double&,
  const double&);

arma::mat info_m_3pl(
  const arma::rowvec&,
  const arma::rowvec&,
  const double&,
  const double&);

double dirinfo_m_3pl(
  const arma::rowvec&,
  const arma::rowvec&,
  const double&,
  const double&);

double thisdirinfo_m_3pl(
  const arma::rowvec&,
  const arma::rowvec&,
  const arma::rowvec&,
  const double&,
  const double&);

double info_pc(
  const arma::rowvec&,
  const arma::rowvec&);

double info_gpc(
  const arma::rowvec&,
  const double&,
  const arma::rowvec&);

arma::mat info_m_gpc(
  const arma::rowvec&,
  const arma::rowvec&,
  const arma::rowvec&);

double dirinfo_m_gpc(
  const arma::rowvec&,
  const arma::rowvec&,
  const arma::rowvec&);

double thisdirinfo_m_gpc(
  const arma::rowvec&,
  const arma::rowvec&,
  const arma::rowvec&,
  const arma::rowvec&);

double info_gr(
  const arma::rowvec&,
  const double&,
  const arma::rowvec&);

arma::mat info_m_gr(
  const arma::rowvec&,
  const arma::rowvec&,
  const arma::rowvec&);

double dirinfo_m_gr(
  const arma::rowvec&,
  const arma::rowvec&,
  const arma::rowvec&);

double thisdirinfo_m_gr(
  const arma::rowvec&,
  const arma::rowvec&,
  const arma::rowvec&,
  const arma::rowvec&);

arma::colvec array_info_1pl(
  const arma::mat&,
  const double&);

arma::colvec array_info_2pl(
  const arma::mat&,
  const double&,
  const double&);

List array_info_m_2pl(
  const arma::mat&,
  const arma::rowvec&,
  const double&);

arma::colvec array_dirinfo_m_2pl(
  const arma::mat&,
  const arma::rowvec&,
  const double&);

arma::colvec array_thisdirinfo_m_2pl(
  const arma::mat&,
  const arma::rowvec&,
  const arma::rowvec&,
  const double&);

arma::colvec array_info_3pl(
  const arma::mat&,
  const double&,
  const double&,
  const double&);

List array_info_m_3pl(
  const arma::mat&,
  const arma::rowvec&,
  const double&,
  const double&);

arma::colvec array_dirinfo_m_3pl(
  const arma::mat&,
  const arma::rowvec&,
  const double&,
  const double&);

arma::colvec array_dirinfo_m_3pl(
  const arma::mat&,
  const arma::rowvec&,
  const arma::rowvec&,
  const double&,
  const double&);

arma::colvec array_info_pc(
  const arma::mat&,
  const arma::rowvec&);

arma::colvec array_info_gpc(
  const arma::mat&,
  const double&,
  const arma::rowvec&);

List array_info_m_gpc(
  const arma::mat&,
  const arma::rowvec&,
  const arma::rowvec&);

arma::colvec array_dirinfo_m_gpc(
  const arma::mat&,
  const arma::rowvec&,
  const arma::rowvec&);

arma::colvec array_thisdirinfo_m_gpc(
  const arma::mat&,
  const arma::rowvec&,
  const arma::rowvec&,
  const arma::rowvec&);

arma::colvec array_info_gr(
  const arma::mat&,
  const double&,
  const arma::rowvec&);

List array_info_m_gr(
  const arma::mat&,
  const arma::rowvec&,
  const arma::rowvec&);

arma::colvec array_dirinfo_m_gr(
  const arma::mat&,
  const arma::rowvec&,
  const arma::rowvec&);

arma::colvec array_thisdirinfo_m_gr(
  const arma::mat&,
  const arma::rowvec&,
  const arma::rowvec&,
  const arma::rowvec&);

#endif
