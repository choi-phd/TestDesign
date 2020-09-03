#ifndef _THETA_FUNCTIONS_H
#define _THETA_FUNCTIONS_H

arma::colvec theta_EAP(
  const arma::mat&,
  const arma::mat&,
  const arma::irowvec&,
  const arma::irowvec&,
  const arma::irowvec&,
  const int&,
  const arma::rowvec&);

arma::mat theta_EAP_matrix(
  const arma::mat&,
  const arma::mat&,
  const arma::imat&,
  const arma::irowvec&,
  const arma::irowvec&,
  const int&,
  const arma::rowvec&);

arma::mat theta_EB(
  const int&,
  const arma::rowvec&,
  const double&,
  const arma::mat&,
  const arma::irowvec&,
  const arma::irowvec&,
  const arma::irowvec&,
  const int&,
  const arma::rowvec&);

arma::mat theta_EB_single(
  const int&,
  const arma::rowvec&,
  const double&,
  const arma::rowvec&,
  const int&,
  const int&,
  const int&,
  const int&,
  const arma::rowvec&);

arma::mat theta_FB(
  const int&,
  const arma::rowvec&,
  const double&,
  const List&,
  const arma::mat&,
  const arma::irowvec&,
  const arma::irowvec&,
  const arma::irowvec&,
  const int&,
  const arma::rowvec&);

arma::mat theta_FB_single(
  const int&,
  const arma::rowvec&,
  const double&,
  const arma::mat&,
  const arma::rowvec&,
  const int&,
  const int&,
  const int&,
  const int&,
  const arma::rowvec&);

#endif
