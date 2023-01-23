#ifndef _HESSIAN_H
#define _HESSIAN_H

double h_1pl(
  const arma::rowvec&,
  const double&,
  const double&);

double h_2pl(
  const arma::rowvec&,
  const double&,
  const double&,
  const double&);

arma::mat h_m_2pl(
  const arma::rowvec&,
  const arma::rowvec&,
  const double&,
  const double&);

double h_3pl(
  const arma::rowvec&,
  const double&,
  const double&,
  const double&,
  const double&);

arma::mat h_m_3pl(
  const arma::rowvec&,
  const arma::rowvec&,
  const double&,
  const double&,
  const double&);

double h_pc(
  const arma::rowvec&,
  const arma::rowvec&,
  const double&);

double h_gpc(
  const arma::rowvec&,
  const double&,
  const arma::rowvec&,
  const double&);

arma::mat h_m_gpc(
  const arma::rowvec&,
  const arma::rowvec&,
  const arma::rowvec&,
  const double&);

double h_gr(
  const arma::rowvec&,
  const double&,
  const arma::rowvec&,
  const double&);

arma::mat h_m_gr(
  const arma::rowvec&,
  const arma::rowvec&,
  const arma::rowvec&,
  const double&);

arma::colvec array_h_1pl(
  const arma::mat&,
  const double&,
  const double&);

arma::colvec array_h_2pl(
  const arma::mat&,
  const double&,
  const double&,
  const double&);

arma::colvec array_h_3pl(
  const arma::mat&,
  const double&,
  const double&,
  const double&,
  const double&);

arma::colvec array_h_pc(
  const arma::mat&,
  const arma::rowvec&,
  const double&);

arma::colvec array_h_gpc(
  const arma::mat&,
  const double&,
  const arma::rowvec&,
  const double&);

arma::colvec array_h_gr(
  const arma::mat&,
  const double&,
  const arma::rowvec&,
  const double&);

#endif
