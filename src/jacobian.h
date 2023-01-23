#ifndef _JACOBIAN_H
#define _JACOBIAN_H

double j_1pl(
  const arma::rowvec&,
  const double&,
  const double&);

double j_2pl(
  const arma::rowvec&,
  const double&,
  const double&,
  const double&);

arma::rowvec j_m_2pl(
  const arma::rowvec&,
  const arma::rowvec&,
  const double&,
  const double&);

double j_3pl(
  const arma::rowvec&,
  const arma::rowvec&,
  const double&,
  const double&,
  const double&);

arma::rowvec j_m_3pl(
  const arma::rowvec&,
  const arma::rowvec&,
  const double&,
  const double&,
  const double&);

double j_pc(
  const arma::rowvec&,
  const arma::rowvec&,
  const double&);

double j_gpc(
  const arma::rowvec&,
  const double&,
  const arma::rowvec&,
  const double&);

arma::rowvec j_m_gpc(
  const arma::rowvec&,
  const arma::rowvec&,
  const arma::rowvec&,
  const double&);

double j_gr(
  const arma::rowvec&,
  const double&,
  const arma::rowvec&,
  const double&);

arma::rowvec j_m_gr(
  const arma::rowvec&,
  const arma::rowvec&,
  const arma::rowvec&,
  const double&);

arma::colvec array_j_1pl(
  const arma::mat&,
  const double&,
  const double&);

arma::colvec array_j_2pl(
  const arma::mat&,
  const double&,
  const double&,
  const double&);

arma::colvec array_j_3pl(
  const arma::mat&,
  const double&,
  const double&,
  const double&,
  const double&);

arma::colvec array_j_pc(
  const arma::mat&,
  const arma::rowvec&,
  const double&);

arma::colvec array_j_gpc(
  const arma::mat&,
  const double&,
  const arma::rowvec&,
  const double&);

arma::colvec array_j_gr(
  const arma::mat&,
  const double&,
  const arma::rowvec&,
  const double&);

#endif
