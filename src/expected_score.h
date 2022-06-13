#ifndef _EXPECTED_SCORE_H
#define _EXPECTED_SCORE_H

double e_1pl(
  const arma::rowvec&,
  const double&);

double e_2pl(
  const arma::rowvec&,
  const double&,
  const double&);

double e_3pl(
  const arma::rowvec&,
  const double&,
  const double&,
  const double&);

double e_pc(
  const arma::rowvec&,
  const arma::rowvec&);

double e_gpc(
  const arma::rowvec&,
  const double&,
  const arma::rowvec&);

double e_gr(
  const arma::rowvec&,
  const double&,
  const arma::rowvec&);

arma::colvec array_e_1pl(
  const arma::mat&,
  const double&);

arma::colvec array_e_2pl(
  const arma::mat&,
  const double&,
  const double&);

arma::colvec array_e_3pl(
  const arma::mat&,
  const double&,
  const double&,
  const double&);

arma::colvec array_e_pc(
  const arma::mat&,
  const arma::rowvec&);

arma::colvec array_e_gpc(
  const arma::mat&,
  const double&,
  const arma::rowvec&);

arma::colvec array_p_gr(
  const arma::mat&,
  const double&,
  const arma::rowvec&);

#endif
