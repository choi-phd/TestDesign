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

double j_3pl(
  const arma::rowvec&,
  const double&,
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

double j_gr(
  const arma::rowvec&,
  const double&,
  const arma::rowvec&,
  const double&);

#endif
