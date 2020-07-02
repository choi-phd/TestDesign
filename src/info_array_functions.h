#ifndef _INFO_ARRAY_FUNCTIONS_H
#define _INFO_ARRAY_FUNCTIONS_H

arma::colvec calc_info(
  const arma::rowvec&,
  const arma::mat&,
  const arma::icolvec&,
  const arma::icolvec&);

arma::mat calc_info_matrix(
  const arma::mat&,
  const arma::mat&,
  const arma::icolvec&,
  const arma::icolvec&);

arma::colvec calc_info_EB (
  const arma::mat&,
  const arma::mat&,
  const arma::icolvec&,
  const arma::icolvec&);

arma::colvec calc_info_FB (
  const arma::mat&,
  const List&,
  const arma::icolvec&,
  const arma::icolvec&,
  const bool&);

arma::colvec calc_MI_FB (
  const arma::rowvec&,
  const List&,
  const arma::icolvec&,
  const arma::icolvec&);

#endif
