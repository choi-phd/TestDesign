#ifndef _POSTERIOR_FUNCTIONS_H
#define _POSTERIOR_FUNCTIONS_H

double calc_likelihood (
  const arma::rowvec&,
  const arma::mat&,
  const arma::irowvec&,
  const arma::irowvec&,
  const arma::irowvec&);

arma::colvec calc_likelihood_function(
  const arma::mat&,
  const arma::mat&,
  const arma::irowvec&,
  const arma::irowvec&,
  const arma::irowvec&);

double calc_log_likelihood(
  const arma::rowvec&,
  const arma::mat&,
  const arma::irowvec&,
  const arma::irowvec&,
  const arma::irowvec&,
  const int&,
  const arma::rowvec&);

arma::colvec calc_log_likelihood_function(
  const arma::mat&,
  const arma::mat&,
  const arma::irowvec&,
  const arma::irowvec&,
  const arma::irowvec&,
  const int&,
  const arma::rowvec&);

double calc_prior_multiplier(
  const double&,
  const int&,
  const arma::rowvec&);

double calc_posterior(
  const arma::rowvec&,
  const arma::mat&,
  const arma::irowvec&,
  const arma::irowvec&,
  const arma::irowvec&,
  const int&,
  const arma::rowvec&);

arma::colvec calc_posterior_function(
  const arma::mat&,
  const arma::mat&,
  const arma::icolvec&,
  const arma::icolvec&,
  const arma::icolvec&,
  const int&,
  const arma::rowvec&);

double calc_posterior_single(
  const arma::rowvec&,
  const arma::rowvec&,
  const int&,
  const int&,
  const int&,
  const int&,
  const arma::rowvec&);

#endif
