#ifndef _POSTERIOR_FUNCTIONS_H
#define _POSTERIOR_FUNCTIONS_H

double calc_likelihood(
  const double&,
  const NumericMatrix&,
  const IntegerVector&,
  const IntegerVector&,
  const IntegerVector&);

NumericVector calc_likelihood_function(
  const NumericVector&,
  const NumericMatrix&,
  const IntegerVector&,
  const IntegerVector&,
  const IntegerVector&);

double calc_log_likelihood(
  const double&,
  const NumericMatrix&,
  const IntegerVector&,
  const IntegerVector&,
  const IntegerVector&,
  const int&,
  const NumericVector&);

NumericVector calc_log_likelihood_function(
  const NumericVector&,
  const NumericMatrix&,
  const IntegerVector&,
  const IntegerVector&,
  const IntegerVector&,
  const int&,
  const NumericVector&);

double calc_posterior(
  const double&,
  const NumericMatrix&,
  const IntegerVector&,
  const IntegerVector&,
  const IntegerVector&,
  const int&,
  const NumericVector&);

NumericVector calc_posterior_function(
  const NumericVector&,
  const NumericMatrix&,
  const IntegerVector&,
  const IntegerVector&,
  const IntegerVector&,
  const int&,
  const NumericVector&);

double calc_posterior_single(
  const double&,
  const NumericVector&,
  const int&,
  const int&,
  const int&,
  const int&,
  const NumericVector&);

#endif
