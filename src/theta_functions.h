#ifndef _THETA_FUNCTIONS_H
#define _THETA_FUNCTIONS_H

NumericVector theta_EAP(
  const NumericVector&,
  const NumericMatrix&,
  const IntegerVector&,
  const IntegerVector&,
  const IntegerVector&,
  const int&,
  const NumericVector&);

NumericMatrix theta_EAP_matrix(
  const NumericVector&,
  const NumericMatrix&,
  const IntegerMatrix&,
  const IntegerVector&,
  const IntegerVector&,
  const int&,
  const NumericVector&);

NumericVector theta_EB(
  const int&,
  const double&,
  const double&,
  const NumericMatrix&,
  const IntegerVector&,
  const IntegerVector&,
  const IntegerVector&,
  const int&,
  const NumericVector&);

NumericVector theta_EB_single(
  const int&,
  const double&,
  const double&,
  const NumericVector&,
  const int&,
  const int&,
  const int&,
  const int&,
  const NumericVector&);

NumericVector theta_FB(
  const int&,
  const double&,
  const double&,
  const List&,
  const NumericMatrix&,
  const IntegerVector&,
  const IntegerVector&,
  const IntegerVector&,
  const int&,
  const NumericVector&);

NumericVector theta_FB_single(
  const int&,
  const double&,
  const double&,
  const NumericMatrix&,
  const NumericVector&,
  const int&,
  const int&,
  const int&,
  const int&,
  const NumericVector&);

#endif
