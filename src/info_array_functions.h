#ifndef _INFO_ARRAY_FUNCTIONS_H
#define _INFO_ARRAY_FUNCTIONS_H

NumericVector calc_info(
  const double&,
  const NumericMatrix&,
  const IntegerVector&,
  const IntegerVector&);

NumericMatrix calc_info_matrix(
  const NumericVector&,
  const NumericMatrix&,
  const IntegerVector&,
  const IntegerVector&);

NumericVector calc_info_EB(
  const NumericVector&,
  const NumericMatrix&,
  const IntegerVector&,
  const IntegerVector&);

NumericVector calc_info_FB(
  const NumericVector&,
  const List&,
  const IntegerVector&,
  const IntegerVector&,
  const bool&);

NumericVector calc_MI_FB(
  const NumericVector&,
  const List&,
  const IntegerVector&,
  const IntegerVector&);

#endif
