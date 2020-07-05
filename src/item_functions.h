#ifndef _ITEM_FUNCTIONS_H
#define _ITEM_FUNCTIONS_H

#include <Rcpp.h>
using namespace Rcpp;

double p_1pl(
  const double&,
  const double&);

double p_2pl(
  const double&,
  const double&,
  const double&);

double p_3pl(
  const double&,
  const double&,
  const double&,
  const double&);

NumericVector p_pc(
  const double&,
  const NumericVector&);

NumericVector p_gpc(
  const double&,
  const double&,
  const NumericVector&);

NumericVector p_gr(
  const double&,
  const double&,
  const NumericVector&);

NumericVector array_p_1pl(
  const NumericVector&,
  const double&);

NumericVector array_p_2pl(
  const NumericVector&,
  const double&,
  const double&);

NumericVector array_p_3pl(
  const NumericVector&,
  const double&,
  const double&,
  const double&);

NumericMatrix array_p_pc(
  const NumericVector&,
  const NumericVector&);

NumericMatrix array_p_gpc(
  const NumericVector&,
  const double&,
  const NumericVector&);

NumericMatrix array_p_gr(
  const NumericVector&,
  const double&,
  const NumericVector&);

double info_1pl(
  const double&,
  const double&);

double info_2pl(
  const double&,
  const double&,
  const double&);

double info_3pl(
  const double&,
  const double&,
  const double&,
  const double&);

double info_pc(
  const double&,
  const NumericVector&);

double info_gpc(
  const double&,
  const double&,
  const NumericVector&);

double info_gr(
  const double&,
  const double&,
  const NumericVector&);

NumericVector array_info_1pl(
  const NumericVector&,
  const double&);

NumericVector array_info_2pl(
  const NumericVector&,
  const double&,
  const double&);

NumericVector array_info_3pl(
  const NumericVector&,
  const double&,
  const double&,
  const double&);

NumericVector array_info_pc(
  const NumericVector&,
  const NumericVector&);

NumericVector array_info_gpc(
  const NumericVector&,
  const double&,
  const NumericVector&);

NumericVector array_info_gr(
  const NumericVector&,
  const double&,
  const NumericVector&);

#endif
