#include <Rcpp.h>
using namespace Rcpp;

//' @rdname find_segment
//' @export
// [[Rcpp::export]]
IntegerVector find_segment(
  NumericVector x,
  NumericVector segment) {
  int ns = segment.size();
  int nx = x.size();
  IntegerVector out(nx);

  for (int j = 0; j < nx; j++) {
    for (int k = 1; k < ns; k++) {
      if (x[j] <= segment[k]) {
        out[j] = k;
        break;
      }
    }
  }

  return out;

}
