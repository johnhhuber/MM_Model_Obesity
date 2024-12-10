// [[Rcpp::depends(RcppEigen)]]
#include <algorithm>
#include "spline.h"
#include <RcppEigen.h>


// [[Rcpp::export]]
Rcpp::NumericVector computeSpline(std::vector<double>& x_pred, std::vector<double>& x_knots, std::vector<double>& y_knots)
{
  tk::spline s;
  s.set_points(x_knots, y_knots);

  int n_pred = x_pred.size();
  Rcpp::NumericVector y_pred(n_pred);

  for(int ii = 0; ii < n_pred; ii++)
  {
    y_pred[ii] = s(x_pred[ii]);
  }

  return y_pred;
}
