#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// the SimpLinCpp function
// [[Rcpp::export]]
Rcpp::List SimpLinCpp(arma::vec x, arma::vec y) {
  // initialize the matrix of the right size with zeros.
  arma::mat X(x.n_elem, 2, arma::fill::zeros);
  
  // set the first column to be 1s
  X.col(0) = arma::ones(x.n_elem);
  
  // set the second column to be the xs
  X.col(1) = x;
  
  arma::vec beta_hats = (X.t() * X).i() * X.t() * y;
  
  arma::vec pred_vals = X * beta_hats;
  
  arma::vec resid = y - pred_vals;
  
  double mse = (1/(x.n_elem - 2.0)) * sum(square(resid));
  
  arma::mat cov_mat = mse * (X.t() * X).i();
  
  arma::vec std_err = {sqrt(cov_mat(0,0)), sqrt(cov_mat(1, 1))};
  
  arma::vec ci_b0 = {beta_hats[0] + R::qt(0.025, x.n_elem - 2, true, false) * std_err[0], 
                     beta_hats[0] - R::qt(0.025, x.n_elem - 2, true, false) * std_err[0]};
  
  arma::vec ci_b1 = {beta_hats[1] + R::qt(0.025, x.n_elem - 2, true, false) * std_err[1], 
                     beta_hats[1] - R::qt(0.025, x.n_elem - 2, true, false) * std_err[1]};
  
  return Rcpp::List::create(Named("coefficients") = beta_hats,
                            Named("stderr") = std_err,
                            Named("predicted values") = pred_vals,
                            Named("residuals") = resid,
                            Named("95% CI for Beta_0") = ci_b0,
                            Named("95% CI for Beta_1") = ci_b1);
}
