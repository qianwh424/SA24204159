#include <Rcpp.h>
using namespace Rcpp;

//' @title the inner product of two vectors using cpp
//' @description add every components of two vectors and sum
//' @param x_vals one of the vectors
//' @param y_vals the other one of the vectors
//' @return the value of the inner product
//' @examples
//' \dontrun{
//' compute_integral_sum(c(1,2),c(1,2))
//' }
//' @export
// [[Rcpp::export]]
double compute_integral_sum(NumericVector x_vals,NumericVector y_vals) {
   double sum = 0;
   int n = x_vals.size();
   // 求加权和
   for (int i = 0; i < n; ++i) {
     sum += x_vals[i] * y_vals[i];  // f(x)*x
   }
   
   return sum;
 }