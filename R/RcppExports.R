# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' @title the inner product of two vectors using cpp
#' @description add every components of two vectors and sum
#' @param x_vals one of the vectors
#' @param y_vals the other one of the vectors
#' @return the value of the inner product
#' @examples
#' \dontrun{
#' compute_integral_sum(c(1,2),c(1,2))
#' }
#' @export
compute_integral_sum <- function(x_vals, y_vals) {
    .Call('_SA24204159_compute_integral_sum', PACKAGE = 'SA24204159', x_vals, y_vals)
}

