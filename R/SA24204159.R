#' @title Calculate TVaR using R
#' @description Calculate TVaR for common distribution using R
#' @param distribution a character string specifying the distribution (for example, "norm" corresponds to the standard normal)
#' @param p a real valued parameter taking values in (0, 1)
#' @param n the number of parts dividing the interval
#' @param ... other parameters
#' @return the value of TVaR
#' @examples
#' \dontrun{
#'     TVaR_normal <- calculate_TVaR_cpp("norm", 0.95,mean=100,sd=5)
#'     print(TVaR_normal)
#'     TVaR_exp <- calculate_TVaR_cpp("exp", 0.95,rate=5)
#'     print(TVaR_exp)
#' }
#' @import stats
#' @export
calculate_TVaR_cpp <- function(distribution, p, n = 10000, ...) {
  # 构造分布函数名称
  qfun <- paste0("q", distribution)
  dfun <- paste0("d", distribution)
  
  # 检查函数是否存在
  if (!exists(qfun, mode = "function") || !exists(dfun, mode = "function")) {
    stop("this is not a common distribution!")
  }
  
  # 计算VaR(p)
  VaR <- do.call(qfun, c(list(p), list(...)))
  
  # 设置积分区间
  lower_limit <- VaR
  upper_limit <- VaR + max(100,n/100)  # 设置一个合适的上限（根据具体分布的尾部）
  
  # 生成 x 值序列
  dx <- (upper_limit - lower_limit) / n
  x_vals <- seq(lower_limit, upper_limit, by = dx)
  
  # 使用R的概率密度函数生成 y 值（即 f(x)）
  y_vals <- sapply(x_vals, function(x) do.call(dfun, c(list(x), list(...))))
  
  # 使用C++函数进行加权求和
  integral_value1 <- compute_integral_sum(x_vals[-1], y_vals[-1])* dx
  integral_value2 <- compute_integral_sum(x_vals[-length(y_vals)], y_vals[-length(y_vals)])*dx
  
  # 计算TVaR
  TVaR <- (integral_value1+integral_value2) / (2*(1 - p))
  
  return(round(TVaR,4))
}

#' @title Calculate ES using R
#' @description Calculate ES for common distribution using R
#' @param distribution a character string specifying the distribution (for example, "norm" corresponds to the standard normal)
#' @param p a real valued parameter taking values in (0, 1)
#' @param n the number of parts dividing the interval
#' @param ... other parameters
#' @return the value of TVaR
#' @examples
#' \dontrun{
#'     ES_normal <- calculate_ES_cpp("norm", 0.95,mean=100,sd=5)
#'     print(ES_normal)
#'     ES_exp <- calculate_ES_cpp("exp", 0.95,rate=5)
#'     print(ES_exp)
#' }
#' @import stats
#' @export
calculate_ES_cpp <- function(distribution, p, n = 10000, ...) {
  # 构造分布函数名称
  qfun <- paste0("q", distribution)
  dfun <- paste0("d", distribution)
  
  # 检查函数是否存在
  if (!exists(qfun, mode = "function") || !exists(dfun, mode = "function")) {
    stop("this is not a common distribution!")
  }
  
  # 计算VaR(p)
  VaR <- do.call(qfun, c(list(p), list(...)))
  
  # 设置积分区间
  lower_limit <- VaR
  upper_limit <- VaR + max(100,n/100)  # 设置一个合适的上限（根据具体分布的尾部）
  
  # 生成 x 值序列
  dx <- (upper_limit - lower_limit) / n
  x_vals <- seq(lower_limit, upper_limit, by = dx)
  
  # 使用R的概率密度函数生成 y 值（即 f(x)）
  y_vals <- sapply(x_vals, function(x) do.call(dfun, c(list(x), list(...))))
  
  # 使用C++函数进行加权求和
  integral_value1 <- compute_integral_sum(x_vals[-1], y_vals[-1])* dx
  integral_value2 <- compute_integral_sum(x_vals[-length(y_vals)], y_vals[-length(y_vals)])*dx
  
  # 计算ES
  ES <- (integral_value1+integral_value2) / 2 - VaR
  
  return(round(ES,4))
}



#' @title A package used for illustration.
#' @name Illustration
#' @import stats
#' @import Rcpp
#' @import ggplot2
#' @import MASS
#' @import boot
#' @import bootstrap
#' @import DAAG
#' @import microbenchmark
#' @useDynLib SA24204159
NULL