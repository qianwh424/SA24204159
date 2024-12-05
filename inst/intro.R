## ----eval=FALSE---------------------------------------------------------------
# # distribution a character string specifying the distribution (for example, "norm" corresponds to the standard normal)
# # p a real valued parameter taking values in (0, 1)
# # n the number of parts dividing the interval
# calculate_TVaR_cpp <- function(distribution, p, n = 10000, ...) {
#   # 构造分布函数名称
#   qfun <- paste0("q", distribution)
#   dfun <- paste0("d", distribution)
# 
#   # 检查函数是否存在
#   if (!exists(qfun, mode = "function") || !exists(dfun, mode = "function")) {
#     stop("this is not a common distribution!")
#   }
# 
#   # 计算VaR(p)
#   VaR <- do.call(qfun, c(list(p), list(...)))
# 
#   # 设置积分区间
#   lower_limit <- VaR
#   upper_limit <- VaR + max(100,n/100)  # 设置一个合适的上限（根据具体分布的尾部）
# 
#   # 生成 x 值序列
#   dx <- (upper_limit - lower_limit) / n
#   x_vals <- seq(lower_limit, upper_limit, by = dx)
# 
#   # 使用R的概率密度函数生成 y 值（即 f(x)）
#   y_vals <- sapply(x_vals, function(x) do.call(dfun, c(list(x), list(...))))
# 
#   # 使用C++函数进行加权求和
#   integral_value1 <- compute_integral_sum(x_vals[-1], y_vals[-1])* dx
#   integral_value2 <- compute_integral_sum(x_vals[-length(y_vals)], y_vals[-length(y_vals)])*dx
# 
#   # 计算TVaR
#   TVaR <- (integral_value1+integral_value2) / (2*(1 - p))
# 
#   return(round(TVaR,4))
# }
# 

## ----eval=FALSE---------------------------------------------------------------
# double compute_integral_sum(NumericVector x_vals,NumericVector y_vals) {
#    double sum = 0;
#    int n = x_vals.size();
#    // 求加权和
#    for (int i = 0; i < n; ++i) {
#      sum += x_vals[i] * y_vals[i];  // f(x)*x
#    }
# 
#    return sum;
#  }

## ----eval=FALSE---------------------------------------------------------------
# calculate_ES_cpp <- function(distribution, p, n = 10000, ...) {
#   # 构造分布函数名称
#   qfun <- paste0("q", distribution)
#   dfun <- paste0("d", distribution)
# 
#   # 检查函数是否存在
#   if (!exists(qfun, mode = "function") || !exists(dfun, mode = "function")) {
#     stop("this is not a common distribution!")
#   }
# 
#   # 计算VaR(p)
#   VaR <- do.call(qfun, c(list(p), list(...)))
# 
#   # 设置积分区间
#   lower_limit <- VaR
#   upper_limit <- VaR + max(100,n/100)  # 设置一个合适的上限（根据具体分布的尾部）
# 
#   # 生成 x 值序列
#   dx <- (upper_limit - lower_limit) / n
#   x_vals <- seq(lower_limit, upper_limit, by = dx)
# 
#   # 使用R的概率密度函数生成 y 值（即 f(x)）
#   y_vals <- sapply(x_vals, function(x) do.call(dfun, c(list(x), list(...))))
# 
#   # 使用C++函数进行加权求和
#   integral_value1 <- compute_integral_sum(x_vals[-1], y_vals[-1])* dx
#   integral_value2 <- compute_integral_sum(x_vals[-length(y_vals)], y_vals[-length(y_vals)])*dx
# 
#   # 计算ES
#   ES <- (integral_value1+integral_value2) / 2 + VaR
# 
#   return(round(ES,4))
# }

