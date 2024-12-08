## -----------------------------------------------------------------------------
# 逆分布法生成Rayleigh分布随机数的函数
rayleigh <- function(n, sigma) {
  # 生成n个[0, 1)区间的均匀随机数
  u <- runif(n)
  # 使用逆分布函数公式生成Rayleigh分布的随机数
  x <- sigma * sqrt(-2 * log(1 - u))
  return(x)
}

# 设置参数
sigma <- c(1, 2, 3)  # σ的不同取值
n <- 10000  # 每次生成的样本数量

# 生成Rayleigh分布样本并绘制直方图
par(mfrow = c(1, length(sigma)))  # 设置多图布局
for (sigma in sigma) {
  # 生成样本
  samples <- rayleigh(n, sigma)
  
  # 绘制直方图
  hist(samples, breaks = 50, col = "mediumseagreen", border = "black", 
       main = paste("Rayleigh(σ=", sigma, ")", sep = ""), 
       xlab = "x", ylab = "频率", freq = FALSE)
  abline(v = sigma, col = "red", lwd = 2, lty = 2)
  legend("topright", legend = paste("理论众数: σ =", sigma), 
         col = "red", lwd = 2, lty = 2)
  }


## ----warning=FALSE------------------------------------------------------------
set.seed(1)
# 混合正态分布样本生成函数
generate_mixture <- function(n, p1, mu1, mu2, sigma1, sigma2) {
  # 生成n个均匀分布的随机数用于决定成分
  u <- runif(n)
  
  # 使用混合概率p1选择不同成分的样本
  samples <- ifelse(u < p1, rnorm(n, mean = mu1, sd = sigma1), rnorm(n, mean = mu2, sd = sigma2))
  return(samples)
}

# 设置参数
n_samples <- 1000   
mu1 <- 0            
mu2 <- 3            
sigma1 <- 1         
sigma2 <- 1         

# 生成混合分布样本
p1_values <- c(0.75, 0.5, 0.25)  # 不同的 p1 值

# 绘图
par(mfrow = c(1, length(p1_values)))  # 设置多图布局
for (p1 in p1_values) {
  # 生成混合分布样本
  samples <- generate_mixture(n_samples, p1, mu1, mu2, sigma1, sigma2)
  
  # 绘制样本的直方图
  hist(samples, breaks = 30, col = "mediumseagreen", border = "black", prob = TRUE,
       main = paste("p1 =", p1), xlab = "x", ylab = "频率")

  # 添加正态分布密度函数的叠加线
  curve(p1 * dnorm(x, mean = mu1, sd = sigma1) + (1 - p1) * dnorm(x, mean = mu2, sd = sigma2), 
        col = "red", lwd = 1, add = TRUE)
}



## -----------------------------------------------------------------------------
set.seed(1)
# 设置参数
lambda <- 1  # 泊松过程的参数 
alpha <- 1   # Gamma分布的形状参数
beta <- 1    # Gamma分布的尺度参数
t <- 10       # 时间点 X(t)

# 模拟次数
num <- 10000

# 模拟复合泊松-伽马过程
X_t <- numeric(num)

for (i in 1:num) {
  N_t <- rpois(1, lambda * t)
  if (N_t > 0) {
    Y <- rgamma(N_t, shape = alpha, rate = beta)
    X_t[i] <- sum(Y)
  } else {
    X_t[i] <- 0
  }
}

# 估计 X(t) 的均值和方差
e_mean <- mean(X_t)
e_variance <- var(X_t)

# 理论均值和方差
E_Y1 <- alpha / beta
E_Y1_square <- (alpha * (alpha + 1)) / (beta ^ 2)
t_mean <- lambda * t * E_Y1
t_variance <- lambda * t * E_Y1_square

# 打印结果
cat("估计的均值:", e_mean, "\n")
cat("理论的均值:", t_mean, "\n")
cat("估计的方差:", e_variance, "\n")
cat("理论的方差:", t_variance, "\n")

# 绘制直方图
hist(X_t, breaks = 30, col = "mediumseagreen", border = "black", main = "直方图 (复合泊松-伽马过程)",
     xlab = "X(10)", ylab = "频率")


## -----------------------------------------------------------------------------
# 设置种子
set.seed(1)

# 设置参数
lambda <- 1  # 泊松过程的参数 
alpha <- 1   # Gamma分布的形状参数
beta <- 1    # Gamma分布的尺度参数
t <- 10       # 时间点 X(t)

# 模拟次数
num <- 1000

# 模拟复合泊松-伽马过程
X_t <- numeric(num)

for (i in 1:num) {
  N_t <- rpois(1, lambda * t)
  if (N_t > 0) {
    Y <- rgamma(N_t, shape = alpha, rate = beta)
    X_t[i] <- sum(Y)
  } else {
    X_t[i] <- 0
  }
}

# 估计 X(t) 的均值和方差
e_mean <- mean(X_t)
e_variance <- var(X_t)

# 理论均值和方差
E_Y1 <- alpha / beta
E_Y1_square <- (alpha * (alpha + 1)) / (beta ^ 2)
t_mean <- lambda * t * E_Y1
t_variance <- lambda * t * E_Y1_square

# 打印结果
cat("估计的均值:", e_mean, "\n")
cat("理论的均值:", t_mean, "\n")
cat("估计的方差:", e_variance, "\n")
cat("理论的方差:", t_variance, "\n")

# 绘制直方图
hist(X_t, breaks = 30, col = "mediumseagreen", border = "black", main = "直方图 (复合泊松-伽马过程)",
     xlab = "X(10)", ylab = "频率")


## -----------------------------------------------------------------------------
# 设置种子
set.seed(1)

# 设置参数
lambda <- 2  # 泊松过程的参数 
alpha <- 2   # Gamma分布的形状参数
beta <- 1    # Gamma分布的尺度参数
t <- 10       # 时间点 X(t)

# 模拟次数
num <- 10000

# 模拟复合泊松-伽马过程
X_t <- numeric(num)

for (i in 1:num) {
  N_t <- rpois(1, lambda * t)
  if (N_t > 0) {
    Y <- rgamma(N_t, shape = alpha, rate = beta)
    X_t[i] <- sum(Y)
  } else {
    X_t[i] <- 0
  }
}

# 估计 X(t) 的均值和方差
e_mean <- mean(X_t)
e_variance <- var(X_t)

# 理论均值和方差
E_Y1 <- alpha / beta
E_Y1_square <- (alpha * (alpha + 1)) / (beta ^ 2)
t_mean <- lambda * t * E_Y1
t_variance <- lambda * t * E_Y1_square

# 打印结果
cat("估计的均值:", e_mean, "\n")
cat("理论的均值:", t_mean, "\n")
cat("估计的方差:", e_variance, "\n")
cat("理论的方差:", t_variance, "\n")

# 绘制直方图
hist(X_t, breaks = 30, col = "mediumseagreen", border = "black", main = "直方图 (复合泊松-伽马过程)",
     xlab = "X(10)", ylab = "频率")


## ----warning=FALSE------------------------------------------------------------
beta_cdf <- function(x, n = 100000) {
  # 生成n_sim个Beta(3, 3)分布的随机样本
  beta_samples <- rbeta(n, 3, 3)
  
  # 计算CDF的Monte Carlo估计
  cdf_estimate <- mean(beta_samples <= x)
  
  return(cdf_estimate)
}

## -----------------------------------------------------------------------------
x_values <- seq(0.1, 0.9, by = 0.1)
mc_estimates <- sapply(x_values,beta_cdf)

## -----------------------------------------------------------------------------
pbeta_values <- pbeta(x_values, 3, 3)
comparison <- data.frame(x = x_values, 
                         Monte_Carlo_Estimate = mc_estimates, 
                         pbeta_Value = pbeta_values)

print(comparison)

## -----------------------------------------------------------------------------
generate_rayleigh <- function(n, sigma) {
  U <- runif(n)  # 生成n个均匀分布的随机数
  X <- sigma * sqrt(-2 * log(U))  # 生成n个Rayleigh分布的随机数
  return(X)
}

## -----------------------------------------------------------------------------
generate_rayleigh_antithetic <- function(n, sigma) {
  U <- runif(n / 2)  # 生成一半的均匀分布随机数
  U_antithetic <- 1 - U  # 对偶变量
  X1 <- sigma * sqrt(-2 * log(U))  # 原始样本
  X2 <- sigma * sqrt(-2 * log(U_antithetic))  # 对偶样本
  return(list(X1, X2))  
}

## -----------------------------------------------------------------------------
# 设置参数
set.seed(1)
n <- 10000  # 样本数量
sigma <- 1  # Rayleigh分布的参数

# 生成独立的Rayleigh分布样本
X1 <- generate_rayleigh(n, sigma)
X2 <- generate_rayleigh(n, sigma)

X_antithetic <- generate_rayleigh_antithetic(n, sigma)

var1 <- var((X1+X2)/2)

var2 <- var((X_antithetic[[1]]+X_antithetic[[2]])/2)

percent_reduction <- 100 * (var1 - var2) / var1
cat("方差减少百分比为:", percent_reduction, "%\n")

## -----------------------------------------------------------------------------
set.seed(1)

# 定义目标函数 g(x)
g <- function(x) {
  (x^2 / sqrt(2 * pi)) * exp(-x^2 / 2)
}

# 修改后的指数分布作为 f1，定义域在 (1, ∞)
f1_density <- function(x, lambda = 1) {
  ifelse(x >= 1, lambda * exp(-lambda * (x - 1)), 0)
}

# 标准正态分布的 |X|+1 作为 f2，定义域在 (1, ∞)
f2_density <- function(x) {
  ifelse(x >= 1, dnorm(x - 1) + dnorm(-(x - 1)), 0)
}

# 采样 f1
f1_sample <- function(n, lambda = 1) {
  rexp(n, rate = lambda) + 1
}

# 采样 f2
f2_sample <- function(n) {
  abs(rnorm(n)) + 1
}

# 重要性采样估计积分
importance_sampling <- function(n, f_sample, f_density) {
  x <- f_sample(n)
  h_x <- g(x)
  weights <- h_x / f_density(x)
  return(list(var=var(weights),mean=mean(weights)))
}

# 设置采样数量
n <- 10000

# 对 f1 进行重要性采样估计
estimate_f1 <- importance_sampling(n, f1_sample, f1_density)
cat("使用 f1 (X+1，X为指数分布) 的估计值：", estimate_f1$mean, "\n")
cat("使用 f1 (X+1，X为指数分布) 的方差：", estimate_f1$var, "\n")
# 对 f2 进行重要性采样估计
estimate_f2 <- importance_sampling(n, f2_sample, f2_density)
cat("使用 f2 (|X|+1，X为标准正态分布) 的估计值：", estimate_f2$mean, "\n")
cat("使用 f2 (|X|+1，X为标准正态分布) 的方差：", estimate_f2$var, "\n")

## ----warning=FALSE------------------------------------------------------------
# 加载必要的包
library(ggplot2)

# 定义目标函数 g(x)
g <- function(x) {
  (x^2 / sqrt(2 * pi)) * exp(-x^2 / 2)
}

# 修改后的指数分布作为 f1，定义域在 (1, ∞)
f1_density <- function(x, lambda = 1) {
  ifelse(x >= 1, lambda * exp(-lambda * (x - 1)), 0)
}

# 标准正态分布的 |X|+1 作为 f2，定义域在 (1, ∞)
f2_density <- function(x) {
  ifelse(x >= 1, dnorm(x - 1) + dnorm(-(x - 1)), 0)
}

# 生成绘图范围
x_vals <- seq(1, 10, length.out = 1000)

# 计算密度值
g_vals <- g(x_vals)
f1_vals <- f1_density(x_vals)
f2_vals <- f2_density(x_vals)

# 创建数据框用于绘图
data <- data.frame(
  x = rep(x_vals, 3),
  density = c(g_vals, f1_vals, f2_vals),
  function_type = rep(c("g(x)", "f1", "f2"), each = length(x_vals))
)

# 绘图
ggplot(data, aes(x = x, y = density, color = function_type)) +
  geom_line(size = 1) +
  labs(title = "Density Functions: g(x), f1, and f2",
       x = "x", y = "Density") +
  scale_color_manual(values = c("g(x)" = "blue", "f1" = "red", "f2" = "green")) +
  theme_minimal()


## -----------------------------------------------------------------------------
library(ggplot2)

# 定义快速排序函数
quick_sort <- function(x) {
  if (length(x) <= 1) {
    return(x)
  } else {
    pivot <- x[1]  # 选择第一个元素作为基准
    less <- x[x < pivot]  # 小于基准的元素
    greater <- x[x > pivot]  # 大于基准的元素
    equal <- x[x == pivot]  # 等于基准的元素
    return(c(quick_sort(less), equal, quick_sort(greater)))  # 递归排序
  }
}

# 定义 n 的值
n_values <- c(10^4, 2 * 10^4, 4 * 10^4, 6 * 10^4, 8 * 10^4)

# 设置重复次数
num_simulations <- 100

# 初始化存储平均排序时间的向量
a_n <- numeric(length(n_values))

# 测量排序时间的函数
measure_sort_time <- function(n) {
  times <- numeric(num_simulations)
  for (i in 1:num_simulations) {
    x <- sample(1:n)  # 生成随机排列
    start_time <- Sys.time()
    sorted_x <- quick_sort(x)  # 使用自定义的快速排序
    end_time <- Sys.time() 
    times[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))  # 记录时间
  }
  mean(times)  # 返回平均时间
}

# 计算每个 n 的平均排序时间
for (i in seq_along(n_values)) {
  n <- n_values[i]
  a_n[i] <- measure_sort_time(n)
  cat("n =", n, "的平均排序时间:", a_n[i], "秒\n")
}

# 计算 tn = n * log(n)
t_n <- n_values * log(n_values)


# 绘制散点图和回归线
data <- data.frame(n_values = n_values, t_n = t_n, a_n = a_n)

ggplot(data, aes(x = t_n, y = a_n)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(title = "快速排序计算时间与 n log(n) 的关系",
       x = "n * log(n)",
       y = "平均排序时间 (秒)") +
  theme_minimal()


## -----------------------------------------------------------------------------
# 设置参数
n <- 10000  # 样本大小
m <- 10000  # Monte Carlo模拟次数
set.seed(1)  # 固定随机种子

# 定义计算偏度的函数
skewness <- function(x) {
  n <- length(x)
  mean_x <- mean(x)
  s3 <- mean((x - mean_x)^3)
  s2 <- mean((x - mean_x)^2)^(3/2)
  return(s3 / s2)
}

# Monte Carlo模拟
skew_values <- numeric(m)  # 存储偏度的结果
for (i in 1:m) {
  sample_data <- rnorm(n)  # 从标准正态分布中生成样本
  skew_values[i] <- skewness(sample_data)
}

## -----------------------------------------------------------------------------
quantiles <- quantile(skew_values, c(0.025, 0.05, 0.95, 0.975))
cat("估计的分位数为：\n")
print(quantiles)

var_formula <- sqrt(6/n)
cat("理论分位数为：\n")
theoretical_quantiles <- qnorm(c(0.025, 0.05, 0.95, 0.975), mean = 0, sd = var_formula)
print(theoretical_quantiles)

## -----------------------------------------------------------------------------
q <- c(0.025, 0.05, 0.95, 0.975)
skew_se_values <- sqrt(q*(1-q)/(n*(dnorm(qnorm(c(0.025, 0.05, 0.95, 0.975), mean = 0, sd = var_formula),mean = 0,sd = var_formula)^2)))
print(skew_se_values)

## -----------------------------------------------------------------------------
# 加载必要的包
library(MASS)  # 用于生成双变量正态分布数据
library(ggplot2)

# 设置随机数种子
set.seed(123)

mu <- c(0, 0)  # 均值向量
Sigma <- matrix(c(1, 0.01, 0.01, 1), 2, 2)  # 协方差矩阵
n <- 100  # 样本大小



set.seed(123)
n_sim <- 10000  # 模拟次数
p_vals_pearson <- numeric(n_sim)
p_vals_spearman <- numeric(n_sim)
p_vals_kendall <- numeric(n_sim)

for (i in 1:n_sim) {
  data_sim <- mvrnorm(n, mu, Sigma)
  p_vals_pearson[i] <- cor.test(data_sim[,1], sqrt(abs(data_sim[,2])), method = "pearson")$p.value
  p_vals_spearman[i] <- cor.test(data_sim[,1], sqrt(abs(data_sim[,2])), method = "spearman")$p.value
  p_vals_kendall[i] <- cor.test(data_sim[,1], sqrt(abs(data_sim[,2])), method = "kendall")$p.value
}

# 计算拒绝原假设的比例（即功效）
alpha <- 0.05
power_pearson <- mean(p_vals_pearson < alpha)
power_spearman <- mean(p_vals_spearman < alpha)
power_kendall <- mean(p_vals_kendall < alpha)

cat("\n功效比较：\n")
cat("Pearson检验功效：", power_pearson, "\n")
cat("Spearman检验功效：", power_spearman, "\n")
cat("Kendall检验功效：", power_kendall, "\n")


## -----------------------------------------------------------------------------
# 两个方法的统计功效
power1 <- 0.651
power2 <- 0.676

# 实验次数
n <- 10000

# 计算合并后的功效（pooled proportion）
pooled_power <- (power1 + power2) / 2

# 计算两比例差异的标准误
se_diff <- sqrt(pooled_power * (1 - pooled_power) * (2 / n))

# Z检验统计量
z_stat <- (power1 - power2) / se_diff

# 计算双尾检验的p值
p_value <- 2 * (1 - pnorm(abs(z_stat)))

# 输出结果
cat("Z检验统计量:", z_stat, "\n")
cat("p值:", p_value, "\n")

# 在0.05显著性水平下判断是否有显著差异
if (p_value < 0.05) {
  cat("在0.05显著性水平上，两者功效有显著差异。\n")
} else {
  cat("在0.05显著性水平上，两者功效没有显著差异。\n")
}


## -----------------------------------------------------------------------------
set.seed(1)  # 设置随机种子
N <- 1000  # 总假设数量
null_hypotheses <- 950  # 零假设数量
alt_hypotheses <- 50  # 备择假设数量
alpha <- 0.1  # 显著性水平
m <- 10000  # 模拟次数

#初始化
bonferroni_fwer <- numeric(m)
bonferroni_fdr <- numeric(m)
bonferroni_tpr <- numeric(m)
bh_fwer <- numeric(m)
bh_fdr <- numeric(m)
bh_tpr <- numeric(m)


for (i in 1:m) {
  # 生成p值
  p_values <- c(runif(null_hypotheses), rbeta(alt_hypotheses, 0.1, 1))
  
  # Bonferroni调整
  bonferroni_adjusted <- p.adjust(p_values, method = "bonferroni")
  
  # Benjamini-Hochberg调整
  bh_adjusted <- p.adjust(p_values, method = "BH")
  
  #分别计算两种调整下的FWER, FDR, TPR
  bonferroni_fwer[i] <- ifelse(sum(bonferroni_adjusted[1:null_hypotheses] < alpha)>=1,1,0)
  
  bh_fwer[i] <- ifelse(sum(bh_adjusted[1:null_hypotheses] < alpha)>=1,1,0)
  
  bonferroni_fdr[i] <- sum(bonferroni_adjusted[1:null_hypotheses] < alpha)/sum(bonferroni_adjusted[1:1000] < alpha)
  
  bh_fdr[i] <- sum(bh_adjusted[1:null_hypotheses] < alpha)/sum(bonferroni_adjusted[1:1000] < alpha)
  
  bonferroni_tpr[i] <- sum(bonferroni_adjusted[(null_hypotheses+1):1000] < alpha)/50
  
  bh_tpr[i] <- sum(bh_adjusted[(null_hypotheses+1):1000] < alpha)/50
}

#构造结果矩阵
results <- matrix(c(mean(bonferroni_fwer),mean(bonferroni_fdr),mean(bonferroni_tpr),mean(bh_fwer),mean(bh_fdr),mean(bh_tpr)),nrow = 3,ncol = 2)
colnames(results) <- c( "Bonferroni correction", "B-H correction")
rownames(results) <- c("FWER", "FDR","TPR")
results

## -----------------------------------------------------------------------------
# 加载boot包并查看数据
library(boot)
data("aircondit")

# 取出数据
fail_times <- aircondit$hours

# MLE估计
# 在指数分布中，lambda的最大似然估计值是1/均值
lambda_mle <- 1 / mean(fail_times)
cat("MLE of lambda:", lambda_mle, "\n")

# 定义自助法函数
bootstrap_mle <- function(data, indices) {
  sample_data <- data[indices]
  return(1 / mean(sample_data))  # 重新计算样本的lambda估计值
}

# 使用boot包中的boot函数执行自助法
boot_results <- boot(data = fail_times, statistic = bootstrap_mle, R = 1000)

# 计算偏差和标准误差
lambda_bias <- mean(boot_results$t) - lambda_mle
lambda_se <- sd(boot_results$t)

cat("Bootstrap bias estimate:", lambda_bias, "\n")
cat("Bootstrap standard error estimate:", lambda_se, "\n")


## -----------------------------------------------------------------------------
# 加载必要的库
library(boot)
data <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)

# 定义一个函数，用于计算平均故障时间 1/λ
mean_time <- function(data, indices) { 
  return(mean(data[indices]))  # 返回 1/λ 即平均时间
}

# 使用 bootstrapping 方法进行估计
set.seed(123)  # 确保结果可重复
bootstrap_results <- boot(data, statistic = mean_time, R = 10000)

# 计算不同方法的95%置信区间
ci_normal <- boot.ci(bootstrap_results, type = "norm")
ci_basic <- boot.ci(bootstrap_results, type = "basic")
ci_percentile <- boot.ci(bootstrap_results, type = "perc")
ci_bca <- boot.ci(bootstrap_results, type = "bca")

# 输出结果
cat("对于平均时间的95%置信区间为：\n")
cat("Normal Method CI: ", ci_normal$normal[2], "to", ci_normal$normal[3], "\n")
cat("Basic Method CI: ", ci_basic$basic[4], "to", ci_basic$basic[5], "\n")
cat("Percentile Method CI: ", ci_percentile$percent[4], "to", ci_percentile$percent[5], "\n")
cat("BCa Method CI: ", ci_bca$bca[4], "to", ci_bca$bca[5], "\n")

## -----------------------------------------------------------------------------
# 加载所需数据集
library(bootstrap)
scores_data <- scor

# 计算总体的第一个特征值占比
cov_eigenvalues <- eigen(cov(scores_data))$values
initial_theta <- cov_eigenvalues[1] / sum(cov_eigenvalues)

# Jackknife估计
num_samples <- nrow(scores_data)
theta_jackknife <- vector(length = num_samples)

for (sample_index in seq_len(num_samples)) {
  # 排除当前样本并计算协方差矩阵
  reduced_cov <- cov(scores_data[-sample_index, ])
  jackknife_eigenvalues <- eigen(reduced_cov)$values
  
  # 计算特征值比例
  theta_jackknife[sample_index] <- jackknife_eigenvalues[1] / sum(jackknife_eigenvalues)
}

# 计算Jackknife的偏差和标准误
jackknife_bias <- (num_samples - 1) * (mean(theta_jackknife) - initial_theta)
jackknife_se <- sqrt((num_samples - 1) * mean((theta_jackknife - mean(theta_jackknife))^2))

# 输出结果
cat("Jackknife偏差估计:", jackknife_bias, "\n")
cat("Jackknife标准误估计:", jackknife_se, "\n")


## -----------------------------------------------------------------------------
# 加载数据
library(DAAG)
attach(ironslag)
num_samples <- length(magnetic)

# 初始化误差向量
errors_model1 <- errors_model2 <- errors_model3 <- errors_model4 <- numeric(num_samples)

# n 折交叉验证
for (i in 1:num_samples) {
  response <- magnetic[-i]
  predictor <- chemical[-i]
  
  # 模型1: 线性模型
  model1 <- lm(response ~ predictor)
  pred1 <- model1$coef[1] + model1$coef[2] * chemical[i]
  errors_model1[i] <- magnetic[i] - pred1
  
  # 模型2: 二次回归模型
  model2 <- lm(response ~ predictor + I(predictor^2))
  pred2 <- model2$coef[1] + model2$coef[2] * chemical[i] + model2$coef[3] * chemical[i]^2
  errors_model2[i] <- magnetic[i] - pred2
  
  # 模型3: 指数模型
  model3 <- lm(log(response) ~ predictor)
  log_pred3 <- model3$coef[1] + model3$coef[2] * chemical[i]
  pred3 <- exp(log_pred3)
  errors_model3[i] <- magnetic[i] - pred3
  
  # 模型4: 三次回归模型
  model4 <- lm(response ~ predictor + I(predictor^2) + I(predictor^3))
  pred4 <- model4$coef[1] + model4$coef[2] * chemical[i] + model4$coef[3] * chemical[i]^2 + model4$coef[4] * chemical[i]^3
  errors_model4[i] <- magnetic[i] - pred4
}

# 计算每个模型的平均平方误差
mse_results <- c(mean(errors_model1^2), mean(errors_model2^2), mean(errors_model3^2), mean(errors_model4^2))
cat("各模型的均方误差为:", mse_results, "\n")

# 比较调整后的 R^2
response_var <- magnetic
predictor_var <- chemical

adj_r2_values <- c(
  summary(lm(response_var ~ predictor_var))$adj.r.squared,
  summary(lm(response_var ~ predictor_var + I(predictor_var^2)))$adj.r.squared,
  summary(lm(log(response_var) ~ predictor_var))$adj.r.squared,
  summary(lm(response_var ~ predictor_var + I(predictor_var^2) + I(predictor_var^3)))$adj.r.squared
)
cat("各模型的调整后 R^2 值为:", adj_r2_values, "\n")


## -----------------------------------------------------------------------------
# 加载数据
attach(chickwts)
group1 <- sort(weight[feed == "soybean"])
group2 <- sort(weight[feed == "linseed"])
detach(chickwts)

# 定义 Cramér-von Mises 检验的统计量函数
compute_cvm <- function(group1, group2) {
  n1 <- length(group1)
  n2 <- length(group2)
  combined_samples <- sort(c(group1, group2))
  ecdf1 <- ecdf(group1)(combined_samples)
  ecdf2 <- ecdf(group2)(combined_samples)
  
  # 计算统计量
  statistic <- (n1 * n2 / (n1 + n2)^2) * sum((ecdf1 - ecdf2)^2)
  return(statistic)
}

# 计算原始数据的统计量
observed_stat <- compute_cvm(group1, group2)

# 置换检验的设置
set.seed(123)  # 保证结果可重复
num_permutations <- 1000
perm_stats <- numeric(num_permutations)

for (j in 1:num_permutations) {
  shuffled_data <- sample(c(group1, group2))
  perm_group1 <- shuffled_data[1:length(group1)]
  perm_group2 <- shuffled_data[(length(group1) + 1):length(shuffled_data)]
  perm_stats[j] <- compute_cvm(perm_group1, perm_group2)
}

# 计算 p 值
p_value <- mean(c(observed_stat, perm_stats) >= observed_stat)
cat("Cramér-von Mises 置换检验的 p 值:", p_value, "\n")

# 绘制置换分布的直方图
hist(perm_stats, main = "", freq = FALSE, breaks = "scott")
points(observed_stat, 0, cex = 1, pch = 16)  # 标出观测到的统计量


## -----------------------------------------------------------------------------
set.seed(1)
x <- rnorm(50)
y <- rnorm(50)

# 使用 cor 函数计算 Spearman 秩相关系数
observed_stat <- cor(x, y, method = "spearman")

# 设置置换检验参数
num_permutations <- 1000
perm_stats <- numeric(num_permutations)

# 置换检验
for (i in 1:num_permutations) {
  perm_y <- sample(y)  
  perm_stats[i] <- cor(x, perm_y, method = "spearman")
}

# 计算置换检验的 p 值
p_value_perm <- mean(abs(perm_stats) >= abs(observed_stat))
cat("置换检验的 p 值:", p_value_perm, "\n")

# 使用 cor.test 函数计算 Spearman 检验的 p 值
test_result <- cor.test(x, y, method = "spearman")
cat("cor.test 的 p 值:", test_result$p.value, "\n")

# 绘制置换检验统计量的直方图
hist(perm_stats, main = "置换检验统计量分布", freq = FALSE, breaks = "scott")
abline(v = observed_stat, col = "red", lwd = 2) 


## -----------------------------------------------------------------------------
# 设置参数
set.seed(123)
n <- 10000  # 生成样本数
burn_in <- 1000 # 丢弃的前1000个样本
x <- numeric(n)  # 存储生成的样本
x[1] <- 0  # 初始值

# Metropolis-Hastings 采样
for (i in 2:n) {
  # 使用正态分布作为提议分布
  proposal <- x[i - 1] + rnorm(1, mean = 0, sd = 1)
  
  # 计算接受率
  alpha <- dcauchy(proposal, location = 0, scale = 1) / dcauchy(x[i - 1], location = 0, scale = 1)
  
  # 接受或拒绝
  if (runif(1) < alpha) {
    x[i] <- proposal
  } else {
    x[i] <- x[i - 1]
  }
}

# 丢弃前1000个样本
samples <- x[(burn_in + 1):n]

# 计算生成样本的分位数
sample_deciles <- quantile(samples, probs = seq(0.1, 0.9, by = 0.1))

# 计算标准柯西分布的分位数
true_deciles <- qcauchy(seq(0.1, 0.9, by = 0.1))

# 对比结果
result <- data.frame(Sample_Deciles = sample_deciles, True_Deciles = true_deciles)

print(result)

## -----------------------------------------------------------------------------
# 设置参数
set.seed(123)
n <- 10      # 二项分布的样本数量
a <- 2       # Beta 分布的参数 a
b <- 2       # Beta 分布的参数 b
iterations <- 10000  # Gibbs 采样的总迭代次数

# 初始化向量
x <- numeric(iterations)
y <- numeric(iterations)
y[1] <- 0.5  # 给定初始值

# Gibbs 采样过程
for (i in 2:iterations) {
  # 给定 y[i - 1] 采样
  x[i] <- rbinom(1, n, y[i - 1])
  
  # 给定 x[i] 采样
  y[i] <- rbeta(1, x[i] + a, n - x[i] + b)
}

burn <- 1000
x_samples <- x[(burn + 1):iterations]
y_samples <- y[(burn + 1):iterations]

# 绘图：展示采样结果
par(mfrow = c(1, 2))
plot(x_samples, y_samples, pch = 20, col = "blue", 
     main = "Gibbs 采样生成的 (x, y) 样本点", xlab = "x", ylab = "y")
hist(y_samples, probability = TRUE, col = "lightblue", 
     main = "y 的边际分布", xlab = "y")


## -----------------------------------------------------------------------------
# 设置参数
set.seed(123)
chains <- 3
n <- 10000
burn_in <- 1000
samples_list <- list()

for (j in 1:chains) {
  x <- numeric(n)
  x[1] <- 0
  
  for (i in 2:n) {
    proposal <- x[i - 1] + rnorm(1, mean = 0, sd = 1)
    alpha <- dcauchy(proposal, location = 0, scale = 1) / dcauchy(x[i - 1], location = 0, scale = 1)
    if (runif(1) < alpha) {
      x[i] <- proposal
    } else {
      x[i] <- x[i - 1]
    }
  }
  
  samples_list[[j]] <- x[(burn_in + 1):n]
}

# Gelman-Rubin 手动计算
m <- chains
n_samples <- n - burn_in
chain_means <- sapply(samples_list, mean)
overall_mean <- mean(chain_means)
chain_vars <- sapply(samples_list, var)

# 计算链内方差 W
W <- mean(chain_vars)

# 计算链间方差 B
B <- n_samples * var(chain_means)

# 计算总方差估计 V
V <- ((n_samples - 1) / n_samples) * W + (1 / n_samples) * B

# 计算 Gelman-Rubin 统计量
R_hat <- sqrt(V / W)

cat("Gelman-Rubin 统计量 (R_hat) 为:", R_hat, "\n")


## -----------------------------------------------------------------------------
# 设置参数
set.seed(123)
n <- 10
a <- 2
b <- 2
chains <- 3
iterations <- 10000
burn_in <- 1000
y_initial <- 0.5
samples_list_x <- list()
samples_list_y <- list()

for (j in 1:chains) {
  x <- numeric(iterations)
  y <- numeric(iterations)
  y[1] <- y_initial
  
  for (i in 2:iterations) {
    x[i] <- rbinom(1, n, y[i - 1])
    y[i] <- rbeta(1, x[i] + a, n - x[i] + b)
  }
  
  samples_list_x[[j]] <- x[(burn_in + 1):iterations]
  samples_list_y[[j]] <- y[(burn_in + 1):iterations]
}

# Gelman-Rubin 手动计算 X 的 R_hat
chain_means_x <- sapply(samples_list_x, mean)
overall_mean_x <- mean(chain_means_x)
chain_vars_x <- sapply(samples_list_x, var)
W_x <- mean(chain_vars_x)
B_x <- n_samples * var(chain_means_x)
V_x <- ((n_samples - 1) / n_samples) * W_x + (1 / n_samples) * B_x
R_hat_x <- sqrt(V_x / W_x)

cat("Gelman-Rubin 统计量 X (R_hat) 为:", R_hat_x, "\n")

# Gelman-Rubin 手动计算 Y 的 R_hat
chain_means_y <- sapply(samples_list_y, mean)
overall_mean_y <- mean(chain_means_y)
chain_vars_y <- sapply(samples_list_y, var)
W_y <- mean(chain_vars_y)
B_y <- n_samples * var(chain_means_y)
V_y <- ((n_samples - 1) / n_samples) * W_y + (1 / n_samples) * B_y
R_hat_y <- sqrt(V_y / W_y)

cat("Gelman-Rubin 统计量 Y (R_hat) 为:", R_hat_y, "\n")


## -----------------------------------------------------------------------------
f <- function(x) log(x+log(x))/log(1+x)
curve(f,3,10)
res <- optimize(f,lower=4,upper=8,maximum=FALSE)
res


## -----------------------------------------------------------------------------
compute_kth_term <- function(k, a) {
  # 计算 Euclidean 范数
  norm_a <- sqrt(sum(a^2))
  d <- length(a)
  
  if (is.double(k)==TRUE){
  #为了防止k过大导致gamma函数值和幂值过大，先取log
  log_gamma_part <-lgamma((d+1)/2)+lgamma(k+3/2)-lgamma(k+d/2+1)-lgamma(k+1)
  log_norm_part <- k*log(norm_a^2/2)
  # 返回第 k 项
  return((-1)^k *(norm_a^2)* exp(log_norm_part+log_gamma_part-log(2*k+1)-log(2*k+2)))}
  else {return("error")}
}
compute_kth_term(1000,c(1,2))

## -----------------------------------------------------------------------------
compute_sum <- function(a, tol = 1e-20, max_iter = 1000) {
  sum_value <- 0
  d <- length(a)
  k <- 0
  
  repeat {
    term <- compute_kth_term(k, a)
    sum_value <- sum_value + term
    
    # 检查是否满足收敛条件
    if (abs(term) < tol || k >= max_iter) break
    
    k <- k + 1
  }
  
  return(sum_value)
}


## -----------------------------------------------------------------------------
compute_sum(c(1,2))

## -----------------------------------------------------------------------------
# 定义 c_k 函数
c_k <- function(a, k) {
  sqrt(a^2 * k / (k + 1 - a^2))
}

# 定义方程左边的积分和表达式
lhs <- function(a, k) {
  ck_minus_1 <- c_k(a, k - 1)
  integrand_lhs <- function(u) (1 + u^2 / (k - 1))^(-k / 2)
  
  integral_lhs <- integrate(integrand_lhs, 0, ck_minus_1)$value
  
  2 * exp(lgamma(k / 2)-lgamma((k - 1) / 2)) / sqrt(pi * (k - 1))  * integral_lhs
}

# 定义方程右边的积分和表达式
rhs <- function(a, k) {
  ck <- c_k(a, k)
  integrand_rhs <- function(u) (1 + u^2 / k)^(-(k + 1) / 2)
  
  integral_rhs <- integrate(integrand_rhs, 0, ck)$value
  2 * exp(lgamma((k+1) / 2)-lgamma(k/ 2)) / sqrt(pi * k) * integral_rhs
}

# 定义方程的差值（左右两边差）
equation <- function(a, k=100) {
  lhs(a, k) - rhs(a, k)
}

# 给定 k 值求解 a
solve_for_a <- function(k, interval = c(0.1,min(3,sqrt(k)-1e-5))) {
  uniroot(equation, interval = interval, k = k)$root
}

# 示例：在不同的 k 值下求解 a 并输出结果
k_values <- c(4:25,100,500,1000)  # 可以根据需要更改 k 的范围
solutions <- sapply(k_values, solve_for_a)

# 打印结果
for (i in seq_along(k_values)) {
  cat("当 k =", k_values[i], "时，a 的解为:", solutions[i], "\n")
}


## -----------------------------------------------------------------------------
# 定义 S_{k-1}(a) 函数
S_k_minus_1 <- function(a, k) {
  threshold <- sqrt(a^2 * (k - 1) / (k - a^2))
  1 - pt(threshold, df = k - 1)
}

# 定义 S_k(a) 函数
S_k <- function(a, k) {
  threshold <- sqrt(a^2 * k / (k + 1 - a^2))
  1 - pt(threshold, df = k)
}

# 定义交点方程的差值
equation <- function(a, k) {
  S_k_minus_1(a, k) - S_k(a, k)
}

# 求解 a 使得 S_{k-1}(a) = S_k(a)
solve_for_Ak <- function(k, interval = c(0.01, min(3,sqrt(k)-1e-5))) {
  uniroot(equation, interval = interval, k = k)$root
}

# 在不同的 k 值下找到交点 A(k)
k_values <- c(4:25, 100, 500, 1000)
solutions <- sapply(k_values, solve_for_Ak)

# 输出结果
for (i in seq_along(k_values)) {
  cat("当 k =", k_values[i], "时，交点 A(k) 的解为:", solutions[i], "\n")
}


## -----------------------------------------------------------------------------
# 初始化数据
observed_data <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)
tau <- 1  # 截尾点
n <- length(observed_data)

# E-M 算法
estimate_lambda_em <- function(data, tau, tol = 1e-6, max_iter = 1000) {
  # 初始化 lambda
  lambda <- 1 / mean(data)  # 初始值
  
  for (i in 1:max_iter) {
    # E 步骤: 计算每个被截尾值的期望
    truncated_data <- data[data == tau]  # 被截尾的值
    untruncated_data <- data[data < tau]  # 未截尾的值
    
    # 对于截尾的观测值，计算其条件期望
    expected_truncated <- sum(truncated_data + 1 / lambda)
    
    # M 步骤: 更新 lambda
    lambda_new <- n / (sum(untruncated_data) + expected_truncated)
    
    # 检查收敛
    if (abs(lambda - lambda_new) < tol) break
    lambda <- lambda_new
  }
  
  return(lambda)
}

# 计算 EM 算法的 lambda
lambda_em <- estimate_lambda_em(observed_data, tau)

# 使用观测数据直接计算 MLE
observed_untruncated <- observed_data[observed_data < tau]
lambda_mle <- 1 / mean(observed_untruncated)

# 输出结果
cat("EM 算法估计的 λ:", lambda_em, "\n")
cat("使用观测数据直接计算的 MLE λ:", lambda_mle, "\n")


## -----------------------------------------------------------------------------
library(boot) #for simplex function
A1 <- rbind(c(2, 1, 1), c(1, -1, 3))
b1 <- c(2, 3)
a <- c(4, 2, 9)
simplex(a = a, A1 = A1, b1 = b1, maxi = FALSE)

## -----------------------------------------------------------------------------
# 定义公式列表
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

# 初始化一个空列表存储模型
models_for_1 <- list()

# 使用for循环拟合模型
for (i in seq_along(formulas)) {
  models_for_1[[i]] <- lm(formulas[[i]], data = mtcars)
}

# 查看结果
models_for_1

## -----------------------------------------------------------------------------
# 使用lapply()拟合模型
models_lapply_1 <- lapply(formulas, function(f) lm(f, data = mtcars))

# 查看结果
models_lapply_1


## -----------------------------------------------------------------------------
set.seed(1)
# 创建bootstrap样本列表
bootstraps <- list()
for (i in 1:10) {
  rows <- sample(1:nrow(mtcars), replace = TRUE)
  bootstraps[[i]] <- mtcars[rows, ]
}

models_for_2 <- list()

# 对每个bootstrap样本拟合模型
for (i in seq_along(bootstraps)) {
  models_for_2[[i]] <- lm(mpg ~ disp, data = bootstraps[[i]])
}

# 查看结果
models_for_2


## -----------------------------------------------------------------------------
set.seed(1)
# 定义一个函数生成bootstrap样本
generate_bootstrap <- function(index) {
  rows <- sample(1:nrow(mtcars), replace = TRUE)
  mtcars[rows, ]
}

# 使用lapply()生成bootstrap样本
bootstraps <- lapply(1:10, generate_bootstrap)

# 定义一个函数拟合模型
fit_model <- function(data) {
  lm(mpg ~ disp, data = data)
}

# 使用lapply()对每个bootstrap样本拟合模型
models_lapply_2 <- lapply(bootstraps, fit_model)

# 查看结果
models_lapply_2


## -----------------------------------------------------------------------------
r_lapply_1 <- lapply(models_lapply_1, function(mod) summary(mod)$r.squared)

unlist(r_lapply_1)

## -----------------------------------------------------------------------------
r_lapply_2 <- lapply(models_lapply_2, function(mod) summary(mod)$r.squared)

unlist(r_lapply_2)

## -----------------------------------------------------------------------------
set.seed(1)
# 模拟100次t检验
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)

# 使用sapply()和匿名函数提取p值
p_values <- sapply(trials, function(x) x$p.value)

# 查看部分结果
head(p_values)


## -----------------------------------------------------------------------------
set.seed(1)
# 使用sapply()直接提取p值
p_values_direct <- sapply(trials, `[[`, "p.value")

# 查看部分结果
head(p_values_direct)


## -----------------------------------------------------------------------------
Map_vapply <- function(FUN, ..., FUN.VALUE) {
  # 使用 Map() 对所有输入进行并行迭代
  results <- Map(FUN, ...)
  
  # 使用 vapply() 将结果强制转换为向量或矩阵
  vapply(results, identity, FUN.VALUE)
}


## -----------------------------------------------------------------------------
set.seed(1)
list1 <- replicate(100,rpois(10, 10),simplify = FALSE)
list2 <- replicate(100,rpois(7, 10),simplify = FALSE)
head(Map_vapply(FUN = function(x,y) t.test(x,y)$p.value,list1,list2,FUN.VALUE = numeric(1)))

## -----------------------------------------------------------------------------
fast_chisq_test <- function(x, y) {
  # 生成列联表（contingency table）
  tbl <- table(x, y)
  
  # 计算行总计和列总计
  row_sums <- rowSums(tbl)
  col_sums <- colSums(tbl)
  total <- sum(tbl)
  
  # 计算期望频数
  expected <- outer(row_sums, col_sums, "*") / total
  
  # 计算卡方统计量
  chisq_stat <- sum((tbl - expected)^2 / expected)
  
  return(chisq_stat)
}
x <- c(1,1,1,2,2,2,2,2,1,1)
y <- c(1,1,1,2,1,1,2,2,2,2)

# 使用快速卡方检验
fast_chisq_test(x, y)

## -----------------------------------------------------------------------------
fast_table <- function(x, y) {
  # 确定值的范围
  x_levels <- sort(unique(x))
  y_levels <- sort(unique(y))
  
  # 创建零初始化的频数矩阵
  tbl <- matrix(0, nrow = length(x_levels), ncol = length(y_levels),
                dimnames = list(x_levels, y_levels))
  
  # 填充频数矩阵
  for (i in seq_along(x)) {
    tbl[x[i], y[i]] <- tbl[x[i], y[i]] + 1
  }
  
  return(tbl)
}


## -----------------------------------------------------------------------------
fast_chisq_test1 <- function(x, y) {
  # 使用快速 table() 构造列联表
  tbl <- fast_table(x, y)
  
  # 计算行和、列和以及总和
  row_sums <- rowSums(tbl)
  col_sums <- colSums(tbl)
  total <- sum(tbl)
  
  # 计算期望频数矩阵
  expected <- outer(row_sums, col_sums, "*") / total
  
  # 计算卡方统计量
  chisq_stat <- sum((tbl - expected)^2 / expected)
  
  return(chisq_stat)
}


## -----------------------------------------------------------------------------
set.seed(123)
x <- sample(1:5, 1e5, replace = TRUE)
y <- sample(1:5, 1e5, replace = TRUE)
tbl <- table(x, y)
chisq_stat <- chisq.test(x, y)$statistic
chisq_stat_fast <- fast_chisq_test(x, y)
chisq_stat - chisq_stat_fast

## -----------------------------------------------------------------------------
gibbs_r <- function(n_samples, n, a, b, x_init, y_init) {
  samples <- matrix(0, nrow = n_samples, ncol = 2)
  x <- x_init
  y <- y_init
  
  for (i in 1:n_samples) {
    # Update x ~ Binomial(n, y)
    x <- rbinom(1, n, y)
    
    # Update y ~ Beta(x+a, n-x+b)
    y <- rbeta(1, x + a, n - x + b)
    
    # Store the samples
    samples[i, ] <- c(x, y)
  }
  
  return(samples)
}


## -----------------------------------------------------------------------------
# 参数设置
n_samples <- 1000
n <- 10
a <- 2
b <- 3
x_init <- 5
y_init <- 0.5

# 调用Rcpp和R函数
samples_rcpp <- gibbs_rcpp(n_samples, n, a, b, x_init, y_init)
samples_r <- gibbs_r(n_samples, n, a, b, x_init, y_init)

# QQ图比较x的分布
qqplot(samples_r[, 1], samples_rcpp[, 1], main = "QQ Plot of x")
abline(0, 1, col = "red")

# QQ图比较y的分布
qqplot(samples_r[, 2], samples_rcpp[, 2], main = "QQ Plot of y")
abline(0, 1, col = "red")


## -----------------------------------------------------------------------------
library(microbenchmark)

benchmark_results <- microbenchmark(
  Rcpp = gibbs_rcpp(n_samples, n, a, b, x_init, y_init),
  R = gibbs_r(n_samples, n, a, b, x_init, y_init),
  times = 100
)

print(benchmark_results)


