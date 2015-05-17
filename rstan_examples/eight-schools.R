require(rstan)

schools_code <- '
  data {
    int<lower=0> J; // サンプル数
    real y[J]; // コーチング効果
    real<lower=0> sigma[J]; // 標準偏差
  }

  parameters { // 推定するパラメータ
    real mu; // 定数
    real<lower=0> tau; // 個体差
    real eta[J]; // 個体差のバラツキ
  }

  transformed parameters { // ハイパーパラメータの情報
    real theta[J];
    for (j in 1:J)
      theta[j] <- mu + tau * eta[j]; // muとtauからthetaを算出
  }

  model {
    eta ~ normal(0, 1); // etaを標準正規分布で推定
    y ~ normal(theta, sigma); // yを平均(theta), 標準偏差(sigma)の正規分布で推定
  }
'
# schools_dat
# y : The observed coaching effect of each school
# se : The standard error of the coaching effect of each school.

# school y se
# A  28  15
# B   8  10
# C  -3  16
# D   7  11
# E  -1   9
# F   1  11
# G  18  10
# H  12  18

schools_dat <- list(J = 8,
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(model_code=schools_code, data=schools_dat, iter=1000, chains=4)
fit2 <- stan(fit=fit, data=schools_dat, iter=10000, chains=4)

print(fit2)
plot(fit2)

la <- extract(fit2, permuted=TRUE) # return a list of arrays
mu <- la$mu

### return an array of three dimensions: iterations, chains, parameters
a <- extract(fit2, permuted=FALSE)

### use S3 functions as.array (or as.matrix) on stanfit objects
a2 <- as.array(fit2)
m <- as.matrix(fit2)

traceplot(fit, ask=T)
print(fit, digits=1)
# Inference for Stan model: schools_code.
# 4 chains, each with iter=1000; warmup=500; thin=1;
# post-warmup draws per chain=500, total post-warmup draws=2000.
#
#          mean se_mean  sd  2.5%  25%  50%  75% 97.5% n_eff Rhat
# mu        7.8     0.3 5.4  -2.3  4.4  7.6 11.0  19.3   253    1
# tau       6.8     0.3 6.0   0.2  2.4  5.3  9.3  22.4   380    1
# eta[1]    0.4     0.0 0.9  -1.4 -0.2  0.4  1.0   2.2  1148    1
# eta[2]    0.0     0.0 0.9  -1.8 -0.6  0.0  0.6   1.7   854    1
# eta[3]   -0.2     0.0 0.9  -2.0 -0.9 -0.2  0.5   1.7  1197    1
# eta[4]    0.0     0.0 0.9  -1.6 -0.6  0.0  0.6   1.7  1161    1
# eta[5]   -0.3     0.0 0.9  -2.0 -0.9 -0.3  0.2   1.4   948    1
# eta[6]   -0.2     0.0 0.9  -1.9 -0.7 -0.2  0.4   1.5   856    1
# eta[7]    0.4     0.0 0.9  -1.5 -0.2  0.4  0.9   2.0  1456    1
# eta[8]    0.1     0.0 0.9  -1.9 -0.5  0.1  0.7   1.9  1248    1
# theta[1] 11.4     0.3 8.2  -2.4  6.2 10.2 15.6  31.3   903    1
# theta[2]  7.7     0.2 6.3  -4.6  3.7  7.7 11.7  20.6  1525    1
# theta[3]  6.0     0.2 7.8 -12.1  1.9  6.7 10.7  20.5  1296    1
# theta[4]  7.6     0.2 6.6  -7.3  4.0  7.8 11.4  21.1  1295    1
# theta[5]  4.9     0.2 6.6 -10.4  1.1  5.5  9.3  16.3   819    1
# theta[6]  5.9     0.2 6.6  -7.9  2.0  6.4 10.4  17.8  1451    1
# theta[7] 10.7     0.2 6.9  -1.6  6.0 10.0 14.5  25.9   898    1
# theta[8]  8.6     0.4 8.0  -5.9  3.7  8.1 12.9  26.6   476    1
# lp__     -4.8     0.1 2.6 -10.5 -6.4 -4.6 -3.0  -0.5   574    1
