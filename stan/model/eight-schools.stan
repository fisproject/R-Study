data {
  int<lower=0> J; // サンプルサイズ
  real y[J]; // コーチング効果
  real<lower=0> sigma[J]; // 標準偏差
}

parameters {
  real mu; // 定数
  real<lower=0> tau; // eta[j] の係数
  real eta[J]; // 個体差
}

transformed parameters {
  real theta[J];
  for (j in 1:J)
    theta[j] <- mu + tau * eta[j]; // 個体ごとの平均
}

model {
  eta ~ normal(0, 1); // eta は標準正規分布に従う
  y ~ normal(theta, sigma); // y は N(theta, sigma)に従う
}
