data {
      int<lower=1> T;
      int<lower=0> d[T];
}

transformed data {
      real log_unif;
      log_unif <- -log(T);
}

parameters {
      real<lower=0> e;
      real<lower=0> l;
}

transformed parameters {
      vector[T] lp;
      lp <- rep_vector(log_unif, T);
      for (s in 1:T){
         for (t in 1:T){
          // パラメータ λ のポアソン分布に従うが s 時点で e, l と変化する
          lp[s] <- lp[s] + poisson_log(d[t], if_else(t < s, e, l));
         }
      }
}

model {
      e ~ uniform(0,100);
      l ~ uniform(0,100);
      increment_log_prob(log_sum_exp(lp));
}

generated quantities {
      int<lower=1,upper=T> s;
      s <- categorical_rng(softmax(lp));
}
