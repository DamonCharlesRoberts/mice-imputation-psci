// stan model

data {
    int<lower=0> N; //observations
    vector[N] x; // for each observation, there is variable x
    vector[N] z; // for each observation, there is variable z
    vector[N] y; // for each observation, there is variable y
}
parameters {
    real alpha; // intercept term
    real beta_1; // beta_1
    real beta_2; // beta_2
    real<lower=0> sigma; // non-negative std. dev. of error
}
model {
    y ~ normal(alpha + beta_1 * x + beta_2 * z, sigma); // likelihood
}