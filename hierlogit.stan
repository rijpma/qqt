data{
    int<lower=1> K; # non-varying
    int<lower=1> D; # varying 
    int<lower=0> N; # obs
    int<lower=1> L; # levels
    int<lower=0, upper=1> enrol[N];
    int<lower=1, upper=L> smpl[N];
    row_vector[D] nsib[N];
    row_vector[K] x[N];
}
parameters{
    real mu[D];
    real<lower=0> sigma[D];
    vector[D] beta[L];
    vector[K] gamma;
    vector[L] a;
    real<lower=0, upper=10> sigma_a;
    real mu_a;
}
model{
    for (d in 1:D){
        mu[d] ~ normal(0, 100);
        for (l in 1:L){
            beta[l, d] ~ normal(mu[d], sigma[d]);
        }
    }
    mu_a ~ normal(0, 1); # 100?
    a ~ normal(mu_a, sigma_a);

    for (i in 1:N){
        enrol[i] ~ bernoulli_logit(a[smpl[i]] + nsib[i] * beta[smpl[i]] + x[i] * gamma);
    }
}