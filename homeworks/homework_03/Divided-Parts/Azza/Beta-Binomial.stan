data{
  int N;
  int y;
  real<lower=0> alpha;
  real<lower=0> beta;
}
parameters{
  real p;
}
model{
  target+=binomial_lpmf(y|N, p);
  target+=beta_lpdf(p|alpha,beta);
}
