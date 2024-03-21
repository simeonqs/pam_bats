data{
  int N_obs;
  int N_knots;
  array[N_obs] int present;
  matrix[N_obs, N_knots+2] B;
}
parameters{
  real a;
  vector[N_knots+2] w;
}
model{
  vector[N_obs] p;
  w ~ normal(0, 1);
  a ~ normal(-2, 2);
  for(i in 1:N_obs){
    p[i] = a + B[i] * w;
    p[i] = inv_logit(p[i]);
  }
  present ~ binomial(1 , p);
}
