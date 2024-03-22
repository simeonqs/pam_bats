data{
  int N_obs;
  int N_knots;
  int N_stations;
  array[N_obs] int present;
  matrix[N_obs, N_knots+2] B;
  array[N_obs] int station;
}
parameters{
  real a;
  vector[N_knots+2] w;
  vector[N_stations] z_station;
  real<lower=0> sigma_station;
}
model{
  vector[N_obs] p;
  a ~ normal(-2, 2);
  w ~ normal(0, 1);
  z_station ~ normal(0, 1);
  sigma_station ~ exponential(1);
  for(i in 1:N_obs){
    p[i] = a + B[i] * w + z_station[station[i]] * sigma_station;
    p[i] = inv_logit(p[i]);
  }
  present ~ binomial(1 , p);
}
