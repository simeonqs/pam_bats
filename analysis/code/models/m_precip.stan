data{
  int N_obs;
  int N_stations;
  array[N_obs] int present;
  array[N_obs] real precip;
  array[N_obs] int station;
}
parameters{
  real a;
  real<lower=0> b;
  vector[N_stations] z_station;
  real<lower=0> sigma_station;
}
model{
  vector[N_obs] p;
  a ~ normal(-4, 2);
  b ~ exponential(1);
  z_station ~ normal(0, 1);
  sigma_station ~ exponential(1);
  for(i in 1:N_obs){
    p[i] = a - b * precip[i] + z_station[station[i]] * sigma_station;
    p[i] = inv_logit(p[i]);
  }
  present ~ binomial(1 , p);
}
