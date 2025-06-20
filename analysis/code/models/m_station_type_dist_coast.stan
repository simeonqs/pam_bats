data{
  int N_obs;
  int N_stations;
  array[N_obs] int present;
  array[N_obs] int station_type;
  array[N_obs] real dist_coast;
  array[N_obs] int station;
}
parameters{
  real a;
  vector[N_stations] z_station;
  real<lower=0> sigma_station;
  vector[3] z_type;
  real<lower=0> sigma_type;
  real<lower=0> b;
}
model{
  vector[N_obs] p;
  a ~ normal(-2, 2);
  z_station ~ normal(0, 1);
  sigma_station ~ exponential(2);
  z_type ~ normal(0, 1);
  sigma_type ~ exponential(1);
  b ~ exponential(10);
  for(i in 1:N_obs){
    p[i] = a + 
    z_station[station[i]] * sigma_station + 
    z_type[station_type[i]] * sigma_type -
    b * dist_coast[i];
    p[i] = inv_logit(p[i]);
  }
  present ~ binomial(1 , p);
}
generated quantities {
  vector[3] a_type;
  a_type = a + z_type * sigma_type;
}
