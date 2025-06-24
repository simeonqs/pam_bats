data{
  int N_obs;
  int N_knots;
  int N_stations;
  array[N_obs] int present;
  matrix[N_obs, N_knots+2] B;
  array[N_obs] int station_type; 
  array[N_obs] int station;
}
parameters{
  real a;
  matrix[N_knots+2, 3] w; 
  vector[N_stations] z_station;
  real<lower=0> sigma_station;
  vector[3] z_type;
  real<lower=0> sigma_type;
}
model{
  vector[N_obs] p;
  a ~ normal(-2, 2);
  to_vector(w) ~ normal(0, 1);
  z_station ~ normal(0, 1);
  sigma_station ~ exponential(1);
  z_type ~ normal(0, 1);
  sigma_type ~ exponential(1);
  for(i in 1:N_obs){
    p[i] = a + B[i] * col(w, station_type[i]) +
           z_station[station[i]] * sigma_station + 
           z_type[station_type[i]] * sigma_type;
    p[i] = inv_logit(p[i]);
  }
  present ~ binomial(1 , p);
}
