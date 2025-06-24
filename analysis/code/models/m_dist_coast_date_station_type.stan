data{
  int N_obs;
  int N_knots;
  int N_stations;
  array[N_obs] int present;
  matrix[N_obs, N_knots+2] B;
  array[N_obs] real dist_coast;
  array[N_obs] int station_type; 
  array[N_obs] int station;
}
parameters{
  real a;
  real<lower=0> b;
  vector[N_knots+2] w;
  vector[N_knots+2] w_interact;
  vector[3] z_type;
  real<lower=0> sigma_type;
  vector[N_stations] z_station;
  real<lower=0> sigma_station;
}
model{
  vector[N_obs] p;
  a ~ normal(-2, 2);
  b ~ exponential(1);
  w ~ normal(0, 1);
  w_interact ~ normal(0, 1);
  z_type ~ normal(0, 1);
  z_station ~ normal(0, 1);
  sigma_type ~ exponential(1);
  sigma_station ~ exponential(1);

  for(i in 1:N_obs){
    vector[N_knots+2] smooth_term = w + w_interact * dist_coast[i];
    p[i] = a - b * dist_coast[i] +
           B[i] * smooth_term +
           z_station[station[i]] * sigma_station +
           z_station[station[i]] * sigma_station;
    p[i] = inv_logit(p[i]);
  }

  present ~ binomial(1, p);
}
