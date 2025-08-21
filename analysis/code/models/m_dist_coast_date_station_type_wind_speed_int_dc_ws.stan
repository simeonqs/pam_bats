data{
  int N_obs;
  int N_knots;
  int N_stations;
  array[N_obs] int present;
  matrix[N_obs, N_knots+2] B;
  array[N_obs] real dist_coast;
  array[N_obs] int station_type; 
  array[N_obs] real wind_speed;
  array[N_obs] int station;
}
parameters{
  real a;
  real<lower=0> b_dist_coast;
  real<lower=0> b_wind_speed;
  real b_int;
  vector[N_knots+2] w;
  vector[3] z_type;
  real<lower=0> sigma_type;
  vector[N_stations] z_station;
  real<lower=0> sigma_station;
}
model{
  vector[N_obs] p;
  a ~ normal(-2, 2);
  b_wind_speed ~ exponential(5);
  b_dist_coast ~ exponential(10);
  b_int ~ normal(0, 1);
  w ~ normal(0, 1);
  z_type ~ normal(0, 1);
  z_station ~ normal(0, 1);
  sigma_type ~ exponential(1);
  sigma_station ~ exponential(1);

  for(i in 1:N_obs){
    p[i] = a +
           B[i] * w -
           b_wind_speed * wind_speed[i] -
           b_dist_coast * dist_coast[i] +
           b_int * wind_speed[i] * dist_coast[i] +
           z_station[station[i]] * sigma_station +
           z_station[station[i]] * sigma_station;
    p[i] = inv_logit(p[i]);
  }

  present ~ binomial(1, p);
}
