data{
  int N_obs;
  int N_knots_date;
  int N_knots_temp;
  int N_stations;
  array[N_obs] int present;
  array[N_obs] int station_type; 
  matrix[N_obs, N_knots_date+2] B_date;
  matrix[N_obs, N_knots_temp+2] B_temp;
  array[N_obs] real wind_speed;
  array[N_obs] real dist_coast;
  array[N_obs] int station;
}
parameters{
  real a;
  vector[3] z_type;
  real<lower=0> sigma_type;
  vector[N_knots_date+2] w_date;
  vector[N_knots_temp+2] w_temp;
  real<lower=0> b_wind_speed;
  real<lower=0> b_dist_coast;
  vector[N_stations] z_station;
  real<lower=0> sigma_station;
}
model{
  vector[N_obs] p;
  a ~ normal(-2, 2);
  z_type ~ normal(0, 1);
  sigma_type ~ exponential(1);
  w_date ~ normal(0, 1);
  w_temp ~ normal(0, 1);
  b_wind_speed ~ exponential(10);
  b_dist_coast ~ exponential(10);
  z_station ~ normal(0, 1);
  sigma_station ~ exponential(1);

  for(i in 1:N_obs){
    real smooth_date = dot_product(B_date[i], w_date);
    real smooth_temp = dot_product(B_temp[i], w_temp);
    p[i] = a +
           z_type[station_type[i]] * sigma_type +
           smooth_date + 
           smooth_temp -
           b_wind_speed * wind_speed[i] -
           b_dist_coast * dist_coast[i] +
           z_station[station[i]] * sigma_station;
    p[i] = inv_logit(p[i]);
  }

  present ~ binomial(1, p);
}
