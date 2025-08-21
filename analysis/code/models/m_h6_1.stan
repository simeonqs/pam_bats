data{
  int N_obs;
  int N_knots_wind_speed;
  int N_knots_date;
  int N_knots_temp;
  int N_stations;
  array[N_obs] int present;
  matrix[N_obs, N_knots_wind_speed+2] B_wind_speed;
  array[N_obs] int wind_dir; 
  matrix[N_obs, N_knots_date+2] B_date;
  matrix[N_obs, N_knots_temp+2] B_temp;
  array[N_obs] real dist_coast;
  array[N_obs] int station;
}
parameters{
  real a;
  matrix[N_knots_wind_speed+2, 2] w_wind_speed;
  vector[N_knots_date+2] w_date;
  vector[N_knots_temp+2] w_temp;
  real<lower=0> b_dist_coast;
  vector[N_stations] z_station;
  real<lower=0> sigma_station;
}
model{
  vector[N_obs] p;
  a ~ normal(-2, 2);
  to_vector(w_wind_speed) ~ normal(0, 1);
  w_date ~ normal(0, 1);
  w_temp ~ normal(0, 1);
  b_dist_coast ~ exponential(10);
  z_station ~ normal(0, 1);
  sigma_station ~ exponential(1);
  for (i in 1:N_obs) {
    real smooth_wind_speed = dot_product(B_wind_speed[i], 
                                         col(w_wind_speed, wind_dir[i]));
    real smooth_date = dot_product(B_date[i], w_date);
    real smooth_temp = dot_product(B_temp[i], w_temp);
    p[i] = a +
      smooth_wind_speed +
      smooth_date + 
      smooth_temp -
      b_dist_coast * dist_coast[i] +
      z_station[station[i]] * sigma_station;
    p[i] = inv_logit(p[i]);
  }
  present ~ binomial(1, p);
}
