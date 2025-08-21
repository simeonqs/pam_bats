data{
  int N_obs;
  int N_knots_date;
  int N_knots_temp;
  int N_stations;
  array[N_obs] int present;
  array[N_obs] real wind_dir; 
  matrix[N_obs, N_knots_date+2] B_date;
  matrix[N_obs, N_knots_temp+2] B_temp;
  array[N_obs] real dist_coast;
  array[N_obs] real wind_speed; 
  array[N_obs] int station;
}
transformed data {
  vector[N_obs] wind_sin;
  vector[N_obs] wind_cos;
  for (i in 1:N_obs) {
    wind_sin[i] = sin(wind_dir[i] * pi() / 180);
    wind_cos[i] = cos(wind_dir[i] * pi() / 180);
  }
}
parameters{
  real a;
  real b_sin;
  real b_cos;
  vector[N_knots_date+2] w_date;
  vector[N_knots_temp+2] w_temp;
  vector[N_knots_date+2] w_sin;
  vector[N_knots_date+2] w_cos;
  real<lower=0> b_dist_coast;
  real<lower=0> b_wind_speed;
  vector[N_stations] z_station;
  real<lower=0> sigma_station;
}
model{
  vector[N_obs] p;
  a ~ normal(-2, 2);
  b_sin ~ normal(0, 2);
  b_cos ~ normal(0, 2);
  w_date ~ normal(0, 1);
  w_temp ~ normal(0, 1);
  w_sin ~ normal(0, 1);
  w_cos ~ normal(0, 1);
  b_dist_coast ~ exponential(10);
  b_wind_speed ~ exponential(10);
  z_station ~ normal(0, 1);
  sigma_station ~ exponential(1);
  for (i in 1:N_obs) {
    real smooth_temp = dot_product(B_temp[i], w_temp);
    real smooth_date = dot_product(B_date[i], w_date);
    real sin_inter = wind_sin[i] * dot_product(B_date[i], w_sin);
    real cos_inter = wind_cos[i] * dot_product(B_date[i], w_cos);
    p[i] = a + 
      b_sin * wind_sin[i] + b_cos * wind_cos[i] +
      smooth_date + 
      smooth_temp +
      sin_inter + cos_inter -
      b_dist_coast * dist_coast[i] -
      b_wind_speed * wind_speed[i] +
      z_station[station[i]] * sigma_station;
    p[i] = inv_logit(p[i]);
  }
  present ~ binomial(1, p);
}
