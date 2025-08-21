data{
  int N_obs;
  int N_knots_date;
  int N_stations;
  array[N_obs] int present;
  matrix[N_obs, N_knots_date+2] B_date;
  array[N_obs] real wind_dir; // degrees, 0 to 360
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
  vector[N_knots_date+2] w_date;
  real b_sin;
  real b_cos;
  vector[N_stations] z_station;
  real<lower=0> sigma_station;
}
model{
  vector[N_obs] p;
  a ~ normal(-2, 2);
  w_date ~ normal(0, 1);
  b_sin ~ normal(0, 2);
  b_cos ~ normal(0, 2);
  z_station ~ normal(0, 1);
  sigma_station ~ exponential(1);
  for (i in 1:N_obs) {
    real smooth_date = dot_product(B_date[i], w_date);
    p[i] = a + smooth_date + 
      b_sin * wind_sin[i] + b_cos * wind_cos[i] +
      z_station[station[i]] * sigma_station;
    p[i] = inv_logit(p[i]);
  }
  present ~ binomial(1, p);
}
