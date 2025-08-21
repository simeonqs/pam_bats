data {
  int N_obs;
  int N_stations;
  array[N_obs] int present;
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
parameters {
  real a;
  real b_sin;
  real b_cos;
  vector[N_stations] z_station;
  real<lower=0> sigma_station;
}
model {
  vector[N_obs] p;

  // Priors
  a ~ normal(-2, 2);
  z_station ~ normal(0, 1);
  sigma_station ~ exponential(2);
  b_sin ~ normal(0, 2);
  b_cos ~ normal(0, 2);

  // Linear predictor using cyclic wind direction
  for (i in 1:N_obs) {
    p[i] = a
           + b_sin * wind_sin[i]
           + b_cos * wind_cos[i]
           + z_station[station[i]] * sigma_station;
    p[i] = inv_logit(p[i]);
  }

  // Likelihood
  present ~ binomial(1, p);
}
