data{
  int N_obs;
  int N_knots;
  int N_stations;
  array[N_obs] int present;
  matrix[N_obs, N_knots+2] B;
  array[N_obs] int year;
  array[N_obs] real dist_coast;
  array[N_obs] int station;
}
parameters {
  real a;
  vector[N_knots+2] w;                          // overall date effect
  matrix[N_knots+2, 2] w_year_offset;           // deviations per year
  real<lower=0> sigma_w;                        // SD of deviations
  real<lower=0> b;
  vector[N_stations] z_station;
  real<lower=0> sigma_station;
}
model {
  vector[N_obs] p;

  a ~ normal(-2, 2);
  w ~ normal(0, 1);                            // shared spline effect
  to_vector(w_year_offset) ~ normal(0, 1);     // standardized offsets
  sigma_w ~ exponential(1);
  b ~ exponential(10);
  z_station ~ normal(0, 1);
  sigma_station ~ exponential(1);

  for (i in 1:N_obs) {
    vector[N_knots+2] w_effect = w + sigma_w * col(w_year_offset, year[i]);
    p[i] = a +
           B[i] * w_effect -
           b * dist_coast[i] +
           z_station[station[i]] * sigma_station;
    p[i] = inv_logit(p[i]);
  }

  present ~ binomial(1, p);
}
