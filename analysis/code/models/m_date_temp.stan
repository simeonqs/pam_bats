data{
  int N_obs;
  int N_knots_date;
  int N_knots_temp;
  int N_stations;
  array[N_obs] int present;
  matrix[N_obs, N_knots_date+2] B_date;
  matrix[N_obs, N_knots_temp+2] B_temp;
  array[N_obs] int station;
}
parameters{
  real a;
  vector[N_knots_date+2] w_date;
  vector[N_knots_temp+2] w_temp;
  matrix[N_knots_date+2, N_knots_temp+2] w_date_temp;
  vector[N_stations] z_station;
  real<lower=0> sigma_station;
}
model{
  vector[N_obs] p;
  a ~ normal(-2, 2);
  w_date ~ normal(0, 1);
  w_temp ~ normal(0, 1);
  to_vector(w_date_temp) ~ normal(0, 1);
  z_station ~ normal(0, 1);
  sigma_station ~ exponential(1);
  for (i in 1:N_obs) {
    real smooth_date = dot_product(B_date[i], w_date);
    real smooth_temp = dot_product(B_temp[i], w_temp);
    real smooth_date_temp = 0;
    for (j1 in 1:(N_knots_date+2)) {
      for (j2 in 1:(N_knots_temp+2)) {
        smooth_date_temp += B_date[i, j1] * B_temp[i, j2] * 
        w_date_temp[j1, j2];
      }
    }
    p[i] = a + smooth_date + smooth_temp + smooth_date_temp +
           z_station[station[i]] * sigma_station;
    p[i] = inv_logit(p[i]);
  }
  present ~ binomial(1, p);
}
