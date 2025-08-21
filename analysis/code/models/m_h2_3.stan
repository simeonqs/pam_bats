data {
  int N_obs;
  int N_knots_date;
  int N_knots_temp;
  int N_stations;
  array[N_obs] int present;
  matrix[N_obs, N_knots_date] B_date;         
  matrix[N_obs, N_knots_temp] B_temp;
  array[N_obs] real wind_speed; 
  array[N_obs] real wind_dir; 
  array[N_obs] real dist_coast;
  array[N_obs] int year;                      
  array[2] matrix[N_obs, N_knots_date] B_year_date; 
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
  vector[N_knots_date] w_date;                
  vector[N_knots_temp] w_temp;
  real<lower=0> b_wind_speed;
  real b_sin;
  real b_cos;
  real<lower=0> b_dist_coast;
  matrix[2, N_knots_date] w_year_date; 
  real<lower=0> sigma_year_date;
  vector[N_stations] z_station;
  real<lower=0> sigma_station;
}
model {
  vector[N_obs] p;
  a ~ normal(-2, 2);
  w_date ~ normal(0, 1);
  w_temp ~ normal(0, 1);
  b_wind_speed ~ exponential(10);
  b_sin ~ normal(0, 2);
  b_cos ~ normal(0, 2);
  b_dist_coast ~ exponential(10);
  to_vector(w_year_date) ~ normal(0, sigma_year_date);
  sigma_year_date ~ exponential(2);
  z_station ~ normal(0, 1);
  sigma_station ~ exponential(1);
  for (i in 1:N_obs) {
    p[i] = a +
           dot_product(B_date[i], w_date) +
           dot_product(B_temp[i], w_temp) -
           b_wind_speed * wind_speed[i] +
           b_sin * wind_sin[i] + b_cos * wind_cos[i] -
           b_dist_coast * dist_coast[i] +
           dot_product(B_year_date[year[i]][i], w_year_date[year[i]]) +
           z_station[station[i]] * sigma_station;
    p[i] = inv_logit(p[i]);
  }
  present ~ binomial(1, p);
}
