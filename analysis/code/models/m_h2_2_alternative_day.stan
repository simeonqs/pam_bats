data {
  int N_obs;
  int N_knots;
  int N_days;
  int N_stations;
  array[N_obs] int present;
  matrix[N_obs, N_knots+2] B_date;         
  array[N_obs] real dist_coast;
  array[N_obs] int year;                      
  array[2] matrix[N_obs, N_knots+2] B_year_date; 
  array[N_obs] int day;
  array[N_obs] int station;                   
}
parameters {
  real a;
  vector[N_knots+2] w_date;                
  real<lower=0> b_dist;
  matrix[2, N_knots+2] w_year_date; 
  real<lower=0> sigma_year_date;
  vector[N_days] z_day;
  real<lower=0> sigma_day;
  vector[N_stations] z_station;
  real<lower=0> sigma_station;
}
model {
  vector[N_obs] p;
  a ~ normal(-2, 2);
  w_date ~ normal(0, 1);
  b_dist ~ exponential(10);
  to_vector(w_year_date) ~ normal(0, sigma_year_date);
  sigma_year_date ~ exponential(1);
  z_day ~ normal(0, 1);
  sigma_day ~ exponential(1);
  z_station ~ normal(0, 1);
  sigma_station ~ exponential(1);
  for (i in 1:N_obs) {
    p[i] = a +
           dot_product(B_date[i], w_date) -
           b_dist * dist_coast[i] +
           dot_product(B_year_date[year[i]][i], w_year_date[year[i]]) +
           z_day[day[i]] * sigma_day + 
           z_station[station[i]] * sigma_station;
    p[i] = inv_logit(p[i]);
  }
  present ~ binomial(1, p);
}
