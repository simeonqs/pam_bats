functions{
    matrix cov_GPL2(matrix x, real sq_alpha, real sq_rho, real delta) {
        int N = dims(x)[1];
        matrix[N, N] K;
        for (i in 1:(N-1)) {
          K[i, i] = sq_alpha + delta;
          for (j in (i + 1):N) {
            K[i, j] = sq_alpha * exp(-sq_rho * square(x[i,j]) );
            K[j, i] = K[i, j];
          }
        }
        K[N, N] = sq_alpha + delta;
        return K;
    }
}
data{
  int N_obs;
  int N_knots_date;
  int N_knots_temp;
  int N_stations;
  array[N_obs] int present;
  matrix[N_obs, N_knots_date+2] B_date;
  matrix[N_obs, N_knots_temp+2] B_temp;
  array[N_obs] real wind_speed;
  array[N_obs] real dist_coast;
  array[N_obs] int station_type; 
  array[N_obs] int station;
  matrix[N_stations, N_stations] d_mat;
}
parameters{
  real a;
  vector[N_knots_date+2] w_date;
  vector[N_knots_temp+2] w_temp;
  real<lower=0> b_wind_speed;
  real<lower=0> b_dist_coast;
  vector[3] z_type;
  real<lower=0> sigma_type;
  vector[N_stations] a_station;
  real<lower=0> etasq;
  real<lower=0> rhosq;
  real<lower=0> sigma_station;
}
model{
  vector[N_stations] mu;
  matrix[N_stations, N_stations] SIGMA;
  vector[N_obs] p;
  a ~ normal(-2, 2);
  w_date ~ normal(0, 1);
  w_temp ~ normal(0, 1);
  b_wind_speed ~ exponential(5);
  b_dist_coast ~ exponential(10);
  z_type ~ normal(0, 1);
  sigma_type ~ exponential(1);
  rhosq ~ exponential(0.1);
  etasq ~ exponential(1);
  sigma_station ~ exponential(1);
  SIGMA = cov_GPL2(d_mat, etasq, rhosq, sigma_station);
  for(i in 1:N_stations) mu[i] = a;
  a_station ~ multi_normal(mu, SIGMA);
  for (i in 1:N_obs) {
    real smooth_date = dot_product(B_date[i], w_date);
    real smooth_temp = dot_product(B_temp[i], w_temp);
    p[i] = a_station[station[i]] + 
      smooth_date +
      smooth_temp -
      b_wind_speed * wind_speed[i] -
      b_dist_coast * dist_coast[i]+
      z_type[station_type[i]] * sigma_type;
    p[i] = inv_logit(p[i]);
  }
  present ~ binomial(1, p);
}
