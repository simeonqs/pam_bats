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
  int N_knots;
  int N_stations;
  array[N_obs] int present;
  matrix[N_obs, N_knots+2] B;
  array[N_obs] int station;
  matrix[N_stations, N_stations] d_mat;
}
parameters{
  real a_bar;
  vector[N_knots+2] w;
  vector[N_stations] a_station;
  real<lower=0> sigma_station;
  real<lower=0> etasq;
  real<lower=0> rhosq;
}
model{
  vector[N_stations] mu;
  matrix[N_stations, N_stations] SIGMA;
  vector[N_obs] p;
  rhosq ~ exponential(0.1);
  etasq ~ exponential(1);
  sigma_station ~ exponential(1);
  SIGMA = cov_GPL2(d_mat, etasq, rhosq, sigma_station);
  a_bar ~ normal(-2, 2);
  w ~ normal(0, 1);
  for(i in 1:N_stations) mu[i] = a_bar;
  a_station ~ multi_normal(mu, SIGMA);
  for(i in 1:N_obs){
    p[i] = a_station[station[i]] + B[i] * w;
    p[i] = inv_logit(p[i]);
  }
  present ~ binomial(1 , p);
}
