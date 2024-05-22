# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Bayesian version of models. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'rethinking', 'splines', 'rstan')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_dat_model = 'analysis/results/spatial_model/dat_model.RData'
path_m_night = 'analysis/code/models/m_night.stan'
path_m_night_spatial = 'analysis/code/models/m_night_spatial.stan'

# Load
load(path_dat_model)

# Make simple model with b-splines
num_knots = 5
degree = 3
knots = as.numeric(quantile(dat_model$night_of_year, 
                            probs = seq(0, 1, length.out = num_knots)))
B = bs(dat_model$night_of_year,
       knots = knots[-c(1, num_knots)],
       degree = degree, intercept = TRUE)
clean_dat = list(N_obs = nrow(dat_model),
                 N_knots = num_knots,
                 N_stations = dat_model$station |> unique() |> length(),
                 present = dat_model$present, 
                 B = B,
                 station = dat_model$station_numeric)
model = cmdstan_model(path_m_night)
fit = model$sample(data = clean_dat,
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4)
fit_nice = fit$output_files() %>%
  rstan::read_stan_csv()
post = fit_nice %>%
  rethinking::extract.samples()
fit_nice |> precis(depth = 2) |> round(2) |> print()

# Plot predictions
plot(dat_model$night_of_year, 
     dat_model$present,
     ylim = c(0, 1),
     pch = 16, col = '#AED6F1',
     xlab = 'night of year', ylab = 'probability presence per night')
nights = seq(min(dat_model$night_of_year),
             max(dat_model$night_of_year), 
             1)
B_plot = bs(nights,
       knots = knots[-c(1, num_knots)],
       degree = degree, intercept = TRUE)
pred = vapply(seq_along(post$a), function(i)
  vapply(seq_along(nights), function(j) post$a[i] + B_plot[j,] %*% post$w[i,],
         numeric(1)),
  numeric(length(nights)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, nights , col = '#AED6F1')
lines(nights, mean_pred, lwd = 3, col = '#1B4F72')

# Include distance
clean_dat$d_mat = d_mat
model = cmdstan_model(path_m_night_spatial)
fit = model$sample(data = clean_dat,
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4)
fit_night_spatial = fit$output_files() %>%
  rstan::read_stan_csv()
post_night_spatial = fit_night_spatial %>%
  rethinking::extract.samples()
fit_night_spatial |> precis(depth = 2) |> round(2) |> print()

# Plot predictions
plot(dat_model$night_of_year, 
     dat_model$present,
     ylim = c(0, 1),
     pch = 16, col = '#AED6F1',
     xlab = 'night of year', ylab = 'probability presence per night')
pred = vapply(seq_along(post_night_spatial$a_bar), function(i)
  vapply(seq_along(nights), function(j) 
    post_night_spatial$a_bar[i] + B_plot[j,] %*% post_night_spatial$w[i,],
         numeric(1)),
  numeric(length(nights)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, nights , col = '#AED6F1')
lines(nights, mean_pred, lwd = 3, col = '#1B4F72')

# Wind direction
plot(dat_model$windspeed, 
     dat_model$present,
     ylim = c(0, 1),
     pch = 16, col = '#AED6F1',
     xlab = 'wind speed', ylab = 'probability presence per night')

# Temperature
plot(dat_model$temp, 
     dat_model$present,
     ylim = c(0, 1),
     pch = 16, col = '#AED6F1',
     xlab = 'temperature', ylab = 'probability presence per night')

