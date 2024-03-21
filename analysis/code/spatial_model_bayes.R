# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Bayesian version of models. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'rethinking', 'splines')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_dat_model = 'analysis/results/spatial_model/dat_model.RData'
path_model = 'analysis/code/models/m_night.stan'

# Load
load(path_dat_model)

# Make simple model with b-splines
num_knots = 5
degree = 3
knots = as.numeric(quantile(dat_model$day_of_year, 
                            probs = seq(0, 1, length.out = num_knots)))
B = bs(dat_model$day_of_year,
       knots = knots[-c(1, num_knots)],
       degree = degree, intercept = TRUE)
clean_dat = list(present = dat_model$present, 
                 B = B,
                 N_obs = nrow(dat_model),
                 N_knots = num_knots)
model = cmdstan_model(path_model)
fit = model$sample(data = clean_dat,
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4)
print(fit$summary())
fit_nice = fit$output_files() %>%
  rstan::read_stan_csv()
post = fit_nice %>%
  rethinking::extract.samples()
precis(fit_nice, depth = 2)

# Plot predictions
plot(dat_model$day_of_year, 
     dat_model$present,
     ylim = c(0, 1),
     pch = 16, col = '#AED6F1',
     xlab = 'night of year', ylab = 'probability presence per night')
nights = seq(min(dat_model$day_of_year),
             max(dat_model$day_of_year), 
             1)
# averages = vapply(nights, function(night) 
#   mean(dat_model$present[dat_model$day_of_year == night]), numeric(1))
# points(nights, averages, pch = 16, cex = 2, col = '#AED6F1')
B = bs(nights,
       knots = knots[-c(1, num_knots)],
       degree = degree, intercept = TRUE)
pred = vapply(seq_along(post$a), function(i)
  vapply(seq_along(nights), function(j) post$a[i] + B[j,] %*% post$w[i,],
         numeric(1)),
  numeric(length(nights)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, nights , col = '#AED6F1')
lines(nights, mean_pred, lwd = 3, col = '#1B4F72')

