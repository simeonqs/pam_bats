# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Bayesian version of model for distance to coast.
# This model assumes exponential decline that follows: y(x)=y0*exp(-k*x). Where
# y0 is the initial value at source, k is rate of decline and x is distance.
# But because the response is binomial, it ends up being just a 'linear' 
# predictor. 
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
path_combined_data = 'analysis/results/combined_data.RData'
path_m_dist_coast = 'analysis/code/models/m_dist_coast.stan'
path_pdf = 'analysis/results/spatial_model/model_dist_coast.pdf'

# Load
load(path_combined_data)

# Simulate data
dist_coast = seq(0, 100, 1)
y0 = 0.2
k = 0.05
detection = y0 * exp(-k*dist_coast)
plot(dist_coast, detection, ylim = c(0, 0.5))

# Linear model
k = 0.01
detection = inv_logit(logit(y0) - k * dist_coast)
points(dist_coast, detection, col = 'green')

# Combine
detection = y0 * exp(-k*dist_coast)
points(dist_coast, detection, col = 'red')

# Add Julian date as continuous
dat_model$julian_date = as.POSIXlt(dat_model$date)$yday + 1
dat_model$year = ifelse(str_detect(dat_model$date, '2023'), 1,
                        ifelse(str_detect(dat_model$date, '2024'), 2, 3))

# Subset of fall migration
dat_model = dat_model[dat_model$julian_date >= 214 & # '08-01'
                        dat_model$julian_date <= 289,] # '10-15'

# Clean data
clean_dat = list(N_obs = nrow(dat_model),
                 N_stations = dat_model$station |> unique() |> length(),
                 present = dat_model$detection |> as.integer(), 
                 dist_coast = dat_model$distance_to_coast,
                 station = dat_model$station |> as.factor() |> as.integer())
model = cmdstan_model(path_m_dist_coast)
fit = model$sample(data = clean_dat,
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4)
print(fit$summary(), n = 100)
extract.samples.cmdstanr <- function(fit_obj) {
  vars <- fit_obj$metadata()$stan_variables
  draws <- posterior::as_draws_rvars(fit_obj$draws())
  
  lapply(vars, \(var_name){  
    posterior::draws_of(draws[[var_name]], with_chains = FALSE)
  }) |> setNames(vars)
}
post = extract.samples.cmdstanr(fit)

# Plot predictions
trans_subset = c(Buoys = 19,        # circle
                 Windturbines = 17, # triangle
                 OSS = 15)          # square
pdf(path_pdf, 6, 3.5)
par(mar = c(4, 4, 0.5, 1))
reorder = order(dat_model$distance_to_coast)
plot(dat_model[reorder,]$distance_to_coast, 
     dat_model[reorder,]$detection - rnorm(nrow(dat_model), 0.2, 0.05),
     xaxt = 'n', yaxt = 'n', xlim = c(0, 90),
     pch = trans_subset[dat_model[reorder,]$subset], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
     )[dat_model[reorder,]$year],
     cex = 0.8,
     xlab = 'Distance to coast', ylab = 'Probability presence per night')
axis(1, c(0, 20, 40, 60, 80))
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)
dist_coast = seq(min(dat_model$distance_to_coast),
             max(dat_model$distance_to_coast), 
             1)
pred = vapply(seq_along(post$a), function(i)
  vapply(seq_along(dist_coast), function(j) 
    post$a[i] - post$b[i] * dist_coast[j],
         numeric(1)),
  numeric(length(dist_coast)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, dist_coast , col = '#AED6F1')
lines(dist_coast, mean_pred, lwd = 3, col = '#1B4F72')
dev.off()




