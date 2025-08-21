# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Bayesian version of model for hypothesis 4. 
# This version includes records of ENV only. 
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
path_m = 'analysis/code/models/m_h4.stan'
path_pdf = 'analysis/results/spatial_model/model_h4_env.pdf'

# Load
load(path_combined_data)

# Add Julian date as continuous
dat_model$julian_date = as.POSIXlt(dat_model$date)$yday + 1
dat_model$year = ifelse(str_detect(dat_model$date, '2023'), 1,
                        ifelse(str_detect(dat_model$date, '2024'), 2, 3))

# Subset of fall migration
dat_model = dat_model[dat_model$julian_date >= 214 & # '08-01'
                        dat_model$julian_date <= 289,] # '10-15'


# Make simple model with b-splines for night of year and temperature
num_knots_date = 5
num_knots_temp = 3
degree = 3
knots_date = as.numeric(quantile(dat_model$julian_date, 
                                 probs = seq(0, 1, 
                                             length.out = num_knots_date)))
knots_temp = as.numeric(quantile(dat_model$mean_temp, 
                                 probs = seq(0, 1, 
                                             length.out = num_knots_temp)))
B_date = bs(dat_model$julian_date,
            knots = knots_date[-c(1, num_knots_date)],
            degree = degree, intercept = TRUE)
B_temp = bs(dat_model$mean_temp,
            knots = knots_temp[-c(1, num_knots_temp)],
            degree = degree, intercept = TRUE)
clean_dat = list(N_obs = nrow(dat_model),
                 N_knots_temp = num_knots_temp,
                 N_knots_date = num_knots_date,
                 N_stations = dat_model$station |> unique() |> length(),
                 present = dat_model$detection_env |> as.integer(), 
                 B_temp = B_temp,
                 B_date = B_date,
                 dist_coast = dat_model$distance_to_coast,
                 wind_speed = dat_model$wind_speed,
                 wind_dir = dat_model$wind_direction,
                 station = dat_model$station |> as.factor() |> as.integer())
model = cmdstan_model(path_m)
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

# Plot ----

## the effect of temperature at peak of migration, 3 m/s and 20 km off the 
## coast and northerly wind

## find peak migration season
sorter = order(dat_model$julian_date)
nights = dat_model$julian_date[sorter]
B_pred_date = B_date[sorter,]
pred = vapply(seq_along(post$a), function(i)
  vapply(seq_along(nights), function(j) 
    post$a[i] + B_pred_date[j,] %*% post$w_date[i,],
    numeric(1)),
  numeric(length(nights)))
mean_pred = apply(pred, 1, mean) |> inv_logit()
peak_dates = dat_model$julian_date[sorter][mean_pred == max(mean_pred)]
if(any(peak_dates != mean(peak_dates))) stop('Problem finding peak date.')
peak_date = peak_dates[1]
B_date_plot = B_date[dat_model$julian_date == peak_date,][1,]

## now plot
trans_subset = c(Buoys = 19,        # circle
                 Windturbines = 17, # triangle
                 OSS = 15)          # square
pdf(path_pdf, 6, 3.5)
par(mar = c(4, 4, 0.5, 1))
sorter = order(dat_model$mean_temp)
plot(dat_model$mean_temp[sorter], 
     dat_model$detection_env[sorter] - rnorm(nrow(dat_model), 0.2, 0.05),
     xaxt = 'n', yaxt = 'n',
     pch = trans_subset[dat_model$subset[sorter]], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
     )[dat_model$year[sorter]],
     cex = 0.8,
     xlab = 'Temperature [°C]', ylab = 'Probability presence per night')
axis(1, unique(round(dat_model$mean_temp), 0.5))
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)
temps = dat_model$mean_temp[sorter]
B_temp_plot = B_temp[sorter,]
pred = vapply(seq_along(post$a), function(i) {
  vapply(seq_along(temps), function(j) {
    post$a[i] + 
      B_temp_plot[j,] %*% post$w_temp[i,] +
      B_date_plot %*% post$w_date[i,] - 
      post$b_dist_coast[i] * 20 -
      post$b_wind_speed[i] * 3 +
      post$b_cos[i] # this represents northerly wind (0˚)
  }, numeric(1))
}, numeric(length(temps)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, temps , col = '#AED6F1')
lines(temps, mean_pred, lwd = 3, col = '#1B4F72')
dev.off()
