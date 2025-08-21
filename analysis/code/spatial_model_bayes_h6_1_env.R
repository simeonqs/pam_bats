# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Bayesian version of model for hypothesis 6_1. 
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
path_m = 'analysis/code/models/m_h6_1.stan'
path_pdf = 'analysis/results/spatial_model/model_h6_1_env.pdf'

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
num_knots_wind_speed = 3
degree = 3
knots_date = as.numeric(quantile(dat_model$julian_date, 
                                 probs = seq(0, 1, 
                                             length.out = num_knots_date)))
knots_temp = as.numeric(quantile(dat_model$mean_temp, 
                                 probs = seq(0, 1, 
                                             length.out = num_knots_temp)))
knots_wind_speed = as.numeric(quantile(dat_model$wind_speed, 
                                       probs = seq(0, 1, 
                                                   length.out = 
                                                     num_knots_wind_speed)))
B_date = bs(dat_model$julian_date,
            knots = knots_date[-c(1, num_knots_date)],
            degree = degree, intercept = TRUE)
B_temp = bs(dat_model$mean_temp,
            knots = knots_temp[-c(1, num_knots_temp)],
            degree = degree, intercept = TRUE)
B_wind_speed = bs(dat_model$wind_speed,
                  knots = knots_wind_speed[-c(1, num_knots_wind_speed)],
                  degree = degree, intercept = TRUE)
clean_dat = list(N_obs = nrow(dat_model),
                 N_knots_date = num_knots_date,
                 N_knots_temp = num_knots_temp,
                 N_knots_wind_speed = num_knots_wind_speed,
                 N_stations = dat_model$station |> unique() |> length(),
                 present = dat_model$detection_env |> as.integer(), 
                 B_wind_speed = B_wind_speed,
                 wind_dir = as.integer(dat_model$wind_direction < 180) + 1L,
                 B_date = B_date,
                 B_temp = B_temp,
                 dist_coast = dat_model$distance_to_coast,
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

## the effect of wind speed at peak of migration, 18 degrees, 20 km off the 
## coast for easterly winds and westerly winds

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

## get spline for temp at ca. 18 degrees (18.003)
ads = abs(dat_model$mean_temp - 18)
B_temp_plot = B_temp[ads == min(ads),]

pdf(path_pdf, 6, 6)
par(mfrow = c(2, 1), mar = c(4, 4, 0.5, 1))

## westerly winds ----
trans_subset = c(Buoys = 19,        # circle
                 Windturbines = 17, # triangle
                 OSS = 15)          # square
subber = dat_model$wind_direction > 180
sorter = order(dat_model$wind_speed[subber])
plot(dat_model$wind_speed[subber][sorter], 
     dat_model$detection_env[subber][sorter] - 
       rnorm(nrow(dat_model[subber,]), 0.2, 0.05),
     xlim = range(dat_model$wind_speed),
     ylim = c(-0.41, 1.01),
     xaxt = 'n', yaxt = 'n',
     pch = trans_subset[dat_model$subset[subber][sorter]], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
     )[dat_model$year[subber][sorter]],
     cex = 0.8,
     xlab = 'Windspeed [m/s]', ylab = 'Probability presence per night')
text(15.5, 0.85, 'Westerly winds', font = 2)
axis(1, seq(0, 30, 2))
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)
speeds = dat_model$wind_speed[subber][sorter]
B_wind_speed_plot = B_wind_speed[subber,][sorter,]
pred = vapply(seq_along(post$a), function(i) {
  vapply(seq_along(speeds), function(j) {
    return(post$a[i] + 
             B_wind_speed_plot[j,] %*% post$w_wind_speed[i,,1] +
             B_temp_plot %*% post$w_temp[i,] +
             B_date_plot %*% post$w_date[i,] - 
             post$b_dist_coast[i] * 20)
  }, numeric(1))
}, numeric(length(speeds)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, speeds , col = '#AED6F1')
lines(speeds, mean_pred, lwd = 3, col = '#1B4F72')

## westerly winds ----
trans_subset = c(Buoys = 19,        # circle
                 Windturbines = 17, # triangle
                 OSS = 15)          # square
subber = dat_model$wind_direction < 180
sorter = order(dat_model$wind_speed[subber])
plot(dat_model$wind_speed[subber][sorter], 
     dat_model$detection_env[subber][sorter] - 
       rnorm(nrow(dat_model[subber,]), 0.2, 0.05),
     xlim = range(dat_model$wind_speed),
     ylim = c(-0.41, 1.01),
     xaxt = 'n', yaxt = 'n',
     pch = trans_subset[dat_model$subset[subber][sorter]], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
     )[dat_model$year[subber][sorter]],
     cex = 0.8,
     xlab = 'Windspeed [m/s]', ylab = 'Probability presence per night')
text(15.5, 0.85, 'Easterly winds', font = 2)
axis(1, seq(0, 30, 2))
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)
speeds = dat_model$wind_speed[subber][sorter]
B_wind_speed_plot = B_wind_speed[subber,][sorter,]
pred = vapply(seq_along(post$a), function(i) {
  vapply(seq_along(speeds), function(j) {
    return(post$a[i] + 
             B_wind_speed_plot[j,] %*% post$w_wind_speed[i,,2] +
             B_temp_plot %*% post$w_temp[i,] +
             B_date_plot %*% post$w_date[i,] - 
             post$b_dist_coast[i] * 20)
  }, numeric(1))
}, numeric(length(speeds)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, speeds , col = '#AED6F1')
lines(speeds, mean_pred, lwd = 3, col = '#1B4F72')

dev.off()