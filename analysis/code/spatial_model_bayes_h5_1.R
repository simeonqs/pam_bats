# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Bayesian version of model for hypothesis 5.1. 
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
path_m = 'analysis/code/models/m_h5_1.stan'
path_pdf = 'analysis/results/spatial_model/model_h5_1.pdf'

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
num_knots_date = 10
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
                 N_knots_date = num_knots_date,
                 N_knots_temp = num_knots_temp,
                 N_stations = dat_model$station |> unique() |> length(),
                 present = dat_model$detection |> as.integer(), 
                 wind_dir = dat_model$wind_direction,
                 B_date = B_date,
                 B_temp = B_temp,
                 dist_coast = dat_model$distance_to_coast,
                 wind_speed = dat_model$wind_speed,
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

## the effect of wind direction at peak of migration, 18 degrees and 20 km
## off the coast 

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

## now plot
trans_subset = c(Buoys = 19,        # circle
                 Windturbines = 17, # triangle
                 OSS = 15)          # square
pdf(path_pdf, 16, 7)
par(mar = c(4, 4, 0.5, 1),
    mfrow = c(2, 3))
layout(matrix(c(1, 2, 3,
                4, 4, 4), 
              byrow = TRUE, nrow = 2, ncol = 3))

## plot dist coast low wind (0-3) ----
ws = 1.5
subber = dat_model$wind_speed <= 3
sorter = order(dat_model$wind_direction[subber])
plot(dat_model$wind_direction[subber][sorter], 
     dat_model$detection[subber][sorter] - 
       rnorm(nrow(dat_model[subber,]), 0.2, 0.05),
     xaxt = 'n', yaxt = 'n',
     pch = trans_subset[dat_model$subset[subber][sorter]], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
     )[dat_model$year[subber][sorter]],
     cex = 0.8,
     xlim = range(dat_model$wind_direction),
     xlab = 'Wind direction', ylab = 'Probability presence per night')
tick_pos = seq(0, 360, by = 45)
tick_labels = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'N')
axis(1, at = tick_pos, labels = tick_labels)
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)

dirs = seq(0, 360, 1)
pred = vapply(seq_along(post$a), function(i) {
  vapply(seq_along(dirs), function(j) {
    wind_sin = sin(dirs[j] * pi / 180);
    wind_cos = cos(dirs[j] * pi / 180);
    return(post$a[i] +
             (post$b_sin[i] + post$b_sin_speed[i] * ws) * wind_sin +
             (post$b_cos[i] + post$b_cos_speed[i] * ws) * wind_cos +
             B_date_plot %*% post$w_date[i,] + 
             B_temp_plot %*% post$w_temp[i,] -
             post$b_dist_coast[i] * 20 -
             post$b_wind_speed[i] * ws 
    )
  }, numeric(1))
}, numeric(length(dirs)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, dirs , col = '#AED6F1')
lines(dirs, mean_pred, lwd = 3, col = '#1B4F72')

# Calculate wind direction effect magnitude for first pane (ws = 1.5)
wind_magnitude = sqrt((post$b_sin + post$b_sin_speed * ws)^2 +
                        (post$b_cos + post$b_cos_speed * ws)^2)
PI_wind_magnitude = PI(wind_magnitude)
mean_wind_magnitude = mean(wind_magnitude)
print(paste0('Wind direction effect magnitude (ws = ', ws, '): ', 
             round(mean_wind_magnitude, 2), 
             ' [', round(PI_wind_magnitude[1], 2), 
             ', ', round(PI_wind_magnitude[2], 2), ']'))

## plot dist coast medium wind (4-7) ----
ws = 5.5
subber = dat_model$wind_speed >= 4 & dat_model$wind_speed <= 7
sorter = order(dat_model$wind_direction[subber])
plot(dat_model$wind_direction[subber][sorter], 
     dat_model$detection[subber][sorter] - 
       rnorm(nrow(dat_model[subber,]), 0.2, 0.05),
     xaxt = 'n', yaxt = 'n',
     pch = trans_subset[dat_model$subset[subber][sorter]], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
     )[dat_model$year[subber][sorter]],
     cex = 0.8,
     xlim = range(dat_model$wind_direction),
     xlab = 'Wind direction', ylab = 'Probability presence per night')
tick_pos = seq(0, 360, by = 45)
tick_labels = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'N')
axis(1, at = tick_pos, labels = tick_labels)
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)

dirs = seq(0, 360, 1)
pred = vapply(seq_along(post$a), function(i) {
  vapply(seq_along(dirs), function(j) {
    wind_sin = sin(dirs[j] * pi / 180);
    wind_cos = cos(dirs[j] * pi / 180);
    return(post$a[i] +
             (post$b_sin[i] + post$b_sin_speed[i] * ws) * wind_sin +
             (post$b_cos[i] + post$b_cos_speed[i] * ws) * wind_cos +
             B_date_plot %*% post$w_date[i,] + 
             B_temp_plot %*% post$w_temp[i,] -
             post$b_dist_coast[i] * 20 -
             post$b_wind_speed[i] * ws 
    )
  }, numeric(1))
}, numeric(length(dirs)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, dirs , col = '#AED6F1')
lines(dirs, mean_pred, lwd = 3, col = '#1B4F72')

# Calculate wind direction effect magnitude for first pane (ws = 1.5)
wind_magnitude = sqrt((post$b_sin + post$b_sin_speed * ws)^2 +
                        (post$b_cos + post$b_cos_speed * ws)^2)
PI_wind_magnitude = PI(wind_magnitude)
mean_wind_magnitude = mean(wind_magnitude)
print(paste0('Wind direction effect magnitude (ws = ', ws, '): ', 
             round(mean_wind_magnitude, 2), 
             ' [', round(PI_wind_magnitude[1], 2), 
             ', ', round(PI_wind_magnitude[2], 2), ']'))

## plot dist coast low wind (8-11) ----
ws = 9.5
subber = dat_model$wind_speed >= 8 & dat_model$wind_speed <= 11
sorter = order(dat_model$wind_direction[subber])
plot(dat_model$wind_direction[subber][sorter], 
     dat_model$detection[subber][sorter] - 
       rnorm(nrow(dat_model[subber,]), 0.2, 0.05),
     xaxt = 'n', yaxt = 'n',
     pch = trans_subset[dat_model$subset[subber][sorter]], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
     )[dat_model$year[subber][sorter]],
     cex = 0.8,
     xlim = range(dat_model$wind_direction),
     xlab = 'Wind direction', ylab = 'Probability presence per night')
tick_pos = seq(0, 360, by = 45)
tick_labels = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'N')
axis(1, at = tick_pos, labels = tick_labels)
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)

dirs = seq(0, 360, 1)
pred = vapply(seq_along(post$a), function(i) {
  vapply(seq_along(dirs), function(j) {
    wind_sin = sin(dirs[j] * pi / 180);
    wind_cos = cos(dirs[j] * pi / 180);
    return(post$a[i] +
             (post$b_sin[i] + post$b_sin_speed[i] * ws) * wind_sin +
             (post$b_cos[i] + post$b_cos_speed[i] * ws) * wind_cos +
             B_date_plot %*% post$w_date[i,] + 
             B_temp_plot %*% post$w_temp[i,] -
             post$b_dist_coast[i] * 20 -
             post$b_wind_speed[i] * ws 
    )
  }, numeric(1))
}, numeric(length(dirs)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, dirs , col = '#AED6F1')
lines(dirs, mean_pred, lwd = 3, col = '#1B4F72')

# Calculate wind direction effect magnitude for first pane (ws = 1.5)
wind_magnitude = sqrt((post$b_sin + post$b_sin_speed * ws)^2 +
                        (post$b_cos + post$b_cos_speed * ws)^2)
PI_wind_magnitude = PI(wind_magnitude)
mean_wind_magnitude = mean(wind_magnitude)
print(paste0('Wind direction effect magnitude (ws = ', ws, '): ', 
             round(mean_wind_magnitude, 2), 
             ' [', round(PI_wind_magnitude[1], 2), 
             ', ', round(PI_wind_magnitude[2], 2), ']'))

## plot wind speed ----
sorter = order(dat_model$wind_speed)
plot(dat_model$wind_speed, 
     dat_model$detection - rnorm(nrow(dat_model), 0.2, 0.05),
     ylim = c(-0.41, 1.01),
     type = 'n',
     xaxt = 'n', yaxt = 'n',
     xlab = 'Windspeed [m/s]', ylab = 'Probability presence per night')
polygon(c(0, 0, 3, 3),
        c(-0.4, 1, 1, -0.4),
        col = '#f2f3f4', border = NA)
polygon(c(4, 4, 7, 7),
        c(-0.4, 1, 1, -0.4),
        col = '#f2f3f4', border = NA)
polygon(c(8, 8, 11, 11),
        c(-0.4, 1, 1, -0.4),
        col = '#f2f3f4', border = NA)
points(dat_model$wind_speed[sorter], 
       dat_model$detection[sorter] - rnorm(nrow(dat_model), 0.2, 0.05),
       pch = trans_subset[dat_model$subset[sorter]], 
       col = c('#c39bd3', # 2023 = purple
               '#f8c471', # 2024 = yellow
               '#d5dbdb'  # 2025 = grey (not included)
       )[dat_model$year[sorter]],
       cex = 0.8)
axis(1, seq(0, 30, 5))
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)

dev.off()