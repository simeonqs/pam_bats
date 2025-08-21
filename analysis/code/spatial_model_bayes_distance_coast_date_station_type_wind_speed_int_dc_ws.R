# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Bayesian version of model for distance to coast, date, station 
# type and wind speed. This version has an interaction between distance to
# coast and wind speed. 
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
path_m_dist_coast_date_station_type_wind_speed_int_dc_ws = 
  paste0('analysis/code/models/m_dist_coast_date_station_type_wind_',
         'speed_int_dc_ws.stan')
path_pdf = paste0('analysis/results/spatial_model/model_dist_coast_',
                  'date_station_type_wind_speed_int_dc_ws.pdf')

# Load
load(path_combined_data)

# Add Julian date as continuous
dat_model$julian_date = as.POSIXlt(dat_model$date)$yday + 1
dat_model$year = ifelse(str_detect(dat_model$date, '2023'), 1,
                        ifelse(str_detect(dat_model$date, '2024'), 2, 3))

# Subset of fall migration
dat_model = dat_model[dat_model$julian_date >= 214 & # '08-01'
                        dat_model$julian_date <= 289,] # '10-15'

# Run model 
trans_type = c(Buoys = 1L, Windturbines = 2L, OSS = 3L)
num_knots = 10
degree = 3
knots = as.numeric(quantile(dat_model$julian_date, 
                            probs = seq(0, 1, length.out = num_knots)))
B = bs(dat_model$julian_date,
       knots = knots[-c(1, num_knots)],
       degree = degree, intercept = TRUE)
clean_dat = list(N_obs = nrow(dat_model),
                 N_knots = num_knots,
                 N_stations = dat_model$station |> unique() |> length(),
                 present = dat_model$detection |> as.integer(), 
                 dist_coast = dat_model$distance_to_coast,
                 B = B,
                 station_type = trans_type[dat_model$subset],
                 wind_speed = dat_model$wind_speed,
                 station = dat_model$station |> as.factor() |> as.integer())
model = cmdstan_model(path_m_dist_coast_date_station_type_wind_speed_int_dc_ws)
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
## the effect of distance to coast at varying levels of wind speed

### we want the effects at the peak in the migration season
sorter = order(dat_model$julian_date)
nights = dat_model$julian_date[sorter]
B_plot = B[sorter,]
pred = vapply(seq_along(post$a), function(i)
  vapply(seq_along(nights), function(j) 
    post$a[i] + B_plot[j,] %*% post$w[i,],
    numeric(1)),
  numeric(length(nights)))
mean_pred = apply(pred, 1, mean) |> inv_logit()
peak_dates = dat_model$julian_date[sorter][mean_pred == max(mean_pred)]
if(any(peak_dates != mean(peak_dates))) stop('Problem finding peak date.')
peak_date = peak_dates[1]
B_date_plot = B[dat_model$julian_date == peak_date,][1,]

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
subber = dat_model$wind_speed <= 3
sorter = order(dat_model$distance_to_coast[subber])
plot(dat_model$distance_to_coast[subber][sorter], 
     dat_model$detection[subber][sorter] - 
       rnorm(nrow(dat_model[subber,]), 0.2, 0.05),
     xaxt = 'n', yaxt = 'n',
     pch = trans_subset[dat_model$subset[subber][sorter]], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
     )[dat_model$year[subber][sorter]],
     cex = 0.8,
     xlim = range(dat_model$distance_to_coast),
     xlab = 'Distance to coast [km]', ylab = 'Probability presence per night')
axis(1, seq(round(min(dat_model$distance_to_coast)), 
            round(max(dat_model$distance_to_coast)),
            10))
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)

dists = seq(min(dat_model$distance_to_coast[subber]),
            max(dat_model$distance_to_coast[subber]),
            1)
pred = vapply(seq_along(post$a), function(i) {
  vapply(seq_along(dists), function(j) {
    post$a[i] + 
      B_date_plot %*% post$w[i, ] - 
      post$b_wind_speed[i] * 1.5 -
      post$b_dist_coast[i] * dists[j] +
      post$b_int[i] * 1.5 * dists[j]
  }, numeric(1))
}, numeric(length(dists)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, dists, col = '#AED6F1')
lines(dists, mean_pred, lwd = 3, col = '#1B4F72')

## plot dist coast medium wind (4-7) ----
subber = dat_model$wind_speed >= 4 & dat_model$wind_speed <= 7
sorter = order(dat_model$distance_to_coast[subber])
plot(dat_model$distance_to_coast[subber][sorter], 
     dat_model$detection[subber][sorter] - 
       rnorm(nrow(dat_model[subber,]), 0.2, 0.05),
     xaxt = 'n', yaxt = 'n',
     pch = trans_subset[dat_model$subset[subber][sorter]], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
     )[dat_model$year[subber][sorter]],
     cex = 0.8,
     xlim = range(dat_model$distance_to_coast),
     xlab = 'Distance to coast [km]', ylab = 'Probability presence per night')
axis(1, seq(round(min(dat_model$distance_to_coast)), 
            round(max(dat_model$distance_to_coast)),
            10))
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)

dists = seq(min(dat_model$distance_to_coast[subber]),
            max(dat_model$distance_to_coast[subber]),
            1)
pred = vapply(seq_along(post$a), function(i) {
  vapply(seq_along(dists), function(j) {
    post$a[i] + 
      B_date_plot %*% post$w[i, ] - 
      post$b_wind_speed[i] * 5.5 -
      post$b_dist_coast[i] * dists[j] +
      post$b_int[i] * 5.5 * dists[j]
  }, numeric(1))
}, numeric(length(dists)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, dists, col = '#AED6F1')
lines(dists, mean_pred, lwd = 3, col = '#1B4F72')

## plot dist coast high wind (8-11) ----
subber = dat_model$wind_speed >= 8 & dat_model$wind_speed <= 11
sorter = order(dat_model$distance_to_coast[subber])
plot(dat_model$distance_to_coast[subber][sorter], 
     dat_model$detection[subber][sorter] - 
       rnorm(nrow(dat_model[subber,]), 0.2, 0.05),
     xaxt = 'n', yaxt = 'n',
     pch = trans_subset[dat_model$subset[subber][sorter]], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
     )[dat_model$year[subber][sorter]],
     cex = 0.8,
     xlim = range(dat_model$distance_to_coast),
     xlab = 'Distance to coast [km]', ylab = 'Probability presence per night')
axis(1, seq(round(min(dat_model$distance_to_coast)), 
            round(max(dat_model$distance_to_coast)),
            10))
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)

dists = seq(min(dat_model$distance_to_coast[subber]),
            max(dat_model$distance_to_coast[subber]),
            1)
pred = vapply(seq_along(post$a), function(i) {
  vapply(seq_along(dists), function(j) {
    post$a[i] + 
      B_date_plot %*% post$w[i, ] - 
      post$b_wind_speed[i] * 9.5 -
      post$b_dist_coast[i] * dists[j] +
      post$b_int[i] * 9.5 * dists[j]
  }, numeric(1))
}, numeric(length(dists)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, dists, col = '#AED6F1')
lines(dists, mean_pred, lwd = 3, col = '#1B4F72')

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





