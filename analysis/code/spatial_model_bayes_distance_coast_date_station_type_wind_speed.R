# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Bayesian version of model for distance to coast, date, station 
# type and wind speed.
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
path_m_dist_coast_date_station_type_wind_speed = 
  'analysis/code/models/m_dist_coast_date_station_type_wind_speed.stan'
path_pdf = paste0('analysis/results/spatial_model/model_dist_coast_',
                  'date_station_type_wind_speed.pdf')

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
model = cmdstan_model(path_m_dist_coast_date_station_type_wind_speed)
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

# Plot predictions ----
trans_subset = c(Buoys = 19,        # circle
                 Windturbines = 17, # triangle
                 OSS = 15)          # square
pdf(path_pdf, 16, 7)
par(mar = c(4, 4, 0.5, 1),
    mfrow = c(2, 3))
layout(matrix(c(1, 2, 3,
                4, 4, 4), 
              byrow = TRUE, nrow = 2, ncol = 3))

## plot temp early season (Aug 1st-15th) ----
subber = dat_model$julian_date %in% seq(214, 228)
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
B_plot = B[dat_model$julian_date == 220,][1,] # Aug 7th
pred = vapply(seq_along(post$a), function(i) {
  vapply(seq_along(dists), function(j) {
    post$a[i] + B_plot %*% post$w[i, ] - post$b_dist_coast[i] * dists[j]
  }, numeric(1))
}, numeric(length(dists)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, dists, col = '#AED6F1')
lines(dists, mean_pred, lwd = 3, col = '#1B4F72')

## plot temp mid season (Sep 1st-15th) ----
subber = dat_model$julian_date %in% seq(244, 258)
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
B_plot = B[dat_model$julian_date == 250,][1,] # Sep 7th
pred = vapply(seq_along(post$a), function(i) {
  vapply(seq_along(dists), function(j) {
    post$a[i] + B_plot %*% post$w[i, ] - post$b_dist_coast[i] * dists[j]
  }, numeric(1))
}, numeric(length(dists)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, dists, col = '#AED6F1')
lines(dists, mean_pred, lwd = 3, col = '#1B4F72')

## plot temp end season (Oct 1st-15th) ----
subber = dat_model$julian_date %in% seq(274, 288)
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
B_plot = B[dat_model$julian_date == 280,][1,] # Oct 7th
pred = vapply(seq_along(post$a), function(i) {
  vapply(seq_along(dists), function(j) {
    post$a[i] + B_plot %*% post$w[i, ] - post$b_dist_coast[i] * dists[j]
  }, numeric(1))
}, numeric(length(dists)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, dists, col = '#AED6F1')
lines(dists, mean_pred, lwd = 3, col = '#1B4F72')

## plot date ----
plot(dat_model$julian_date, 
     dat_model$detection - rnorm(nrow(dat_model), 0.2, 0.05),
     ylim = c(-0.41, 1.01),
     type = 'n',
     xaxt = 'n', yaxt = 'n',
     xlab = 'Night of year', ylab = 'Probability presence per night')
polygon(c(214, 214, 228, 228),
        c(-0.4, 1, 1, -0.4),
        col = '#f2f3f4', border = NA)
polygon(c(244, 244, 258, 258),
        c(-0.4, 1, 1, -0.4),
        col = '#f2f3f4', border = NA)
polygon(c(274, 274, 288, 288),
        c(-0.4, 1, 1, -0.4),
        col = '#f2f3f4', border = NA)
points(dat_model$julian_date, 
       dat_model$detection - rnorm(nrow(dat_model), 0.2, 0.05),
       pch = trans_subset[dat_model$subset], 
       col = c('#c39bd3', # 2023 = purple
               '#f8c471', # 2024 = yellow
               '#d5dbdb'  # 2025 = grey (not included)
       )[dat_model$year],
       cex = 0.8)
axis_dates = as.POSIXlt(c('2024-08-01', '2024-08-15', '2024-09-01', 
                          '2024-09-15', '2024-10-01', '2024-10-15'))$yday + 1
axis(1, axis_dates, c('Aug 1st', 'Aug 15th', 'Sep 1st', 'Sep 15th', 
                      'Oct 1st', 'Oct 15th'))
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)
dev.off()




