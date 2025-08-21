# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Bayesian version of model for night of year.
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
path_m_date_temp = 'analysis/code/models/m_date_temp.stan'
path_pdf = 'analysis/results/spatial_model/model_date_temp.pdf'

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
                 B_date = B_date,
                 B_temp = B_temp,
                 station = dat_model$station |> as.factor() |> as.integer())
model = cmdstan_model(path_m_date_temp)
fit = model$sample(data = clean_dat,
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4)
fit_nice = fit$output_files() %>%
  rstan::read_stan_csv()
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
sorter = order(dat_model$mean_temp[subber])
plot(dat_model$mean_temp[subber][sorter], 
     dat_model$detection[subber][sorter] - 
       rnorm(nrow(dat_model[subber,]), 0.2, 0.05),
     xaxt = 'n', yaxt = 'n',
     pch = trans_subset[dat_model$subset[subber]], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
     )[dat_model$year[subber]],
     cex = 0.8,
     xlim = range(dat_model$mean_temp),
     xlab = 'Temperature [°C]', ylab = 'Probability presence per night')
axis(1, unique(round(dat_model$mean_temp), 0.5))
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)

temps = dat_model$mean_temp[subber][sorter]
B_date_plot = B_date[dat_model$julian_date == 220,][1,]
B_temp_plot = B_temp[subber,][sorter,]
pred = vapply(seq_along(post$a), function(i)
  vapply(seq_len(nrow(B_temp_plot)), function(j) {
    smooth_date = sum(B_date_plot * post$w_date[i, ])
    smooth_temp = sum(B_temp_plot[j, ] * post$w_temp[i, ])
    smooth_date_temp = sum(
      B_date_plot %*% post$w_date_temp[i,,] %*% B_temp_plot[j, ]
    )
    post$a[i] + smooth_date + smooth_temp + smooth_date_temp
  }, numeric(1)),
  numeric(nrow(B_temp_plot))
)
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, temps , col = '#AED6F1')
lines(temps, mean_pred, lwd = 3, col = '#1B4F72')

## plot temp mid season (Sep 1st-15th) ----
subber = dat_model$julian_date %in% seq(244, 258)
sorter = order(dat_model$mean_temp[subber])
plot(dat_model$mean_temp[subber][sorter], 
     dat_model$detection[subber][sorter] - 
       rnorm(nrow(dat_model[subber,]), 0.2, 0.05),
     xaxt = 'n', yaxt = 'n',
     pch = trans_subset[dat_model$subset[subber]], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
     )[dat_model$year[subber]],
     cex = 0.8,
     xlim = range(dat_model$mean_temp),
     xlab = 'Temperature [°C]', ylab = 'Probability presence per night')
axis(1, unique(round(dat_model$mean_temp), 0.5))
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)

temps = dat_model$mean_temp[subber][sorter]
B_date_plot = B_date[dat_model$julian_date == 250,][1,]
B_temp_plot = B_temp[subber,][sorter,]
pred = vapply(seq_along(post$a), function(i)
  vapply(seq_len(nrow(B_temp_plot)), function(j) {
    smooth_date = sum(B_date_plot * post$w_date[i, ])
    smooth_temp = sum(B_temp_plot[j, ] * post$w_temp[i, ])
    smooth_date_temp = sum(
      B_date_plot %*% post$w_date_temp[i,,] %*% B_temp_plot[j, ]
    )
    post$a[i] + smooth_date + smooth_temp + smooth_date_temp
  }, numeric(1)),
  numeric(nrow(B_temp_plot))
)
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, temps , col = '#AED6F1')
lines(temps, mean_pred, lwd = 3, col = '#1B4F72')

## plot temp end season (Oct 1st-15th) ----
subber = dat_model$julian_date %in% seq(274, 288)
sorter = order(dat_model$mean_temp[subber])
plot(dat_model$mean_temp[subber][sorter], 
     dat_model$detection[subber][sorter] - 
       rnorm(nrow(dat_model[subber,]), 0.2, 0.05),
     xaxt = 'n', yaxt = 'n',
     pch = trans_subset[dat_model$subset[subber]], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
     )[dat_model$year[subber]],
     cex = 0.8,
     xlim = range(dat_model$mean_temp),
     xlab = 'Temperature [°C]', ylab = 'Probability presence per night')
axis(1, unique(round(dat_model$mean_temp), 0.5))
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)

temps = dat_model$mean_temp[subber][sorter]
B_date_plot = B_date[dat_model$julian_date == 280,][1,]
B_temp_plot = B_temp[subber,][sorter,]
pred = vapply(seq_along(post$a), function(i)
  vapply(seq_len(nrow(B_temp_plot)), function(j) {
    smooth_date = sum(B_date_plot * post$w_date[i, ])
    smooth_temp = sum(B_temp_plot[j, ] * post$w_temp[i, ])
    smooth_date_temp = sum(
      B_date_plot %*% post$w_date_temp[i,,] %*% B_temp_plot[j, ]
    )
    post$a[i] + smooth_date + smooth_temp + smooth_date_temp
  }, numeric(1)),
  numeric(nrow(B_temp_plot))
)
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, temps , col = '#AED6F1')
lines(temps, mean_pred, lwd = 3, col = '#1B4F72')

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



