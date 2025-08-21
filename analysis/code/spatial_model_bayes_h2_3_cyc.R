# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Bayesian version of model for hypothesis 2_3.
# This version has cyclical smoother for date and wind direction. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'rethinking', 'splines', 'rstan', 'mgcv')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_combined_data = 'analysis/results/combined_data.RData'
path_m = 'analysis/code/models/m_h2_3_cyc.stan'
path_pdf = 'analysis/results/spatial_model/model_h2_3_cyc.pdf'

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
num_knots_date = 10
num_knots_temp = 3
num_knots_wind = 10
degree = 3
knots_temp = as.numeric(quantile(dat_model$mean_temp, 
                                 probs = seq(0, 1, 
                                             length.out = num_knots_temp)))
range_julian = range(dat_model$julian_date)
B_date_obj = smoothCon(s(julian_date, bs = 'cc', k = num_knots_date),
                       data = dat_model,
                       absorb.cons = TRUE)[[1]]
B_date = B_date_obj$X
B_temp = bs(dat_model$mean_temp,
            knots = knots_temp[-c(1, num_knots_temp)],
            degree = degree, intercept = TRUE)
B_wind_obj = smoothCon(s(wind_direction, bs = 'cc', k = num_knots_wind),
                       data = dat_model,
                       absorb.cons = TRUE)[[1]]
B_wind = B_wind_obj$X
unique_years = sort(unique(dat_model$year))

N_years = length(unique_years)
N_obs = nrow(dat_model)

unique_years = sort(unique(dat_model$year))
N_years = length(unique_years)
N_obs = nrow(dat_model)

B_year_date = array(0, dim = c(N_years, N_obs, num_knots_date-2))

for (j in seq_along(unique_years)) {
  year_val = unique_years[j]
  year_rows = which(dat_model$year == year_val)
  
  B = smoothCon(s(julian_date, bs = 'cc', k = num_knots_date),
                data = dat_model[year_rows, ],
                absorb.cons = TRUE)[[1]]$X
  B_year_date[j, year_rows, ] = B
}

clean_dat = list(N_obs = nrow(dat_model),
                 N_knots_date = num_knots_date-2,
                 N_knots_temp = num_knots_temp+2,
                 N_knots_wind = num_knots_wind-2,
                 N_stations = dat_model$station |> unique() |> length(),
                 present = dat_model$detection |> as.integer(), 
                 B_date = B_date,
                 B_temp = B_temp,
                 dist_coast = dat_model$distance_to_coast,
                 wind_speed = dat_model$wind_speed,
                 B_wind = B_wind,
                 year = dat_model$year,
                 B_year_date = B_year_date,
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

# Plot predictions ----
## we want the effect of date at 18 degrees, 20 km from the coast, 3 m/s and 
## no specific wind

## get spline for temp at ca. 18 degrees (18.003)
ads = abs(dat_model$mean_temp - 18)
B_temp_plot = B_temp[ads == min(ads),]

trans_subset = c(Buoys = 19,        # circle
                 Windturbines = 17, # triangle
                 OSS = 15)          # square
pdf(path_pdf, 6, 3.5)
par(mar = c(4, 4, 0.5, 1))
sorter = order(dat_model$julian_date)
plot(dat_model$julian_date[sorter], 
     dat_model$detection[sorter] - 
       rnorm(nrow(dat_model), 0.2, 0.05),
     xaxt = 'n', yaxt = 'n',
     pch = trans_subset[dat_model$subset[sorter]], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
     )[dat_model$year[sorter]],
     cex = 0.8,
     xlab = 'Night of year', ylab = 'Probability presence per night')
axis_dates = as.POSIXlt(c('2024-08-01', '2024-08-15', '2024-09-01', 
                          '2024-09-15', '2024-10-01', '2024-10-15'))$yday + 1
axis(1, axis_dates, c('Aug 1st', 'Aug 15th', 'Sep 1st', 'Sep 15th', 
                      'Oct 1st', 'Oct 15th'))
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)
nights = dat_model$julian_date[sorter]
B_plot = B_date[sorter,]
pred = vapply(seq_along(post$a), function(i)
  vapply(seq_along(nights), function(j) 
    return(post$a[i] + 
             B_plot[j,] %*% post$w_date[i,] +
             B_temp_plot %*% post$w_temp[i,] -
             post$b_wind_speed[i] * 3 -
             post$b_dist_coast[i] * 20),
    numeric(1)),
  numeric(length(nights)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, nights , col = '#AED6F1')
lines(nights, mean_pred, lwd = 3, col = '#1B4F72')
dev.off()

