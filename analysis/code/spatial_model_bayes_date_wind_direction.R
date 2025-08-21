# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Bayesian version of model for night of year and wind direction. 
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
path_m_date_wind_dir = 'analysis/code/models/m_date_wind_dir.stan'
path_pdf = 'analysis/results/spatial_model/model_date_wind_dir.pdf'

# Load
load(path_combined_data)

# Add Julian date as continuous
dat_model$julian_date = as.POSIXlt(dat_model$date)$yday + 1
dat_model$year = ifelse(str_detect(dat_model$date, '2023'), 1,
                        ifelse(str_detect(dat_model$date, '2024'), 2, 3))

# Subset of fall migration
dat_model = dat_model[dat_model$julian_date >= 214 & # '08-01'
                        dat_model$julian_date <= 289,] # '10-15'


# Make simple model with b-splines for night of year
num_knots_date = 10
degree = 3
knots_date = as.numeric(quantile(dat_model$julian_date, 
                                 probs = seq(0, 1, 
                                             length.out = num_knots_date)))
B_date = bs(dat_model$julian_date,
            knots = knots_date[-c(1, num_knots_date)],
            degree = degree, intercept = TRUE)
clean_dat = list(N_obs = nrow(dat_model),
                 N_knots_date = num_knots_date,
                 N_stations = dat_model$station |> unique() |> length(),
                 present = dat_model$detection |> as.integer(), 
                 B_date = B_date,
                 wind_dir = dat_model$wind_direction,
                 station = dat_model$station |> as.factor() |> as.integer())
model = cmdstan_model(path_m_date_wind_dir)
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
pdf(path_pdf, 4.4, 3)
par(mar = c(4, 4, 0.5, 1))

### we want the effect of wind direction at the peak in the migration season
sorter = order(dat_model$julian_date)
nights = dat_model$julian_date[sorter]
B_plot = B_date[sorter,]
pred = vapply(seq_along(post$a), function(i)
  vapply(seq_along(nights), function(j) 
    post$a[i] + B_plot[j,] %*% post$w_date[i,],
    numeric(1)),
  numeric(length(nights)))
mean_pred = apply(pred, 1, mean) |> inv_logit()
peak_dates = dat_model$julian_date[sorter][mean_pred == max(mean_pred)]
if(any(peak_dates != mean(peak_dates))) stop('Problem finding peak date.')
peak_date = peak_dates[1]
B_date_plot = B_date[dat_model$julian_date == peak_date,][1,]
reorder = order(dat_model$wind_direction)
plot(dat_model[reorder,]$wind_direction, 
     dat_model[reorder,]$detection - 
       rnorm(nrow(dat_model), 0.2, 0.05),
     xaxt = 'n', yaxt = 'n',
     pch = trans_subset[dat_model$subset[reorder]], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
     )[dat_model$year[reorder]],
     cex = 0.8,
     xlab = 'Wind direction', ylab = 'Probability presence per night')
tick_pos = seq(0, 360, by = 45)
tick_labels = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'N')
axis(1, at = tick_pos, labels = tick_labels)
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)
sorter = order(dat_model$wind_direction)
dirs = unique(dat_model$wind_direction[sorter])
pred = vapply(seq_along(post$a), function(i)
  vapply(seq_along(dirs), function(j){
    wind_sin = sin(dirs[j] * pi / 180);
    wind_cos = cos(dirs[j] * pi / 180);
    return(post$a[i] +
             B_date_plot %*% post$w_date[i,] +
             post$b_sin[i] * wind_sin +
             post$b_cos[i] * wind_cos)
  }, numeric(1)),
  numeric(length(dirs)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, dirs , col = '#AED6F1')
lines(dirs, mean_pred, lwd = 3, col = '#1B4F72')

dev.off()



