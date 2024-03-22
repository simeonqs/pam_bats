# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Runs GAM on detected bat calls.  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'mgcv') # gam
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_dat_model = 'analysis/results/spatial_model/dat_model.RData'
path_results = 'analysis/results/spatial_model/fit_and_dat_model.Data'

# Load
load(path_dat_model)

# Fit GAM
fit = gam(present ~ s(Lat..N., Long..E., k = 5) + (1|station),
          data = dat_model, familiy = binomial)
# fit = gam(present ~ s(Lat..N., Long..E.) + (1|station), 
#           data = dat_model, familiy = binomial)
summary(fit)

# Day of year
# fit_year = gam(present ~ s(Lat..N., Long..E., k = 5) + s(day_of_year, k = 4), 
#                data = dat_model, familiy = binomial, link = 'logit')
dat_model$station = as.factor(dat_model$station)
fit_year = gam(present ~ s(day_of_year) + s(station, bs = 're'), 
               data = dat_model, familiy = binomial, link = 'logit')
summary(fit_year)
plot(dat_model$day_of_year, 
     dat_model$present + rnorm(nrow(dat_model), 0, 0.02),
     pch = 16, col = '#AED6F1',
     xlab = 'night of year', ylab = 'probability presence per night')
# new_data = data.frame(
#   day_of_year = seq(from = min(dat_model$day_of_year), 
#                     to = max(dat_model$day_of_year),
#                     by = 0.1),
#   Lat..N. = median(dat_model$Lat..N.),
#   Long..E. = median(dat_model$Long..E.))
new_data = data.frame(
    day_of_year = seq(from = min(dat_model$day_of_year),
                      to = max(dat_model$day_of_year),
                      by = 0.1),
    station = 'HR3_4')
preds_new = predict(fit_year, newdata = new_data, se.fit = FALSE,
                    type = 'response')
# pred_df_new = data.frame(
#   day_of_year = new_data$day_of_year,
#   pred = preds_new$fit,
#   lower = preds_new$fit - 1.96 * preds_new$se.fit,
#   upper = preds_new$fit + 1.96 * preds_new$se.fit
# )
# polygon(c(pred_df_new$day_of_year, rev(pred_df_new$day_of_year)),
#         c(pred_df_new$lower, rev(pred_df_new$upper)), 
#         col = '#5DADE2', border = NA)
# lines(pred_df_new$day_of_year, pred_df_new$pred, col = '#21618C', lwd = 3)
lines(new_data$day_of_year, preds_new, col = '#21618C', lwd = 3)

# Temperature
fit_temp = gam(present ~ s(Lat..N., Long..E., k = 5) + temp, 
               data = dat_model, familiy = binomial)
summary(fit_temp)
plot(dat_model$temp, dat_model$present + rnorm(nrow(dat_model), 0, 0.02))

# Store fit
save(fit, dat_model, file = path_results)