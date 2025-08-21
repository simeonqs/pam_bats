# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Bayesian version of model for station type. We have buoys, 
# wind turbines and the transistor platform. 
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
path_m_station_type = 'analysis/code/models/m_station_type.stan'
path_m_station_type_dist_coast = 
  'analysis/code/models/m_station_type_dist_coast.stan'
path_pdf = 'analysis/results/spatial_model/model_station_type.pdf'
path_pdf_incl_dist_coast = 
  'analysis/results/spatial_model/model_station_type_dist_coast.pdf'

# Load
load(path_combined_data)

# Add Julian date as continuous
dat_model$julian_date = as.POSIXlt(dat_model$date)$yday + 1
dat_model$year = ifelse(str_detect(dat_model$date, '2023'), 1,
                        ifelse(str_detect(dat_model$date, '2024'), 2, 3))

# Subset of fall migration
dat_model = dat_model[dat_model$julian_date >= 214 & # '08-01'
                        dat_model$julian_date <= 289,] # '10-15'

# Run model ----
trans_type = c(Buoys = 1L, Windturbines = 2L, OSS = 3L)
clean_dat = list(N_obs = nrow(dat_model),
                 N_stations = dat_model$station |> unique() |> length(),
                 present = dat_model$detection |> as.integer(), 
                 station_type = trans_type[dat_model$subset],
                 station = dat_model$station |> as.factor() |> as.integer())
model = cmdstan_model(path_m_station_type)
fit = model$sample(data = clean_dat,
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4)
# fit_nice = fit$output_files() %>%
#   rstan::read_stan_csv()
# post = fit_nice %>%
#   rethinking::extract.samples()
t = fit$summary()
print(t, n = 100)
extract.samples.cmdstanr <- function(fit_obj) {
  vars <- fit_obj$metadata()$stan_variables
  draws <- posterior::as_draws_rvars(fit_obj$draws())
  
  lapply(vars, \(var_name){  
    posterior::draws_of(draws[[var_name]], with_chains = FALSE)
  }) |> setNames(vars)
}
post = extract.samples.cmdstanr(fit)
# fit_nice |> precis(depth = 2) |> round(2) |> print()

# Plot predictions
trans_subset = c(Buoys = 19,        # circle
                 Windturbines = 17, # triangle
                 OSS = 15)          # square
dat_model = dat_model[sample(nrow(dat_model)),] # randomise order
pdf(path_pdf, 6, 3.5)
par(mar = c(4, 4, 0.5, 1))
plot(trans_type[dat_model$subset] + rnorm(nrow(dat_model), 0, 0.1), 
     dat_model$detection - rnorm(nrow(dat_model), 0.2, 0.05),
     xaxt = 'n', yaxt = 'n',
     pch = trans_subset[dat_model$subset], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
     )[dat_model$year],
     cex = 0.8,
     xlab = 'Station type', ylab = 'Probability presence per night')
axis(1, trans_type, names(trans_type))
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)
PIs = post$a_type |> apply(2, PI) |> inv_logit()
means = post$a_type |> apply(2, mean) |> inv_logit()
for(i in 1:3){
  points(i, means[i], cex = 1.5, pch = 16)
  lines(rep(i, 2), PIs[,i], lwd = 2)
}
dev.off()

