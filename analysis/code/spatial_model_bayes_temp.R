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
path_m_temp = 'analysis/code/models/m_temp.stan'
path_pdf = 'analysis/results/spatial_model/model_temp.pdf'

# Load
load(path_combined_data)

# Add Julian date as continuous
dat_model$julian_date = as.POSIXlt(dat_model$date)$yday + 1
dat_model$year = ifelse(str_detect(dat_model$date, '2023'), 1,
                        ifelse(str_detect(dat_model$date, '2024'), 2, 3))

# Subset of fall migration
dat_model = dat_model[dat_model$julian_date >= 214 & # '08-01'
                        dat_model$julian_date <= 289,] # '10-15'

# Make simple model with b-splines for temperature
num_knots = 3
degree = 3
knots = as.numeric(quantile(dat_model$mean_temp, 
                            probs = seq(0, 1, length.out = num_knots)))
B = bs(dat_model$mean_temp,
       knots = knots[-c(1, num_knots)],
       degree = degree, intercept = TRUE)
clean_dat = list(N_obs = nrow(dat_model),
                 N_knots = num_knots,
                 N_stations = dat_model$station |> unique() |> length(),
                 present = dat_model$detection |> as.integer(), 
                 B = B,
                 station = dat_model$station |> as.factor() |> as.integer())
model = cmdstan_model(path_m_temp)
fit = model$sample(data = clean_dat,
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4)
fit_nice = fit$output_files() %>%
  rstan::read_stan_csv()
post = fit_nice %>%
  rethinking::extract.samples()
fit_nice |> precis(depth = 2) |> round(2) |> print()

# Plot predictions
trans_subset = c(Buoys = 19,        # circle
                 Windturbines = 17, # triangle
                 SSO = 15)          # square
pdf(path_pdf, 6, 3.5)
par(mar = c(4, 4, 0.5, 1))
plot(dat_model[sample(nrow(dat_model)),]$mean_temp, 
     dat_model[sample(nrow(dat_model)),]$detection - 
       rnorm(nrow(dat_model), 0.2, 0.05),
     xaxt = 'n', yaxt = 'n',
     pch = trans_subset[dat_model$subset], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
             )[dat_model$year],
     cex = 0.8,
     xlab = 'Temperature [°C]', ylab = 'Probability presence per night')
axis(1, unique(round(dat_model$mean_temp), 0.5))
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)
sorter = order(dat_model$mean_temp)
temps = dat_model$mean_temp[sorter]
B_plot = B[sorter,]
pred = vapply(seq_along(post$a), function(i)
  vapply(seq_along(temps), function(j) post$a[i] + B_plot[j,] %*% post$w[i,],
         numeric(1)),
  numeric(length(temps)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, temps , col = '#AED6F1')
lines(temps, mean_pred, lwd = 3, col = '#1B4F72')
dev.off()




