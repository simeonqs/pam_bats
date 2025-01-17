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
path_m_night = 'analysis/code/models/m_night.stan'
path_pdf = 'analysis/results/spatial_model/model_date.pdf'

# Load
load(path_combined_data)

# Add Julian date as continuous
dat_model$julian_date = as.POSIXlt(dat_model$date)$yday + 1
dat_model$year = ifelse(str_detect(dat_model$date, '2023'), 1,
                        ifelse(str_detect(dat_model$date, '2024'), 2, 3))

# Subset of fall migration
dat_model = dat_model[dat_model$julian_date >= 228 & # '08-15'
                        dat_model$julian_date <= 289,] # '10-15'

# Make simple model with b-splines for night of year
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
                 B = B,
                 station = dat_model$station |> as.factor() |> as.integer())
model = cmdstan_model(path_m_night)
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
pdf(path_pdf, 6, 4)
plot(dat_model$julian_date, 
     dat_model$detection - rnorm(nrow(dat_model), 0.2, 0.05),
     pch = 15 + as.integer(as.factor(dat_model$subset)), 
     xaxt = 'n', yaxt = 'n',
     # grey = 2023, purple = 2024, orange = 2025
     col = c('#b3b6b7', '#bb8fce', '#f0b27a')[dat_model$year],
     xlab = 'Night of year', ylab = 'Probability presence per night')
axis_dates = as.POSIXlt(c('2024-08-15', '2024-09-01', '2024-09-15',
               '2024-10-01', '2024-10-15'))$yday + 1
axis(1, axis_dates, c('Aug 15th', 'Sep 1st', 'Sep 15th', 
                      'Oct 1st', 'Oct 15th'))
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)
nights = seq(min(dat_model$julian_date),
             max(dat_model$julian_date), 
             1)
B_plot = bs(nights,
            knots = knots[-c(1, num_knots)],
            degree = degree, intercept = TRUE)
pred = vapply(seq_along(post$a), function(i)
  vapply(seq_along(nights), function(j) post$a[i] + B_plot[j,] %*% post$w[i,],
         numeric(1)),
  numeric(length(nights)))
PI_pred = apply(pred, 1, PI) |> inv_logit()
mean_pred = apply(pred, 1, mean) |> inv_logit()
shade(PI_pred, nights , col = '#AED6F1')
lines(nights, mean_pred, lwd = 3, col = '#1B4F72')
dev.off()




