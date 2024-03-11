# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Runs GAM on detected bat calls.  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'mgcv')
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
fit = gam(present ~ s(Lat..N., Long..E., k = 5), 
          data = dat_model, familiy = binomial)

summary(fit)

# Store fit
save(fit, dat_model, file = path_results)