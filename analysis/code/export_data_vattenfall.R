# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Exports the data (HRIII - y1) for the Vattenfall project.  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Clean R
rm(list=ls()) 

# Paths 
path_combined_data = 'analysis/results/combined_data.RData'
path_out = 'analysis/results/dat_HRIII.RData'

# Load data
load(path_combined_data)
dat_HRIII = dat_model[dat_model$subset == 'Windturbines' &
                        dat_model$date <= as.Date('2024-04-10'),]

# Save
save(dat_HRIII, file = path_out)
