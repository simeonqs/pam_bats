# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Exports the data from y2 for the screening project.  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Clean R
rm(list=ls()) 

# Paths 
path_combined_data = 'analysis/results/combined_data.RData'
path_out = 'analysis/results/dat_model_NSI_y2.RData'

# Load data
load(path_combined_data)
dat_model_NSI_y2 = dat_model[dat_model$subset %in% c('Windturbines', 'Buoys', 
                                                     'OSS') &
                               dat_model$date > as.Date('2024-04-10'),]

# Save
save(dat_model_NSI_y2, file = path_out)
