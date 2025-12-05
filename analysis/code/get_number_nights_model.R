# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Get the number of nights included in the model.  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_combined_data = 'analysis/results/combined_data.RData'
path_out = 'analysis/results/number_nights_model_per_year_and_station.csv'

# Load
load(path_combined_data)

# Summarise per station and year
dat_model$year = ifelse(dat_model$date < as.Date('2024-04-10'), 'Y1',
                  ifelse(dat_model$date < as.Date('2025-04-10'), 'Y2', NA))
dat_model = dat_model[!is.na(dat_model$year),]
sum_dat = dat_model |>
  group_by(year, station) |>
  summarise(n = length(unique(date)))

# Write output
write.csv(sum_dat, path_out, row.names = FALSE)
message('Done.')