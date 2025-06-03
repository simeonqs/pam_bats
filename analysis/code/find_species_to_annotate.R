# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Finds all files that haven't been annotated yet. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
path_combined_data = 'analysis/results/combined_data.RData'
path_out = '~/Desktop/missing_annotations.csv'

# Load data 
load(path_combined_data)

# Find all files with bats
files_bats = unique(dat$file_name[dat$n_bats > 0 & dat$offshore])

# Find which are missing
missing = files_bats[!files_bats %in% species_offshore$Fil]

# Write to csv
write.csv(data.frame(Fil = missing), path_out, row.names = FALSE)