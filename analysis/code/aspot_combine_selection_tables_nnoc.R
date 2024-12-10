# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Combines selection tables from classification and nnoc model.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'callsync')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_combined_selection_tables = 'aspot/models_s/m45/combined_selection_tables'
path_nnoc_selection_tables = 'aspot/models_s/m46/selection_tables'
path_out = 'aspot/models_s/m46/combined_selection_tables/sts_nnoc.csv'

# Read combined selection tables
sts_combined = load.selection.tables(path_combined_selection_tables)
sts_nnoc = load.selection.tables(path_nnoc_selection_tables)

# Check if the number of Nnoc selection tables matches the number of NVE 
# detections
nve_detects = which(sts_combined$Sound.type == 'NVE')
if(!length(list.files(path_nnoc_selection_tables)) == length(nve_detects))
  stop('Number NVE detections and Nnoc tables does not match!')

# Go through all NVE detections and fill out if Nnoc
sts_combined$Sound.type[nve_detects] = vapply(nve_detects, function(i){
  file = sts_combined$file[i]
  sel = sts_combined$Selection[i]
  fs = paste(file, sel, sep = '_')
  y = sts_nnoc$Sound.type[sts_nnoc$file == fs]
  if(length(y) == 0) return('VE') else return('Nnoc')
}, character(1))

# Write table
write.csv(sts_combined, path_out, row.names = FALSE)

