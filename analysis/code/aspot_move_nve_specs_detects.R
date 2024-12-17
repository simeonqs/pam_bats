# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Takes specs detections that are classified as NVE and copies
# them to a new folder to be classified by Nnoc model. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('callsync')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_specs_detects = 'aspot/models/m59/specs_detections'
path_combined_selection_tables = 'aspot/models_s/m45/combined_selection_tables'
path_out = 'aspot/models_s/m46/NVE_specs_detects_m59'

# Read selection tables
selection_tables = load.selection.tables(path_combined_selection_tables)

# Subset for NVE and copy files
sub = selection_tables[selection_tables$Sound.type == 'NVE',]
file.copy(sprintf('%s/%s_%s.wav', path_specs_detects, sub$file, sub$Selection),
          sprintf('%s/%s_%s.wav', path_out, sub$file, sub$Selection))
