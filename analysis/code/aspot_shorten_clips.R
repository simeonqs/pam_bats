# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Remove wings for species classifications.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('callsync', 'stringr', 'seewave', 'tuneR')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
path_in = 'aspot/results/T3-NS26_A_Spring23/specs_detections'
path_out = 'aspot/results/T3-NS26_A_Spring23/specs_detections_short'

# Run through files and shorten
for(file in list.files(path_in, full.names = TRUE)){
  wave = readWave(file)
  start = 0.01 * wave@samp.rate
  end = length(wave@left) - 0.01 * wave@samp.rate
  writeWave(wave[start:end],
            file = str_replace(file, 'specs_detections', 
                               'specs_detections_short'),
            extensible = FALSE)
}