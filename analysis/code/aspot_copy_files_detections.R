# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Copy all files that have at least one detection in Animal Spot
# to folder for Signe to run Sonochiro.
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
folder = 'Ballum'
path_aspot = sprintf('~/Documents/results_aspot/%s', folder)
path_audio = sprintf('/media/au472091/T7 Shield/temp/%s_all', folder)
path_output = sprintf('/media/au472091/T7 Shield/for_signe/%s', folder)

# Load data
aspot = load.selection.tables(path_aspot, recursive = TRUE)

# Create directories
if(!file.exists(path_output)) dir.create(path_output, recursive = TRUE)

# Run through files and copy
copied = lapply(unique(aspot$file), function(file)
  file.copy(sprintf('%s/%s.wav', path_audio, file),
            sprintf('%s/%s.wav', path_output, file)))

# Message
message(sprintf('Copied %s files from %s.', 
                length(unique(aspot$file)), 
                folder))