# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Copy sound files that are needed to create training data from 
# selection tables to audio folder. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('callsync', 'stringr')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_detections = 
  '/media/au472091/cd5d3443-6980-4c41-96c9-fdfa2b7c264e/selection_tables_1000'
path_audio = 
  '/media/au472091/cd5d3443-6980-4c41-96c9-fdfa2b7c264e/togter_all'
path_output = 'analysis/data/audio/additional_noise_togter'

# Run through files and copy
detections = load.selection.tables(path_detections)
for(file in unique(detections$file)){
  file.copy(sprintf('%s/%s.wav', path_audio, file),
            sprintf('%s/%s.wav', path_output, file))
}




