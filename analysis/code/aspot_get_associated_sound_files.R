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
path_detections = 'aspot/models/m38/selection_tables_1000'
path_audio = 
  '/media/au472091/T7 Shield/LOT_1_BØJER_DATA/T3-NS26_A_Spring23/Data'
path_output = 'analysis/data/audio/from_runs'

# Run through files and copy
detections = load.selection.tables(path_detections)
for(file in unique(detections$file)){
  file.copy(sprintf('%s/%s.wav', path_audio, file),
            sprintf('%s/%s.wav', path_output, file))
}




