# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Use classification from genera model to sort detections.
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
path_wavs = 'aspot/results/T3-NS26_A_Spring23/specs_detections'
path_classifiction = 'aspot/models_g/m06/selection_tables'
path_out = 'aspot/results/T3-NS26_A_Spring23/genera'

# List files
files = list.files(path_wavs, '*wav', full.names = TRUE)

# Get all classes and create dirs if they don't already exist
detections = load.selection.tables(path_classifiction)
for(type in c('', 'noise', unique(detections$Sound.type))){
  dir = sprintf('%s/%s', path_out, type)
  if(!file.exists(dir)) dir.create(dir)
}

# Run through all files and copy to correct folder
for(file in files){
  f = file |> basename() |> str_remove('\\.wav')
  d = load.selection.table(
    sprintf('%s/%s_predict_output.log.annotation.result.txt',
            path_classifiction, f))
  type = d$Sound.type
  if(length(type) == 0) type = 'noise'
  if(length(unique(type)) > 1) 
    warning(sprintf('Multiple types in %s.', f))
  file.copy(file, sprintf('%s/%s/%s.wav',
                          path_out, 
                          type[1], 
                          f))
}
message(sprintf('Moved %s files.', length(files)))

