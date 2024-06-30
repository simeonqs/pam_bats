# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Lists selection tables. Takes ones with most entries. Finds
# associated wav files and copies to folder. 
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
path_predictions = '/home/au472091/Desktop/selection_tables'
path_wavs = 
  '/home/au472091/Documents/large_data/noise_land_3_copy'
path_out = '/home/au472091/Desktop'

# List files
files = list.files(path_predictions, full.names = TRUE)
file_infos = file.info(files)
sizes = file_infos$size

# Copy wavs to desktop
n = 201
to_copy = files[order(sizes, decreasing = TRUE)][n:(n+50)]
wav_names = to_copy |> basename() |> 
  str_remove('_predict_output.log.annotation.result.txt')
file.copy(sprintf('%s/%s.wav', path_wavs, wav_names),
          sprintf('%s/%s.wav', path_out, wav_names))
