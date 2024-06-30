# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Plot spectrograms for all detections from folder. Also stores
# wavs clips in same folder.
# source('analysis/code/aspot_plot_specs_detections.R')
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('callsync', 'stringr', 'seewave', 'tuneR')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Settings
wing = 0.01 # how much to add before and after detection

# Paths 
# folder = 'Stadiloe'
# path_detections = sprintf(
#   '/home/au472091/Documents/results_aspot/%s/selection_tables', folder)
# path_audio = sprintf(
#   '/media/au472091/T7 Shield/LAND_winter_23/%s_all', folder)
# path_out = sprintf(
#   '/home/au472091/Documents/results_aspot/%s/specs_detections', folder)
path_detections = '/home/au472091/Downloads/noise_tables'
path_audio = '/media/au472091/cd5d3443-6980-4c41-96c9-fdfa2b7c264e/togter_all'
path_out = '/home/au472091/Downloads/specs_noise'

# Load selection tables
detections = load.selection.tables(path_detections)

# Function to plot specs
plot.spec = function(detection_row, path_audio = NULL, path_out = NULL){
  wave = readWave(sprintf('%s/%s.wav', path_audio, detection_row$file), 
                  from = detection_row$Begin.time..s. - wing, 
                  to = detection_row$End.time..s. + wing,
                  units = 'seconds')
  writeWave(wave, sprintf(sprintf('%s/%s_%s.wav', 
                                  path_out, 
                                  detection_row$file,
                                  detection_row$Selection)))
}

# Run function on all detections
lapply(seq_len(nrow(detections)), function(i) 
  plot.spec(detections[i,], path_audio, path_out))
