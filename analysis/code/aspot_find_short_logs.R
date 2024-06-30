# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Finds log files that are too short at copies the wav file for 
# that log to a folder.
# source('analysis/code/aspot_find_short_logs.R')
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'callsync', 'parallel')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths and settings
station = 'Skagen'
type = 'segmentation'

if(type == 'classification'){
  path_logs = sprintf(
    '/home/au472091/Documents/results_aspot/%s/predict_species', station)
  path_wavs = sprintf(
    '/home/au472091/Documents/results_aspot/%s/specs_detections', station)
  path_out = '/media/au472091/T7 Shield/missed_land_classification'
} 
if(type == 'segmentation'){
  path_logs = sprintf(
    '/home/au472091/Documents/results_aspot/HRIII/predict', station)
  path_wavs = sprintf(
    '/media/au472091/T7 Shield/HRIII_all', station)
  path_out = '/media/au472091/T7 Shield/HRIII_missed'
}

# Create directory if not existing
if(!dir.exists(path_out)) dir.create(path_out)

# Go through files and find the ones that are too short
files = list.files(path_wavs, full.names = TRUE)
message('Running with ', length(files), ' selection tables:')
for(file in files){
  
  # Print progress
  message(which(file == files), '/', length(files))
  
  # Find log file
  # finder_thingy = paste0('/', str_remove(basename(file), '.wav'), '_') |>
  #   str_replace('\\+', '\\\\+')
  log_file = sprintf('%s/%s_predict_output.log',
                     path_logs, str_remove(basename(file), '.wav'))
  
  # If not found, copy wav file
  if(!file.exists(log_file)){
    file.copy(file, paste(path_out, basename(file), sep = '/'))
  } else {
    
    # Else if log file not long enough, copy wav file
    pred = readLines(log_file)
    stop_time = (pred[9] |> strsplit('=') |> sapply(`[`, 2) |> as.numeric())
    if(is.na(stop_time)) stop_time = 0
    if(type == 'classification') check_time = 9 + (stop_time-3)*12 else
      check_time = 9 + (stop_time-1)
    if(length(pred) != check_time){
      file.copy(file, paste(path_out, basename(file), sep = '/'))
    }
    
  } # end else if not found
  
} # end files loop

message(sprintf('Checked all files for %s.', station))









