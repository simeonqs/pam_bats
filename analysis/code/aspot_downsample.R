# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Takes Raven selection tables and raw audio and creates audio
# clips with the correct file names for Animal Spot.
# setwd('/home/au472091/Documents/au/projects/pam_bats')
# source('analysis/code/aspot_downsample.R')
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'seewave', 'tuneR', 'parallel')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_wavs = 
  '/media/au472091/T7 Shield/LOT_1_BØJER_DATA/NS13_A_Spring23/Data'
path_results = 
  '/media/au472091/T7 Shield/downsampled_data/NS13_A_Spring23'
path_predictions = 
  '/home/au472091/Documents/results_aspot/NS13_A_Spring23/predict'

# Settings
resample_rate = 192000
bandpass = c(1000, 95000)
debug = TRUE

# List audio files
files = list.files(path_wavs, '*wav', recursive = FALSE, full.names = TRUE)
set.seed(1)
# files = sample(files, 1000)

# Check which files are already downsampled and remove them from vector
already_done = list.files(path_results, '*wav')
already_predicted = list.files(path_predictions, '*.log') |>
  str_remove('_predict_output.log') |>
  paste0('.wav')
files = files[!basename(files) %in% c(already_done, already_predicted)]

# Function to process file
downsample.file = function(file){
  if(debug) print(file)
  wave = readWave(file)
  orig_max = max(abs(wave@left))
  wave = ffilter(wave, from = bandpass[1], to = bandpass[2], 
                     output = 'Wave')
  wave@left = round(wave@left / max(abs(wave@left)) * orig_max)
  wave = downsample(wave, resample_rate)
  writeWave(wave, sprintf('%s/%s', path_results, basename(file)))
}

# Run on all files
message(sprintf('Downsampling %s files...', length(files)))
if(!debug) mclapply(files, downsample.file, mc.cores = 15)
if(debug) lapply(files, downsample.file)
message('Downsampled all files.')

