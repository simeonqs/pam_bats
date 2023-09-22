# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Takes Raven selection tables and raw audio and creates audio
# clips with the correct file names for Animal Spot.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'seewave', 'tuneR')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_wavs = 'analysis/data/test_data'
path_results = 'analysis/results/test_data/downsampled'

# Settings
resample_rate = 192000
bandpass = c(15000, 90000)

# List audio files
files = list.files(path_wavs, recursive = TRUE, full.names = TRUE)

# Function to process file
downsample.file = function(file){
  wave = readWave(file)
  orig_max = max(abs(wave@left))
  wave = ffilter(wave, from = bandpass[1], to = bandpass[2], 
                     output = 'Wave')
  wave@left = round(wave@left / max(abs(wave@left)) * orig_max)
  wave = downsample(wave, resample_rate)
  writeWave(wave, sprintf('%s/%s', path_results, basename(file)))
}

# Run on all files
lapply(files, downsample.file)
message('Downsampled all files.')