# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Takes Raven selection tables and raw audio and creates audio
# clips with the correct file names for Animal Spot.
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
path_wavs = '/media/au472091/T7 Shield/NS6C_A_Spring23_Backup/Data'
path_results = '/home/au472091/Documents/test_data_bojer'

# Settings
resample_rate = 192000
bandpass = c(10000, 90000)

# List audio files
files = list.files(path_wavs, '*wav', recursive = FALSE, full.names = TRUE)
set.seed(1)
files = sample(files, 1000)

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
mclapply(files, downsample.file, mc.cores = 4)
message('Downsampled all files.')