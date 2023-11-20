# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Mixes two tapes. Zero-padds the end of the shortest tape.
# Stores the mixed version in the same folder as the first file with the 
# file name file_1+file_2.wav.
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
file_1 = 'aspot/test_data_sets/test_data_1_95/LAND9-LOT1_20230510_015740.wav'
file_2 = 'aspot/test_data_sets/test_data_1_95/LOT1-ONBOARD_20230427_191535.wav'

# Read files and mix
wav_1 = readWave(file_1)
wav_2 = readWave(file_2)
difference = length(wav_1@left) - length(wav_2@left)
# zero-pad shortest
if(difference > 0) wav_2@left = c(wav_2@left, rep(0, abs(difference))) else 
  wav_1@left = c(wav_1@left, rep(0, abs(difference)))
new_wav = wav_1
new_wav@left = new_wav@left + wav_2@left

# Save
base_1 = basename(file_1)
base_2 = basename(file_2)
location = str_remove(file_1, base_1)
writeWave(new_wav, 
          sprintf('%s%s+%s.wav',
                  location,
                  str_remove(base_1, '.wav'),
                  str_remove(base_2, '.wav')),
          extensible = FALSE)
          