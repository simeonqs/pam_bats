# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Generates long-term spectrogram to detect microphone failure. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'lubridate', 'scales', 'callsync', 'tuneR',
              'seewave', 'oce')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
folder = '/media/au472091/backup_1/NS16_B'

# List files
files = list.files(folder, full.names = TRUE)

# Settings 
window_length = 1e3
n_samples = 3

# Function to load wave and create average spectrum 
create.av.spec = function(file){
  wave = readWave(file)
  duration = wave@left |> length()
  starts = sample(duration - window_length, n_samples)
  spectra = vapply(starts, function(start) 
    log(spec(wave[start:(start+window_length)], plot = FALSE)[,2]),
    numeric(500)) 
  return(rowSums(spectra))
}

# Get dates and keep one file per hour
dts = files |> str_extract('\\d{8}_\\d{6}') |> 
  as.POSIXct(format = '%Y%m%d_%H%M%S') |> round('hours') 
keep = !duplicated(dts)
files = files[keep]
dts = dts[keep]
dates = as.Date(dts)

# Create spectra
spectra = files |> vapply(create.av.spec, numeric(500))

# Create long-term spectrogram
wave = readWave(files[1])
s = spec(wave[1:(1+window_length)])
imagep(t(spectra), drawPalette = FALSE, axes = FALSE)
index = seq(1, length(dts), length.out = 5)
axis(1, index, dates[index])
index = seq(1, nrow(s), 100)
axis(2, index, round(s[index,1]))
mtext('frequency [kHz]', 2, 2)
