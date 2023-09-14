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
path_selections = 'analysis/data/aspot_selections/noise'
path_wavs = 'analysis/data'
path_results = 'analysis/results/aspot'

# Settings
type = 'noise'
bandpass = c(10000, 90000)
resample_rate = 192000

# List files
audio_files = c(list.files(path_wavs,  '*wav', full.names = TRUE, 
                           recursive = TRUE), 
                list.files(path_wavs,  '*WAV', full.names = TRUE, 
                           recursive = TRUE))

# Find the selection tables
selection_tables = list.files(path_selections, '*txt', full.names = TRUE, 
                              recursive = TRUE)

# The function for each selection
export.selection = function(selection, selection_table, wave, file_name){
  
  # Get start and end
  start = round((selection_table$Begin.Time..s.[selection]) * wave@samp.rate)
  if(length(start) == 0) 
    start = round((selection_table$Begin.time..s.[selection]) * wave@samp.rate)
  end = round((selection_table$End.Time..s.[selection]) * wave@samp.rate)
  if(length(end) == 0) 
    end = round((selection_table$End.time..s.[selection]) * wave@samp.rate)
  
  # Create new name 
  ID = round(runif(1) * 1e7)
  year = 2020
  tape_name = str_replace_all(file_name, '_', '-')
  start_name = round(start / wave@samp.rate * 1000)
  end_name = round(end / wave@samp.rate * 1000)
  new_name = paste0(type, '-bat', '_', ID, '_', year, '_', 
                    tape_name, '_', start_name, '_', end_name)
  
  # Create new wave and save
  new_wave = wave[start:end]
  orig_max = max(abs(new_wave@left))
  new_wave = ffilter(new_wave, from = bandpass[1], to = bandpass[2], 
                     output = 'Wave')
  new_wave@left = round(new_wave@left / max(abs(new_wave@left)) * orig_max)
  new_wave = downsample(new_wave, resample_rate)
  
  new_name = paste0(path_results, '/', type, '/', new_name, '.wav')
  if(length(new_wave@left)/new_wave@samp.rate > 0.0001)  
    writeWave(new_wave, new_name, extensible = FALSE)
  
}

# The running per selection table
export.selections = function(path_selection_table){
  
  # Read the table
  selection_table = read.csv(path_selection_table, sep = '\t')
  selection_table = 
    selection_table[str_detect(selection_table$View, 'Waveform'),]
  
  # Find wav file name
  file_name = path_selection_table |>
    basename() |>
    str_remove('.Table.1.selections.txt') |>
    str_remove('.Table.2.selections.txt') |>
    str_remove('_predict_output.log.annotation.result.txt')
  
  print(file_name)
  
  # Read the wave
  wave = readWave(audio_files[ grepl(paste0(file_name, '.wav'), audio_files,
                                     ignore.case = TRUE) ])
  
  # Export for each piece
  sapply(seq_len(nrow(selection_table)), export.selection, 
         selection_table, wave, file_name)
}

# Calls the function
sapply(selection_tables, export.selections)

# Message
message('Exported all selections.')