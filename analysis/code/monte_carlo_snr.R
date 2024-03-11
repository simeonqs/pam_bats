# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Computes the SNR for each call in the ground truth tables.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'callsync', 'seewave', 'tuneR')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_selection_tables = 'aspot/test_data_sets/boeje_test_data/txt'
path_audio_files = 'aspot/test_data_sets/boeje_test_data/wav'
path_output = 'analysis/results/signal_to_noise_test_boejer.csv'

# Load selection_tables
selection_tables = load.selection.tables(path_selection_tables)

# Run through files and compute SNR
for(i in seq_len(nrow(selection_tables))){
  ## signal
  start = selection_tables$Begin.Time..s.[i]
  end = selection_tables$End.Time..s.[i]
  duration = selection_tables$End.Time..s.[i] - 
    selection_tables$Begin.Time..s.[i]
  ### if signal is shorter than 4 ms, buffer, following assumes some calls are
  ### very close to the start, but no calls are very close to the end of the 
  ### recording
  if(duration < 4/1000){
    missing = 4/1000 - duration
    if(start > missing/2) start = start - missing/2 else end = end + missing/2
    end = end + missing/2
  }
  wave = readWave(sprintf('%s/%s.wav', 
                          path_audio_files,
                          selection_tables$file[i]),
                  from = start,
                  to = end,
                  units = 'seconds')
  wave = ffilter(wave, from = 15000, output = 'Wave', wl = 512)
  squared = wave@left^2
  max_index = which(squared == max(squared))
  ### 1 ms before and after max 
  start = max_index - 1/1000*wave@samp.rate
  end = max_index + 1/1000*wave@samp.rate
  if(start < 1) start = 1
  if(end > length(wave@left)) end = length(wave@left)
  signal = sqrt(mean(squared[start:end]))
  ## noise
  ### if call is too close to start, pick noise after call, otherwise before
  start = ifelse(selection_tables$Begin.Time..s.[i] <= 3/1000,
                 selection_tables$End.Time..s.[i] - 1/1000,
                 selection_tables$Begin.Time..s.[i] - 3/1000)
  end = ifelse(selection_tables$Begin.Time..s.[i] <= 3/1000,
               selection_tables$End.Time..s.[i] + 3/1000,
               selection_tables$Begin.Time..s.[i] + 1/1000)
  wave = readWave(sprintf('%s/%s.wav', 
                          path_audio_files,
                          selection_tables$file[i]),
                  from = start,
                  to = end,
                  units = 'seconds')
  wave = ffilter(wave, from = 15000, output = 'Wave', wl = 512)
  squared = wave@left^2
  start = 1/1000*wave@samp.rate
  end = 3/1000*wave@samp.rate
  noise = sqrt(mean(squared[start:end]))
  selection_tables$SNR[i] = 20*log10(signal/noise)
}

# Write output
write.csv(selection_tables, path_output, row.names = FALSE)

# Message
message('Stored all results.')