# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Reads the durations from the log files and stores output as
# csv. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'lubridate', 'scales')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_data = 'aspot/results'
path_csv = 'analysis/results/activity_overview/summaries/durations'

# List files
files = list.files(path_data, pattern = '*.txt', 
                   recursive = TRUE, full.names = TRUE)

# Function to get duration from log file
read.log = function(lf){
  line_9 = readLines(lf, n = 9)[9]
  total_duration = as.numeric(sub('.*stop time=(\\d+).*', '\\1', line_9))
}

# Make summary per folder
folders = list.files(path_data, full.names = TRUE)
folders = folders[!folders %in% c('aspot/results/maybe_bats',
                                  'aspot/results/defenitely_bats')]
for(folder in folders){
  
  # List log files
  files = list.files(sprintf('%s/predict', folder), 
                     '*predict_output.log',
                     full.names = TRUE)
  out = data.frame(
    file = files |> basename() |> str_remove('_predict_output.log') )
  out$duration = files |> vapply(read.log, numeric(1))
  
  # Write output
  write.csv(out, sprintf('%s/%s.csv',
                         path_csv,
                         basename(folder)))
  
}

# Message
message(sprintf('Processed %s folder(s).', length(folders)))