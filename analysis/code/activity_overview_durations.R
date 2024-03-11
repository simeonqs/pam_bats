# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Reads the durations from the log files and stores output as
# csv. Outputs csv file per folder with four columns: duration = duration in
# seconds, n_windows = number of windows that were predicted on, station = 
# station name, file = name of the wav file. 
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
path_data = '/home/au472091/Documents/results_aspot'
path_csv = 'analysis/results/activity_overview/summaries/durations'

# List files
files = list.files(path_data, pattern = '*.txt', 
                   recursive = TRUE, full.names = TRUE)

# Function to get duration from log file
read.log = function(lf){
  line_8 = readLines(lf, n = 8)[8]
  line_9 = readLines(lf, n = 9)[9]
  n_windows = as.numeric(sub('.*=(\\d+).*', '\\1', line_8))
  duration = as.numeric(sub('.*stop time=(\\d+).*', '\\1', line_9))/100
  return(c(n_windows = n_windows, duration = duration))
}

# Make summary per folder
folders = list.files(path_data, full.names = TRUE)
folders = folders[!folders %in% 
                    c('/home/au472091/Documents/results_aspot/defenitely_bats',
                      '/home/au472091/Documents/results_aspot/maybe_bats',
                      '/home/au472091/Documents/results_aspot/noise')]
for(folder in folders){
  
  # List log files
  files = list.files(sprintf('%s/predict', folder), 
                     '*predict_output.log',
                     full.names = TRUE)
  out = data.frame(
    file = files |> basename() |> str_remove('_predict_output.log') )
  predict_infos = files |> vapply(read.log, numeric(2)) |> t() |> 
    as.data.frame()
  out$duration = predict_infos$duration
  out$n_windows = predict_infos$n_windows
  out$station = folder |> basename() |> 
    strsplit('_A') |> sapply(`[`, 1) |> 
    strsplit('_B') |> sapply(`[`, 1) |> 
    as.character()
  ## get season
  season = 'no_season'
  if(str_detect(folder, 'Fall')) season = 'fall'
  if(str_detect(folder, 'Spring')) season = 'spring'
  if(str_detect(folder, 'Summer')) season = 'summer'
  if(str_detect(folder, 'Spring')) season = 'spring'
  if(str_detect(folder, 'Summer')) season = 'summer'
  
  # Write output
  write.csv(out, sprintf('%s/%s_%s.csv',
                         path_csv,
                         basename(folder),
                         season),
            row.names = FALSE)
  
}

# Message
message(sprintf('Processed %s folder(s).', length(folders)))