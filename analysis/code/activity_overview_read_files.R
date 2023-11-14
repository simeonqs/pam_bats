# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Reads the files and outputs txt files with data needed for 
# plotting.
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
path_data = '/media/au472091/T7 Shield/LOT_1_BØJER_DATA'
path_summaries = 'analysis/results/activity_overview/summaries'

# List files
files = list.files(path_data, pattern = '*.txt', 
                   recursive = TRUE, full.names = TRUE)

# Run through files and process data
for(file in files){
  ## get station 
  station = file |> vapply(function(x) 
    gsub('.*(NS\\d+).*', '\\1', x), character(1)) |> as.character()
  ## read file
  dat = read.csv(file) 
  ## remove extra headers
  dat = dat[dat$DATE != 'DATE',] 
  ## make summary for each station
  summary = dat |> 
    group_by(DATE) |>
    count()
  summary$station = station
  ## store summary data
  write.csv(summary, 
            sprintf('%s/%s.csv',
                    path_summaries,
                    file |> basename() |> str_remove('_Summary.txt')),
            row.names = FALSE)
  ## get detections
  folder = str_remove(file, basename(file))
  detections = list.files(folder, pattern = '*.wav', 
                          recursive = TRUE, full.names = TRUE)
  ## get dates and station name from detections 
  dat_detections = data.frame(
    date = detections |> str_extract('\\d{8}') |> as.Date(format = '%Y%m%d')
  )
  ## create summary
  summary_detections = dat_detections |>
    group_by(date) |>
    count()
  summary_detections$station = station
  ## store summary data
  write.csv(summary, 
            sprintf('%s/detections_%s.csv',
                    path_summaries,
                    file |> basename() |> str_remove('_Summary.txt')),
            row.names = FALSE)
}

# Message
message(sprintf('Processed %s files.', length(files)))
