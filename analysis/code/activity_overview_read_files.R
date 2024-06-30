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
path_data = '/media/au472091/T7 Shield/read_summaries'
path_summaries = 'analysis/results/activity_overview/summaries'

# List files
files = list.files(path_data, pattern = '*.txt', 
                   recursive = TRUE, full.names = TRUE)

# Run through files and process data
for(file in files){
  print(file)
  ## get station 
  station = file |> basename() |> 
    strsplit('_A') |> sapply(`[`, 1) |> 
    strsplit('_B') |> sapply(`[`, 1)
  ## get folder
  split = strsplit(file, '/')[[1]]
  folder = split[length(split)-1]
  ## read file
  dat = read.csv(file) 
  ## remove extra headers
  dat = dat[dat$DATE != 'DATE',] 
  ## make summary for file
  summary = dat |> 
    group_by(DATE) |>
    count() |> 
    na.omit()
  summary$station = station
  ## store summary data
  write.csv(summary, 
            sprintf('%s/summaries/%s_%s.csv',
                    path_summaries,
                    folder,
                    file |> basename() |> str_remove('_Summary.txt')),
            row.names = FALSE)
}

# Get detections
detections = list.files(path_data, pattern = '*.wav', 
                        recursive = TRUE, full.names = TRUE)
## get dates and station name from detections 
dat = data.frame(
  date = detections |> str_extract('\\d{8}') |> as.Date(format = '%Y%m%d'),
  station = detections |> basename() |> strsplit('_') |> sapply(`[`, 1))
for(station in unique(dat$station)){
  sub = dat[dat$station == station,]
  for(date in unique(sub$date)){
    summary_detections = data.frame(DATE = as.Date(date),
                                    station = station,
                                    n = nrow(sub[sub$date == date,]))
    ## store summary data
    write.csv(summary_detections, 
              sprintf('%s/detections/%s_%s.csv',
                      path_summaries,
                      station,
                      as.Date(date)),
              row.names = FALSE)
  }
}

# Message
message(sprintf('Processed %s file(s) and %s detection(s).', length(files),
                length(detections)))
