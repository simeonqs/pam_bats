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

# Run land detections?
run_land = TRUE

# Paths 
path_data = '/media/au472091/T7 Shield/temp'
path_summaries = 'analysis/results/activity_overview/summaries'

# List files
files = list.files(path_data, pattern = '*.txt', 
                   recursive = TRUE, full.names = TRUE)

# Run through files and process data
for(file in files){
  ## get station 
  station = file |> basename() |> 
    strsplit('_A') |> sapply(`[`, 1) |> 
    strsplit('_B') |> sapply(`[`, 1)
  ## get season
  season = 'no_season'
  if(str_detect(file, 'Fall')) season = 'fall'
  if(str_detect(file, 'Spring')) season = 'spring'
  if(str_detect(file, 'Summer')) season = 'summer'
  ## read file
  dat = read.csv(file) 
  ## remove extra headers
  dat = dat[dat$DATE != 'DATE',] 
  ## make summary for each station
  summary = dat |> 
    group_by(DATE) |>
    count() |> 
    na.omit()
  summary$station = station
  ## store summary data
  write.csv(summary, 
            sprintf('%s/summaries/%s_%s.csv',
                    path_summaries,
                    file |> basename() |> str_remove('_Summary.txt'),
                    season),
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
    count() |> 
    na.omit()
  summary_detections$station = station
  ## store summary data
  write.csv(summary_detections, 
            sprintf('%s/detections/%s_%s.csv',
                    path_summaries,
                    file |> basename() |> str_remove('_Summary.txt'),
                    season),
            row.names = FALSE)
}

# Find detections for LAND
if(run_land){
  ## get detections
  folder = path_data
  detections = list.files(folder, pattern = '*.wav', 
                          recursive = TRUE, full.names = TRUE)
  ## get dates and station name from detections 
  dat_detections = data.frame(
    date = detections |> str_extract('\\d{8}') |> as.Date(format = '%Y%m%d')
  )
  ## create summary
  summary_detections = dat_detections |>
    group_by(date) |>
    count() |> 
    na.omit()
  summary_detections$station = station
  ## store summary data
  write.csv(summary_detections, 
            sprintf('%s/detections/%s_%s.csv',
                    path_summaries,
                    file |> basename() |> str_remove('_Summary.txt'),
                    season),
            row.names = FALSE)
}

# Message
message(sprintf('Processed %s file(s).', length(files)))
