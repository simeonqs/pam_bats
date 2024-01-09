# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Reads the output from animal spot and stores csv files per
# station.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'lubridate', 'scales', 'callsync')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_data = '/home/au472091/Documents/results_aspot'
path_data_bats = '/home/au472091/Documents/results_aspot/defenitely_bats'
path_summaries = 'analysis/results/activity_overview/summaries'

# List files
files = list.files(path_data, pattern = '*.txt', 
                   recursive = TRUE, full.names = TRUE)

# Make summary per folder
folders = list.files(path_data, full.names = TRUE)
folders = folders[!folders %in% 
                    c('/home/au472091/Documents/results_aspot/defenitely_bats',
                      '/home/au472091/Documents/results_aspot/maybe_bats')]
for(folder in folders){
  detections = load.selection.tables(sprintf('%s/selection_tables', folder))
  ## get dates
  detections$DATE = detections$file |> 
    str_extract('\\d{8}') |> 
    as.Date(format = '%Y%m%d')
  ## get season
  season = 'no_season'
  if(str_detect(folder, 'Fall')) season = 'fall'
  if(str_detect(folder, 'Spring')) season = 'spring'
  if(str_detect(folder, 'Summer')) season = 'summer'
  ## make summary
  summary = detections |> 
    group_by(DATE) |>
    count() |> 
    na.omit()
  summary$station = folder |> basename() |> 
    strsplit('_A') |> sapply(`[`, 1) |> 
    strsplit('_B') |> sapply(`[`, 1) |> 
    as.character()
  ## store summary data
  write.csv(summary, 
            sprintf('%s/aspot/%s_%s.csv',
                    path_summaries,
                    basename(folder),
                    season),
            row.names = FALSE)
}

# Make summary per folder where bats were detected
folders = list.files(path_data_bats, full.names = TRUE)
for(folder in folders){
  detections = load.selection.tables(folder)
  ## get dates
  detections$DATE = detections$file |> 
    str_extract('\\d{8}') |> 
    as.Date(format = '%Y%m%d')
  ## get season
  season = 'no_season'
  if(str_detect(folder, 'Fall')) season = 'fall'
  if(str_detect(folder, 'Spring')) season = 'spring'
  if(str_detect(folder, 'Summer')) season = 'summer'
  ## make summary
  summary = detections |> 
    group_by(DATE) |>
    count() |> 
    na.omit()
  summary$station = folder |> basename() |> 
    strsplit('_A') |> sapply(`[`, 1) |> 
    strsplit('_B') |> sapply(`[`, 1) |> 
    as.character()
  ## store summary data
  write.csv(summary, 
            sprintf('%s/aspot_bats/%s_%s.csv',
                    path_summaries,
                    basename(folder),
                    season),
            row.names = FALSE)
}
