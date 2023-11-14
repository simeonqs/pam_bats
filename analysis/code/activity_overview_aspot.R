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
path_data = 'aspot/results'
path_summaries = 'analysis/results/activity_overview/summaries'

# List files
files = list.files(path_data, pattern = '*.txt', 
                   recursive = TRUE, full.names = TRUE)

# Make summary per folder
folders = list.files(path_data, full.names = TRUE)
folders = folders[!folders %in% c('aspot/results/maybe_bats', 
                                  'aspot/results/defenitely_bats')]
for(folder in folders){
  detections = load.selection.tables(sprintf('%s/selection_tables', folder))
  ## get dates
  detections$DATE = detections$file |> 
    str_extract('\\d{8}') |> 
    as.Date(format = '%Y%m%d')
  ## make summary
  summary = detections |> 
    group_by(DATE) |>
    count()
  summary$station = gsub('.*(NS\\d+).*', '\\1', folder) |> 
    as.character()
  ## store summary data
  write.csv(summary, 
            sprintf('%s/aspot_%s.csv',
                    path_summaries,
                    basename(folder)),
            row.names = FALSE)
}

