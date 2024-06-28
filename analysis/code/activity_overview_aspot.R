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
                      '/home/au472091/Documents/results_aspot/maybe_bats',
                      '/home/au472091/Documents/results_aspot/noise')]
# for(folder in folders){
#   detections = load.selection.tables(sprintf('%s/selection_tables', folder))
#   if(nrow(detections) == 0) next
#   ## get dates
#   detections$DATE = detections$file |> 
#     str_extract('\\d{8}') |> 
#     as.Date(format = '%Y%m%d')
#   ## make summary
#   detections$station = detections$file |> strsplit('_') |> sapply(`[`, 1)
#   summary = detections |> 
#     group_by(DATE, station) |>
#     count() |> 
#     na.omit()
#   ## store summary data
#   write.csv(summary, 
#             sprintf('%s/aspot/%s.csv',
#                     path_summaries,
#                     basename(folder)),
#             row.names = FALSE)
# }

# Make summary per folder where bats were detected
folders = list.files(path_data_bats, full.names = TRUE)
for(folder in folders){
  detections = load.selection.tables(folder)
  ## get dates
  detections$DATE = detections$file |> 
    str_extract('\\d{8}') |> 
    as.Date(format = '%Y%m%d')
  ## make summary
  detections$station = detections$file |> strsplit('_') |> sapply(`[`, 1)
  summary = detections |> 
    group_by(DATE, station) |>
    count() |> 
    na.omit()
  ## store summary data
  write.csv(summary, 
            sprintf('%s/aspot_bats/%s.csv',
                    path_summaries,
                    basename(folder)),
            row.names = FALSE)
}
