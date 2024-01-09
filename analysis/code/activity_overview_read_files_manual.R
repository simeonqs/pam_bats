# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Summarises the detections for a folder without summary file.
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
path_summaries = 'analysis/results/activity_overview/summaries'
path_main_folder = '/media/au472091/T7 Shield/temp'
path_sub_folder = 'NS35_B_Spring23'
path_inner_folder = 'NS35_B_Spring23'

# Run manual folder
folder = paste(path_main_folder, path_sub_folder, path_inner_folder, 
               sep = '/')
station = path_sub_folder |> basename() |> 
  strsplit('_A') |> sapply(`[`, 1) |> 
  strsplit('_B') |> sapply(`[`, 1)
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
          sprintf('%s/detections/%s.csv',
                  path_summaries,
                  path_sub_folder),
          row.names = FALSE)
message('Done.')
