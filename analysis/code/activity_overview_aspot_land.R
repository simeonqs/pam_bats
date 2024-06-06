# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Reads the output from animal spot and stores csv files per
# station. 
# This version reads the classified tables for the land stations. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'lubridate', 'callsync')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_summaries = 'analysis/results/activity_overview/summaries'
folders = c('/home/au472091/Documents/results_aspot/Ballum', 
            '/home/au472091/Documents/results_aspot/Blaavand', 
            '/home/au472091/Documents/results_aspot/Fanoe', 
            '/home/au472091/Documents/results_aspot/Husby', 
            '/home/au472091/Documents/results_aspot/Kammerslusen', 
            '/home/au472091/Documents/results_aspot/Mandoe', 
            '/home/au472091/Documents/results_aspot/Nyminde', 
            '/home/au472091/Documents/results_aspot/Rejsby', 
            '/home/au472091/Documents/results_aspot/Roemoe', 
            '/home/au472091/Documents/results_aspot/Skagen', 
            '/home/au472091/Documents/results_aspot/Skjern', 
            '/home/au472091/Documents/results_aspot/Stadiloe')

# Make summary per folder where bats detected
for(folder in folders){
  detections = load.selection.tables(
    sprintf('%s/selection_tables_species', folder))
  detections = detections[detections$Sound.type != 'noise',]
  ## get dates
  detections$DATE = detections$file |> 
    str_extract('\\d{8}') |> 
    as.Date(format = '%Y%m%d')
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
            sprintf('%s/aspot_bats/%s.csv',
                    path_summaries,
                    basename(folder)),
            row.names = FALSE)
}

