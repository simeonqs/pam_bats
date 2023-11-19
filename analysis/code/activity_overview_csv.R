# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Reads all meta data and creates csv with overview.
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
path_summaries = 'analysis/results/activity_overview/summaries/summaries'
path_detections = 'analysis/results/activity_overview/summaries/detections'
path_aspot = 'analysis/results/activity_overview/summaries/aspot'
path_aspot_bats = 'analysis/results/activity_overview/summaries/aspot_bats'
path_durations = 'analysis/results/activity_overview/summaries/durations'

# List files
files_summaries = list.files(path_summaries, pattern = '*.csv', 
                             recursive = TRUE, full.names = TRUE)
files_detections = list.files(path_detections, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)
files_aspot = list.files(path_aspot, pattern = '*.csv', 
                         recursive = TRUE, full.names = TRUE)
files_aspot_bats = list.files(path_aspot_bats, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)
files_durations = list.files(path_durations, pattern = '*.csv', 
                             recursive = TRUE, full.names = TRUE)

# Read files
summary = files_summaries |>
  lapply(read.csv) |> bind_rows()
summary_detections = files_detections |>
  lapply(read.csv) |> bind_rows()
summary_aspot = files_aspot |>
  lapply(read.csv) |> bind_rows()
summary_aspot_bats = files_aspot_bats |>
  lapply(read.csv) |> bind_rows()
durations = files_durations |>
  lapply(read.csv) |> bind_rows()

# Create overview
overview = data.frame(station = unique(summary$station))
## add total number files
overview$n_files = vapply(overview$station, function(station)
  sum(summary_detections$n[summary_detections$station == station]), numeric(1))
## add total number detections
overview$n_detections = vapply(overview$station, function(station)
  sum(summary_aspot$n[summary_aspot$station == station]), numeric(1))
## add total number of bat detections
overview$n_bats = vapply(overview$station, function(station)
  sum(summary_aspot_bats$n[summary_aspot_bats$station == station]), numeric(1))
## add duration
overview$total_duration_h = vapply(overview$station, function(station)
  sum(durations$duration[durations$station == station]), numeric(1)) / 3600
## add column with false positive rate
overview$fp_rate_window = (overview$n_detections - overview$n_bats) /
  vapply(overview$station, function(station)
    sum(durations$n_windows[durations$station == station]), numeric(1))
overview$fp_rate_h = (overview$n_detections - overview$n_bats) /
  overview$total_duration_h

# Write csv and message