# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Combines all data and meta data. 
# Output:
# 1) dat - data frame with an entry per wav file
# 2) dat_model - data frame with an entry per recording night for offshore
# 3) summary - data frame with an entry per date (not night), combination of
#    all summary files
# 4) summary_bats - data frame with an entry per file with bat detection - not
#    currently included
# 5) sun - data frame with sun set and rise
# 6) colours - named vector with colour codes per species
# 7) species_offshore - data frame with Signes species classification for 
#    offshore detections
# 8) species - vector with all species
# 9) locations_all_buoys - data frame with lat long for all buoys, including
#    ones without bat equipment
# All times should be in UTC. 
# cd /home/au472091/OneDrive/au/projects/pam_bats
# source("analysis/code/combine_all_data.R")
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries ----
libraries = c('stringr', 'dplyr', 'lubridate', 'callsync', 'sf',
              'rnaturalearth')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R ----
rm(list=ls()) 

# Paths ----
path_results = '/media/au472091/data/new_results_aspot'
path_detections_bats = '/media/au472091/data/new_results_aspot/defenitely_bats'
path_meta_togter = 'analysis/data/meta_data_bird_surveys.csv'
path_meta_boejer = 'analysis/data/meta_data_boejer.csv'
path_meta_HRIII = 'analysis/data/meta_data_HRIII.csv'
path_sun = 'analysis/data/sunrise_sunset_west_coast_DK.csv'
path_species_offshore = 'analysis/data/species_offshore.csv'
path_species_offshore_env = 'analysis/data/species_offshore_env.csv'
path_weather = 'analysis/data/weather/generated_data'
path_stations = 'analysis/data/weather/stations.csv'
path_all_buoys = 'analysis/data/locations_boejer.csv'
path_summary_per_station_y1 = 'analysis/results/summary_per_station_y1.csv'
path_summary_per_station_y2 = 'analysis/results/summary_per_station_y2.csv'
path_lunar_data = 'analysis/data/lunar_data_esbjerg.csv'
path_combined_data = 'analysis/results/combined_data.RData'

# Should land species be run (very time consuming)
species_land = FALSE

# Load data ----

message('[UPDATE] [', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), 
        '] Starting data load.')

# Load weather stations
weather_stations = read.csv(path_stations)

# Load meta data and fix date/time columns
## fugletogter
meta_togter = read.csv(path_meta_togter, na.strings = '')
meta_togter = meta_togter[,c('start_date', 'start_time', 
                             'end_date', 'end_time')]
if(nrow(meta_togter) < 3) stop('Meta not complete.')
for(i in 2:nrow(meta_togter)){
  if(is.na(meta_togter$start_date[i])){
    meta_togter$start_date[i] = meta_togter$start_date[i-1]
    meta_togter$start_time[i] = meta_togter$start_time[i-1]
  }
}
for(i in (nrow(meta_togter)-1):1){
  if(is.na(meta_togter$end_date[i])){
    meta_togter$end_date[i] = meta_togter$end_date[i+1]
    meta_togter$end_time[i] = meta_togter$end_time[i+1]
  }
}
meta_togter = unique(meta_togter)
meta_togter$start_date = as.Date(meta_togter$start_date)
meta_togter$end_date = as.Date(meta_togter$end_date)
meta_togter$start = paste(meta_togter$start_date, 
                          paste(meta_togter$start_time, ':00'), 
                          sep = '_') |> ymd_hms() - 
  ifelse((as.Date(meta_togter$start_date) > as.Date('2023-03-25') &
            as.Date(meta_togter$start_date) < as.Date('2023-10-29')) |
           (as.Date(meta_togter$start_date) > as.Date('2024-03-30') &
              as.Date(meta_togter$start_date) < as.Date('2024-10-27')),
         60*60, 0) - # removing summertime
  60*60 # from CET to UTC
meta_togter$end = paste(meta_togter$end_date, 
                        paste(meta_togter$end_time, ':00'), 
                        sep = '_') |> ymd_hms()- 
  ifelse((as.Date(meta_togter$start_date) > as.Date('2023-03-25') &
            as.Date(meta_togter$start_date) < as.Date('2023-10-29')) |
           (as.Date(meta_togter$start_date) > as.Date('2024-03-30') &
              as.Date(meta_togter$start_date) < as.Date('2024-10-27')),
         60*60, 0) - # removing summertime
  60*60 # from CET to UTC

## boejer
meta_boejer = read.csv(path_meta_boejer)
meta_boejer = meta_boejer[!is.na(meta_boejer$Recovery.date),]
meta_boejer$Deployment.date = meta_boejer$Deployment.date |> 
  as.character() |>
  as.Date(format = '%Y%m%d')
meta_boejer$Recovery.date = meta_boejer$Recovery.date |> 
  as.character() |>
  as.Date(format = '%Y%m%d')

## meta HRIII
meta_HRIII = read.csv(path_meta_HRIII)
meta_HRIII$Deployment.service.date = meta_HRIII$Deployment.service.date |> 
  as.character() |>
  as.Date(format = '%Y%m%d')
meta_HRIII$Recovery.date = meta_HRIII$Recovery.date |> 
  as.character() |>
  as.Date(format = '%Y%m%d')

# List all prediction checks
files_prediction_checks = list.files(path_results, 
                                     pattern = 'prediction_check_',
                                     recursive = TRUE, full.names = TRUE)

# Read all prediction checks
dat = lapply(files_prediction_checks, read.csv)
names(dat) = files_prediction_checks |> basename() |> 
  str_remove('prediction_check_') |> str_remove('.csv')
dat = dat |> bind_rows(.id = 'folder_name')

# Check if any prediction logs are incomplete
incomplete = which(dat$log_complete == 'False' & dat$broken == 'False')
if(length(incomplete) > 0) stop('Some prediction logs were incomplete.')

# Define removed dates ----
removed_dates = data.frame(
  station = 'HR3-4',
  date = as.Date(c('2023-06-03', '2023-09-17', '2023-09-18', '2023-12-12', 
                   '2023-12-13', '2023-12-14'))
)
removed_dates = rbind(removed_dates, data.frame(
  station = 'HR3-6',
  date = as.Date(c('2023-12-22'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS06',
  date = c(seq(as.Date('2023-09-17'), as.Date('2023-09-30'), by = 'day'),
           seq(as.Date('2023-12-17'), as.Date('2023-12-20'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS08',
  date = c(as.Date('2023-05-31'), as.Date('2023-06-01'),
           seq(as.Date('2023-09-30'), as.Date('2023-11-07'), by = 'day'),
           as.Date('2023-12-27'),
           seq(as.Date('2024-08-22'), as.Date('2024-10-12'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS12',
  date = c(seq(as.Date('2023-09-21'), as.Date('2023-11-03'), by = 'day'),
           seq(as.Date('2023-06-04'), as.Date('2023-06-06 '), by = 'day'),
           as.Date('2023-12-29'),
           seq(as.Date('2024-04-17'), as.Date('2024-05-16'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS13',
  date = c(seq(as.Date('2023-05-26'), as.Date('2023-05-29'), by = 'day'),
           seq(as.Date('2023-08-04'), as.Date('2023-08-30'), by = 'day'),
           seq(as.Date('2023-12-26'), as.Date('2024-01-08'), by = 'day'),
           as.Date('2024-03-27'),
           seq(as.Date('2024-08-14'), as.Date('2024-10-16'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS14',
  date = c(seq(as.Date('2023-06-06'), as.Date('2023-06-08'), by = 'day'),
           seq(as.Date('2023-09-05'), as.Date('2023-09-06'), by = 'day'),
           seq(as.Date('2023-12-27'), as.Date('2024-01-07'), by = 'day'),
           as.Date('2024-04-05'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS16',
  date = c(seq(as.Date('2023-06-08'), as.Date('2023-06-11'), by = 'day'),
           seq(as.Date('2023-12-25'), as.Date('2024-01-23'), by = 'day'),
           as.Date('2024-04-01'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS19',
  date = c(seq(as.Date('2023-05-25'), as.Date('2023-05-26'), by = 'day'),
           seq(as.Date('2023-09-23'), as.Date('2023-10-28'), by = 'day'),
           seq(as.Date('2023-12-23'), as.Date('2023-12-30'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS20',
  date = c(seq(as.Date('2023-06-06'), as.Date('2023-06-08 '), by = 'day'),
           seq(as.Date('2023-09-26'), as.Date('2023-10-15'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS21',
  date = c(seq(as.Date('2023-06-18'), as.Date('2023-06-19'), by = 'day'),
           seq(as.Date('2023-12-26'), as.Date('2024-01-06'), by = 'day'),
           seq(as.Date('2024-08-15'), as.Date('2024-10-16'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS24',
  date = c(seq(as.Date('2023-06-03'), as.Date('2023-06-04'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS25',
  date = c(seq(as.Date('2023-06-08'), as.Date('2023-06-12'), by = 'day'),
           seq(as.Date('2023-12-27'), as.Date('2023-12-29'), by = 'day'),
           as.Date('2024-04-01'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS26',
  date = c(as.Date('2023-06-05'),
           seq(as.Date('2023-09-20'), as.Date('2023-09-21'), by = 'day'),
           as.Date('2023-12-10'),
           seq(as.Date('2024-08-14'), as.Date('2024-10-16'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS27',
  date = c(as.Date('2023-06-03'),
           seq(as.Date('2024-04-11'), as.Date('2024-05-16'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS28',
  date = c(seq(as.Date('2023-05-31'), as.Date('2023-06-01'), by = 'day'),
           as.Date('2024-04-01'),
           seq(as.Date('2025-03-11'), as.Date('2025-04-02'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS29',
  date = c(seq(as.Date('2023-05-30'), as.Date('2023-06-05'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS30',
  date = c(seq(as.Date('2023-09-14'), as.Date('2023-10-23'), by = 'day'),
           seq(as.Date('2023-12-26'), as.Date('2023-12-28'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS31',
  date = c(seq(as.Date('2023-06-07'), as.Date('2023-06-10'), by = 'day'),
           seq(as.Date('2023-09-21'), as.Date('2023-11-01'), by = 'day'),
           seq(as.Date('2023-12-28'), as.Date('2023-12-29'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS32',
  date = c(as.Date('2023-05-09'),
           seq(as.Date('2023-11-30'), as.Date('2023-12-04'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS33',
  date = c(as.Date('2023-05-29'),
           seq(as.Date('2023-09-21'), as.Date('2023-09-24'), by = 'day'),
           seq(as.Date('2023-12-22'), as.Date('2023-12-26'), by = 'day'),
           seq(as.Date('2024-06-20'), as.Date('2024-08-11'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS34',
  date = c(seq(as.Date('2023-06-08'), as.Date('2023-06-11'), by = 'day'),
           seq(as.Date('2023-09-25'), as.Date('2023-11-17'), by = 'day'),
           seq(as.Date('2023-12-23'), as.Date('2023-12-25'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS35',
  date = c(seq(as.Date('2023-06-06'), as.Date('2023-06-09'), by = 'day'),
           seq(as.Date('2023-09-02'), as.Date('2023-09-05'), by = 'day'),
           seq(as.Date('2023-09-25'), as.Date('2023-11-05'), by = 'day'),
           seq(as.Date('2023-12-28'), as.Date('2024-01-04'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'A01',
  date = c(seq(as.Date('2023-11-06'), as.Date('2023-11-12'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'A05',
  date = c(seq(as.Date('2023-06-28'), as.Date('2023-06-29'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'A06',
  date = c(seq(as.Date('2023-06-27'), as.Date('2023-06-29'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'F03',
  date = c(seq(as.Date('2023-11-15'), as.Date('2023-11-17'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'J01',
  date = c(seq(as.Date('2023-07-13'), as.Date('2023-07-14'), by = 'day'))
))

# Clean up ----

message('[UPDATE] [', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), 
        '] Starting clean up.')

dat = dat[!duplicated(dat$file_name),] # removing duplicates caused by recovery
dat$file_name = dat$file_name |> str_remove('.wav')
dat$date = dat$file_name |> strsplit('_') |> sapply(`[`, 2) |> 
  as.Date(format = '%Y%m%d')
dat$time = dat$file_name |> strsplit('_') |> sapply(`[`, 3)
dat$time = sprintf('%02d:%02d:%02d', 
                   as.numeric(substr(dat$time, 1, 2)), 
                   as.numeric(substr(dat$time, 3, 4)), 
                   as.numeric(substr(dat$time, 5, 6)))
dat$time[is.na(dat$time) | dat$time == '00:NA:NA'] = '00:00:00'
dat$date_time = dat$date_time = ymd(dat$date) + hms(dat$time)
dat$night_date_time = dat$date_time - hours(12)
split = dat$night_date_time |> as.character() |> str_split(' ')
dat$night_date = split |> vapply(function(x) x[1], character(1)) |> as.Date()
dat$night_time_m = split |> vapply(function(x) 
  as.numeric(strsplit(x[2], ':')[[1]]) %*% c(60, 1, 1/60), numeric(1))
rm(split)
dat$file_name = dat$file_name |> str_remove('.wav')

# List all summary files ----

message('[UPDATE] [', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), 
        '] Starting listing summary files.')

files_summary = list.files(path_results, 
                           pattern = 'summary_',
                           recursive = TRUE, full.names = TRUE)

# Read all summary files ----

message('[UPDATE] [', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), 
        '] Starting read summary files.')

summary = lapply(files_summary, read.csv)
names(summary) = files_summary |> basename() |> 
  str_remove('summary_') |> str_remove('.csv')
summary = summary |> bind_rows(.id = 'folder_name')
summary$date = as.Date(summary$DATE, format = '%Y-%b-%d')
summary$DATE = NULL

# Load sun ----

message('[UPDATE] [', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), 
        '] Starting loading sun data.')

sun = read.csv(path_sun)
sun$rise_min = vapply(sun$Sunrise, function(time_str) {
  as.numeric(strsplit(time_str, ':')[[1]]) %*% c(60, 1, 1/60)
}, numeric(1)) - # making time to minutes
  ifelse((as.Date(sun$Date) > as.Date('2023-03-25') &
            as.Date(sun$Date) < as.Date('2023-10-29')) |
           (as.Date(sun$Date) > as.Date('2024-03-30') &
              as.Date(sun$Date) < as.Date('2024-10-27')),
         60, 0) + # removing summertime 
  12*60 # fixing time so that the date runs from 12:00 to 12:00
sun$set_min = vapply(sun$Sunset, function(time_str) {
  as.numeric(strsplit(time_str, ':')[[1]]) %*% c(60, 1, 1/60)
}, numeric(1)) - 
  ifelse((as.Date(sun$Date) > as.Date('2023-03-25') &
            as.Date(sun$Date) < as.Date('2023-10-29')) |
           (as.Date(sun$Date) > as.Date('2024-03-30') &
              as.Date(sun$Date) < as.Date('2024-10-27')),
         60, 0) - 
  12*60 

# Load species offshore ----

message('[UPDATE] [', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), 
        '] Starting loading species offshore.')

species_offshore = read.csv(path_species_offshore)
species_offshore_env = read.csv(path_species_offshore_env)

# Fix Signes species names
species_offshore$sp = species_offshore$art
species_offshore$sp = ifelse(str_detect(species_offshore$sp, 'rold'),
                             'Pnat', species_offshore$sp)
species_offshore$sp = ifelse(str_detect(species_offshore$sp, 'ENV'),
                             'ENV', species_offshore$sp)
species_offshore$sp = ifelse(str_detect(species_offshore$sp, 'dværgflagermus'),
                             'Ppyg', species_offshore$sp)
species_offshore$sp = ifelse(str_detect(species_offshore$sp, 'Myo'),
                             'M', species_offshore$sp)
## remove corrupted recordings (had land files after recovery)
species_offshore = species_offshore[!str_detect(species_offshore$Fil, 
                                                'HR3-Y'),]
## fill out ENV species
for(i in which(species_offshore$sp == 'ENV' & 
               species_offshore$Station != 'fugletogt' &
               !str_detect(species_offshore$Fil, 'ONBOARD'))){
  new_species = species_offshore_env$species[species_offshore_env$file ==
                                               species_offshore$Fil[i]]
  if(length(new_species) != 1) stop('Problem with ENV species.')
  species_offshore$sp[i] = new_species
}

# Load location all buoys
locations_all_buoys = read.csv(path_all_buoys)

# Add type location dat ----
type_locations = files_prediction_checks |> strsplit('/') |> 
  vapply(function(split) split[length(split)-2], character(1))
names(type_locations) = files_prediction_checks |> strsplit('/') |> 
  vapply(function(split) split[length(split)-1], character(1))
dat$type_location = type_locations[dat$folder_name]

# Add and fix station names ----

message('[UPDATE] [', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), 
        '] Starting fixing station names.')

dat$station = NA
## fugletogter
dat$station[dat$type_location == 'fugletogter'] = 'survey_ship'
## boejer
dat$station[dat$type_location == 'boejer'] = 
  dat$file_name[dat$type_location == 'boejer'] |> 
  strsplit('_') |> 
  sapply(`[`, 1) 
## HRIII
dat$station[dat$type_location == 'HRIII'] = 
  dat$file_name[dat$type_location == 'HRIII'] |> 
  strsplit('_') |> 
  sapply(`[`, 1) 
## land
dat$station[dat$type_location == 'land'] = 
  dat$file_name[dat$type_location == 'land'] |> 
  strsplit('_') |> 
  sapply(`[`, 1) 

# Fix station names
dat$station = dat$station |>
  str_remove('-C') |>
  str_remove('-FALL23') |>
  str_remove('-LOT1') |>
  str_remove('T3-') |>
  str_remove('T3') |>
  str_remove('C')
dat$station = ifelse(dat$station == 'HR3-4S', 'HR3-4', dat$station)
dat$station = ifelse(dat$station == 'HR-A01', 'A01', dat$station)
dat$station = ifelse(dat$station == 'HR3-X', 'A02', dat$station)
dat$station = ifelse(dat$station == 'HR-A-A05', 'A05', dat$station)
dat$station = ifelse(dat$station == 'HR-B-A06', 'A06', dat$station)
dat$station = ifelse(dat$station == 'HR-Y', 'B01', dat$station)
dat$station = ifelse(dat$station == 'HR-06', 'C06', dat$station)
dat$station = ifelse(dat$station == 'HR3-Y', 'E05', dat$station)
dat$station = ifelse(dat$station == 'HR-F01', 'F01', dat$station)
dat$station = ifelse(dat$station == 'HR-V', 'F03', dat$station)
dat$station = ifelse(dat$station == 'HR3-Z', 'C03', dat$station)
dat$station = ifelse(dat$station == 'HR-X', 'A01', dat$station)
dat$station = ifelse(dat$station == 'HR-J01', 'J01', dat$station)
dat$station = ifelse(dat$station == 'NS24S', 'NS24', dat$station)
dat$station = ifelse(dat$station == 'NS27S', 'NS27', dat$station)
dat$station = ifelse(dat$station == 'NS28S', 'NS28', dat$station)
dat$station = ifelse(dat$station == 'NS32S', 'NS32', dat$station)
dat$station = ifelse(dat$station == 'BALLUM', 'Ballum', dat$station)
dat$station = ifelse(dat$station == 'LAND-BALLUM', 'Ballum', dat$station)
dat$station = ifelse(dat$station == 'LAND6', 'Ballum', dat$station)
dat$station = ifelse(dat$station == 'BLAAVAND', 'Blaavand', dat$station)
dat$station = ifelse(dat$station == 'LAND2', 'Blaavand', dat$station)
dat$station = ifelse(dat$station == 'FANO', 'Fanoe', dat$station)
dat$station = ifelse(dat$station == 'LAND9', 'Fanoe', dat$station)
dat$station = ifelse(dat$station == 'HUSBY', 'Husby', dat$station)
dat$station = ifelse(dat$station == 'LAND5', 'Husby', dat$station)
dat$station = ifelse(dat$station == 'NS6', 'NS06', dat$station)
dat$station = ifelse(dat$station == 'NS8', 'NS08', dat$station)
dat$station = ifelse(dat$station == 'VAT2', 'OSS', dat$station)
dat$station = ifelse(dat$station == 'KAMMER', 'Kammerslusen', dat$station)
dat$station = ifelse(dat$station == 'LAND1', 'Kammerslusen', dat$station)
dat$station = ifelse(dat$station == 'TWJ-08', 'Mandoe', dat$station)
dat$station = ifelse(dat$station == 'MANDO', 'Mandoe', dat$station)
dat$station = ifelse(dat$station == 'LAND-MANDØ', 'Mandoe', dat$station)
dat$station = ifelse(dat$station == 'LAND7', 'Mandoe', dat$station)
dat$station = ifelse(dat$station == 'NYMND-PLTG', 'Nyminde', dat$station)
dat$station = ifelse(dat$station == 'REJSBY', 'Rejsby', dat$station)
dat$station = ifelse(dat$station == 'LAND10', 'Rejsby', dat$station)
dat$station = ifelse(dat$station == 'ROEMOE', 'Roemoe', dat$station)
dat$station = ifelse(dat$station == 'SKAGEN', 'Skagen', dat$station)
dat$station = ifelse(dat$station == 'SKJERN', 'Skjern', dat$station)
dat$station = ifelse(dat$station == 'LAND3', 'Skjern', dat$station)
dat$station = ifelse(dat$station == 'STADILOE', 'Stadiloe', dat$station)
dat$station = ifelse(dat$station == 'LAND4', 'Stadiloe', dat$station)
dat$station = ifelse(dat$station == 'STADILO', 'Stadiloe', dat$station)
dat$station = ifelse(dat$station == 'LAND8', 'Nyminde', dat$station)

dat$station[dat$folder_name == 'E07_BoxD_card_A'] = 'E07'
dat$station[dat$folder_name == 'G05_BoxA_card_A'] = 'G05'
dat$station[dat$folder_name == 'G06_BoxB_card_A'] = 'G06'
dat$station[dat$folder_name == 'F07_BoxC_card_A'] = 'F07'
dat$station = ifelse(dat$station == 'VAT-E', 'F01', dat$station)
dat$station = ifelse(dat$station == 'VAT-F', 'F04', dat$station)
dat$station = ifelse(dat$station == 'VAT-H', 'F07', dat$station)
dat$station = ifelse(dat$station == 'VAT-G', 'G05', dat$station)
dat$station = ifelse(dat$station == 'VA', 'G05', dat$station)
dat$station = ifelse(dat$station == 'VAT1', 'F07', dat$station)
dat$station = ifelse(dat$station == 'VAT4', 'E07', dat$station)

meta_boejer$Station.ID = meta_boejer$Station.ID |>
  str_remove('T3/') |>
  str_remove('/T3') |>
  str_replace('_', '-')

summary$station = summary$station |>
  str_remove('T3-') |>
  str_remove('T3') |>
  str_remove('S-C') |>
  str_remove('-C') |>
  str_remove('LOT1-') |>
  str_remove('-LOT1') |>
  str_remove('C')
summary$station = ifelse(summary$station == 'HR-A01', 'A01', summary$station)
summary$station = ifelse(summary$station == 'HR3-X', 'A02', summary$station)
summary$station = ifelse(summary$station == 'HR-A-A05', 'A05', summary$station)
summary$station = ifelse(summary$station == 'HR-B-A06', 'A06', summary$station)
summary$station = ifelse(summary$station == 'HR-Y', 'B01', summary$station)
summary$station = ifelse(summary$station == 'HR--C06', 'C06', summary$station)
summary$station = ifelse(summary$station == 'HR3-Y', 'E05', summary$station)
summary$station = ifelse(summary$station == 'HR-F01', 'F01', summary$station)
summary$station = ifelse(summary$station == 'HR-V', 'F03', summary$station)
summary$station = ifelse(summary$station == 'HR3-Z', 'C03', summary$station)
summary$station = ifelse(summary$station == 'HR-X', 'A01', summary$station)
summary$station = ifelse(summary$station == 'HR-J01', 'J01', summary$station)
summary$station = ifelse(summary$station == 'NS24S', 'NS24', summary$station)
summary$station = ifelse(summary$station == 'NS27S', 'NS27', summary$station)
summary$station = ifelse(summary$station == 'NS28S', 'NS28', summary$station)
summary$station = ifelse(summary$station == 'NS32S', 'NS32', summary$station)
summary$station = ifelse(summary$station == 'HR-06', 'C06', summary$station)
summary$station = ifelse(summary$station == 'BALLUM', 'Ballum', 
                         summary$station)
summary$station = ifelse(summary$station == 'BLAAVAND', 'Blaavand', 
                         summary$station)
summary$station = ifelse(summary$station == 'LAND2', 'Blaavand', 
                         summary$station)
summary$station = ifelse(summary$station == 'FANO', 'Fanoe', summary$station)
summary$station = ifelse(summary$station == 'LAND9', 'Fanoe', summary$station)
summary$station = ifelse(summary$station == 'HUSBY', 'Husby', summary$station)
summary$station = ifelse(summary$station == 'LAND5', 'Husby', summary$station)
summary$station = ifelse(summary$station == 'NS6', 'NS06', summary$station)
summary$station = ifelse(summary$station == 'NS8', 'NS08', summary$station)
summary$station = ifelse(summary$station == 'VAT2', 'OSS', 
                         summary$station)
summary$station = ifelse(summary$station == 'ONBOARD', 'survey_ship', 
                         summary$station)
summary$station = ifelse(summary$station == 'LAND6', 'Ballum', summary$station)
summary$station = ifelse(summary$station == 'LAND-BALLUM', 'Ballum', 
                         summary$station)
summary$station = ifelse(summary$station == 'KAMMER', 'Kammerslusen', 
                         summary$station)
summary$station = ifelse(summary$station == 'LAND1', 'Kammerslusen', 
                         summary$station)
summary$station = ifelse(summary$station == 'MANDO', 'Mandoe', summary$station)
summary$station = ifelse(summary$station == 'LAND-MANDØ', 'Mandoe', 
                         summary$station)
summary$station = ifelse(summary$station == 'TWJ-08', 'Mandoe', 
                         summary$station)
summary$station = ifelse(summary$station == 'LAND7', 'Mandoe', summary$station)
summary$station = ifelse(summary$station == 'NYMND-PLTG', 'Nyminde', 
                         summary$station)
summary$station = ifelse(summary$station == 'LAND8', 'Nyminde', 
                         summary$station)
summary$station = ifelse(summary$station == 'REJSBY', 'Rejsby', 
                         summary$station)
summary$station = ifelse(summary$station == 'LAND10', 'Rejsby', 
                         summary$station)
summary$station = ifelse(summary$station == 'ROEMOE', 'Roemoe', 
                         summary$station)
summary$station = ifelse(summary$station == 'SKAGEN', 'Skagen', 
                         summary$station)
summary$station = ifelse(summary$station == 'SKJERN', 'Skjern', 
                         summary$station)
summary$station = ifelse(summary$station == 'LAND3', 'Skjern', 
                         summary$station)
summary$station = ifelse(summary$station == 'STADILOE', 'Stadiloe', 
                         summary$station)
summary$station = ifelse(summary$station == 'LAND4', 'Stadiloe', 
                         summary$station)
summary$station = ifelse(summary$station == 'STADILO', 'Stadiloe', 
                         summary$station)

summary$station[summary$folder_name == 'E07_BoxD_card_A_D_A'] = 'E07'
summary$station[summary$folder_name == 'G05_BoxA_card_A_A_A'] = 'G05'
summary$station[summary$folder_name == 'G06_BoxB_card_A_B_A'] = 'G06'
summary$station[summary$folder_name == 'F07_BoxC_card_A_C_A'] = 'F07'
summary$station[summary$folder_name == 'C06_A_FIRST_HR-C-C06_A'] = 'C06'
summary$station[summary$folder_name == 
                  'HR3_Z_C03_A_Fall2023_Recovered_HR3-Z_A'] = 'C03'
summary$station[summary$folder_name == 
                  'HR3_Z_C03_B_Fall2023_Recovered_HR3-Z_B'] = 'C03'
summary$station = ifelse(summary$station == 'VAT-E', 'F01', summary$station)
summary$station = ifelse(summary$station == 'VAT-F', 'F04', summary$station)
summary$station = ifelse(summary$station == 'VAT-H', 'F07', summary$station)
summary$station = ifelse(summary$station == 'VAT-G', 'G05', summary$station)
summary$station = ifelse(summary$station == 'VA', 'G05', summary$station)
summary$station = ifelse(summary$station == 'VAT1', 'F07', summary$station)
summary$station = ifelse(summary$station == 'VAT4', 'E07', summary$station)

locations_all_buoys$Position.ID = locations_all_buoys$Position.ID |>
  str_remove('_SH') |>
  str_remove('_S') |>
  str_remove('HR3_3/') |> 
  str_remove('T3/') |>
  str_replace('_', '-')

message('Station names dat: ', paste(unique(dat$station), collapse = ', '))
message('Station names meta_boejer: ', 
        paste(unique(meta_boejer$Station.ID), collapse = ', '))
message('Station names summary: ', 
        paste(unique(summary$station), collapse = ', '))

# Add the type of location for summary
summary$type_location = NA
summary$type_location = ifelse(str_detect(summary$folder_name, 'HR'), 
                               'HRIII', 
                               summary$type_location)
summary$type_location = ifelse(str_detect(summary$folder_name, 'Box'), 
                               'HRIII', 
                               summary$type_location)
summary$type_location = ifelse(str_detect(summary$folder_name, 'NS'), 
                               'boejer', 
                               summary$type_location)
summary$type_location = ifelse(str_detect(summary$folder_name, 'HR3-4'), 
                               'boejer', 
                               summary$type_location)
summary$type_location = ifelse(str_detect(summary$folder_name, 'HR3_4'), 
                               'boejer', 
                               summary$type_location)
summary$type_location = ifelse(str_detect(summary$folder_name, 'HR3_6'), 
                               'boejer', 
                               summary$type_location)
summary$type_location = ifelse(str_detect(summary$folder_name, 'ONBOARD'), 
                               'fugletogter', 
                               summary$type_location)
summary$type_location = ifelse(summary$station %in% 
                                 c('Ballum', 'Blaavand', 'Fanoe', 'Husby', 
                                   'Kammerslusen', 'Mandoe', 'Nyminde', 
                                   'Rejsby', 'Roemoe', 'Skagen', 'Skjern', 
                                   'Stadiloe'), 
                               'land', 
                               summary$type_location)

# Check if all stations are included as weather station
dat_check = vapply(unique(dat$station), function(x) 
  x %in% weather_stations$station_id, logical(1))
if(!all(dat_check[names(dat_check) != 'survey_ship'])){
  warning('Could not find weather station for: ', 
          paste(unique(dat$station)[!dat_check], collapse = ', '))
}
meta_boejer_check = vapply(unique(meta_boejer$Station.ID), function(x) 
  x %in% weather_stations$station_id, logical(1))
if(!all(meta_boejer_check)){
  warning('Could not find weather station for: ', 
          paste(unique(meta_boejer$Station.ID)[!meta_boejer_check], 
                collapse = ', '))
}
summary_check = vapply(unique(summary$station), function(x) 
  x %in% weather_stations$station_id, logical(1))
if(!all(summary_check[names(summary_check) != 'survey_ship'])){
  warning('Could not find weather station for: ', 
          paste(unique(summary$station)[!summary_check], collapse = ', '))
}

# Fix stations with incorrect prefix
dat$station[dat$folder == 'HR3_6_SettingsNS28Mixup_B_Winter 2023'] = 
  'HR3-6'
dat$station[dat$folder == 'HR3_6_SettingsNS28Mixup_A_Winter2023_Recovered'] = 
  'HR3-6'

dat$station[dat$folder == 'NS16_A_May2024_NB_NS6PREFIXISWRONG'] = 
  'NS16'

dat$station[dat$folder == 'NS28_A_June2024_NB_PREFIXISWRONG_NS25'] = 
  'NS28'

dat$station[dat$folder == 'NS6_juli24_A_locationISNS6butprefixsaysNS16'] = 
  'NS06'

dat$station[dat$folder == 'NS14_juli24_A_locationISNS14butprefixsaysNS25'] = 
  'NS14'

dat$station[dat$folder == 'NS25_juli24_A_locationISNS25butprefixsaysNS6'] = 
  'NS25'

dat$station[dat$folder == 'NS13_A_Spring_2024'] = 
  'NS25'

dat$station[dat$folder == 'NS16_A_Spring2024'] = 
  'NS06'

dat$station[dat$folder == 'NS28_A_Spring2024'] = 
  'NS13'


summary$station[summary$folder_name == 
                  'HR3_6_SettingsNS28Mixup_A_Winter2023_Recovered_NS28_A'] = 
  'HR3-6'
summary$station[summary$folder_name == 
                  'HR3_6_SettingsNS28Mixup_B_Winter 2023_NS28_B'] = 
  'HR3-6'

summary$station[summary$folder_name == 
                  'NS16_A_May2024_NB_NS6PREFIXISWRONG_NS6_A'] = 
  'NS16'

summary$station[summary$folder_name == 
                  'NS28_A_June2024_NB_PREFIXISWRONG_NS25_NS25_A'] = 
  'NS28'

summary$station[summary$folder_name == 
                  'NS6_juli24_A_locationISNS6butprefixsaysNS16_NS16_A'] = 
  'NS06'

summary$station[summary$folder_name == 
                  'NS14_juli24_A_locationISNS14butprefixsaysNS25_NS25_A'] = 
  'NS14'

summary$station[summary$folder_name == 
                  'NS25_juli24_A_locationISNS25butprefixsaysNS6_NS6_A'] = 
  'NS25'

summary$station[summary$folder_name == 'NS13_A_Spring_2024_NS13_A'] = 
  'NS25'

summary$station[summary$folder_name == 'NS16_A_Spring2024_NS16_A'] = 
  'NS06'

summary$station[summary$folder_name == 'NS28_A_Spring2024_NS28_A'] = 
  'NS13'

# Species bats ----

message('[UPDATE] [', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), 
        '] Starting adding offshore bat species.')

## go through dat and add species
dat$species = NA
for(row in which(dat$type_location %in% c('boejer', 'HRIII') &
                 dat$file_name %in% species_offshore$Fil)){
  sub = species_offshore[species_offshore$Fil == dat$file_name[row],]
  dat$species[row] = ifelse(nrow(sub) == 1, sub$sp, NA)
}

if(species_land){
  message('[UPDATE] [', format(Sys.time(), '%Y-%m-%d %H:%M:%S'),
          '] Starting adding onshore bat species.')
  
  for(folder in unique(dat$folder_name[which(dat$type_location == 'land')])){
    print(folder)
    ## list files from land stations
    files = list.files(paste0(path_results,
                              sprintf('/land/%s/combined_selection_tables',
                                      folder)),
                       full.names = TRUE)
    files_clean = files |> basename() |>
      str_remove('_predict_output.log.annotation.result.txt')
    for(row in which(dat$folder_name == folder)){
      species_d = c()
      file = files[files_clean == dat$file_name[row]]
      if(length(file) == 0){
        dat$species[row] = NA
        next
      }
      st = load.selection.table(file)
      if(nrow(st) == 0){
        dat$species[row] = NA
        next
      }
      st$Annotations = str_to_title(st$Annotations)
      st$Annotations[st$Annotations %in%
                       c('Mbramys', 'Mdas', 'Mnat', 'Mdau')] = 'M'
      st$Annotations[st$Annotations %in% c('Nnoc', 'Eser', 'Vmur')] = 'NVE'
      st$Annotations[st$Annotations %in% c('Noise', 'Bbar', 'B')] = 'noise'
      duration = max(st$End.Time..s.)
      chunk_starts = seq(0, duration, 5)
      for(chunk_start in chunk_starts){
        d = st[st$Begin.Time..s. >= chunk_start &
                 st$Begin.Time..s. < (chunk_start+5),]
        ### remove species with less than three occurrences
        table_species = table(d$Annotations)
        table_species = table_species[names(table_species) != 'noise']
        table_species = table_species[table_species >= 5]
        species_d = c(species_d, names(table_species))
      } # end chunk loop
      dat$species[row] = ifelse(length(species_d) > 0,
                                paste(unique(species_d), collapse = ', '),
                                NA)
    } # end row loop
  } # end folder loop
} # end if species_land loop

# Add if recordings were offshore ----

message('[UPDATE] [', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), 
        '] Starting adding offshore or not.')

dat$offshore = ifelse(dat$type_location == 'land', FALSE, NA)
summary$offshore = ifelse(summary$type_location == 'land', FALSE, NA)
## fugletogter
dat$offshore[which(dat$type_location == 'fugletogter')] = 
  vapply(which(dat$type_location == 'fugletogter'), function(i){
    any(dat$date_time[i] > meta_togter$start & 
          dat$date_time[i] < meta_togter$end)
  }, logical(1))
summary$offshore[which(summary$type_location == 'fugletogter')] = 
  vapply(which(summary$type_location == 'fugletogter'), function(i){
    any(summary$date_time[i] > meta_togter$start & 
          summary$date_time[i] < meta_togter$end)
  }, logical(1))
## boejer
for(st in unique(dat$station[dat$type_location == 'boejer'])){
  sub_meta = meta_boejer[meta_boejer$Station.ID == st,]
  dates_station = c(dat$date[which(dat$station == st)],
                    summary$date[which(summary$station == st)]) |> 
    unique() |> sort()
  offshore_dates = c()
  for(i in seq_len(nrow(sub_meta))){
    start = sub_meta$Deployment.date[i]
    end = sub_meta$Recovery.date[i] 
    offshore_dates = c(offshore_dates, 
                       dates_station[dates_station > start + 1 &
                                       dates_station < (end - 2)] |> 
                         as.character())
  } 
  dat$offshore[which(dat$station == st)] = 
    (dat$night_date[which(dat$station == st)] %in% offshore_dates)
  summary$offshore[which(summary$station == st)] = 
    (summary$date[which(summary$station == st)] %in% offshore_dates)
}
# problem dates because of overlap in Bioconsult deployment dates, recorder
# active on deck for multiple days or after buoy stranded
dat$offshore[dat$station == 'NS06' & dat$date %in% 
               as.Date(c('2024-04-16', '2024-04-17', 
                         '2024-04-18', '2024-04-19'))] = FALSE
summary$offshore[summary$station == 'NS06' & summary$date %in% 
                   as.Date(c('2024-04-16', '2024-04-17', 
                             '2024-04-18', '2024-04-19'))] = FALSE
dat$offshore[dat$station == 'NS08' & dat$date %in% 
               as.Date(c('2024-05-15', '2024-05-16'))] = FALSE
summary$offshore[summary$station == 'NS08' & summary$date %in% 
                   as.Date(c('2024-05-15', '2024-05-16'))] = FALSE

dat$offshore[dat$folder_name == 'NS12_Summer 2024' & dat$date %in% 
               as.Date(c('2024-05-15'))] = FALSE
summary$offshore[summary$folder_name == 'A_NS12_A' & summary$date %in% 
                   as.Date(c('2024-05-15'))] = FALSE

dat$offshore[dat$station == 'NS13' & dat$date %in% 
               as.Date(c('2024-04-16', '2024-04-17', 
                         '2024-04-18', '2024-04-19'))] = FALSE
summary$offshore[summary$station == 'NS13' & summary$date %in% 
                   as.Date(c('2024-04-16', '2024-04-17', 
                             '2024-04-18', '2024-04-19'))] = FALSE
dat$offshore[dat$station == 'NS24' & dat$date %in% 
               as.Date(c('2023-09-11', '2023-09-12'))] = FALSE
dat$offshore[dat$station == 'NS20' & dat$date %in% 
               as.Date(c('2024-05-15'))] = FALSE
summary$offshore[summary$station == 'NS20' & summary$date %in% 
                   as.Date(c('2024-05-15'))] = FALSE
summary$offshore[summary$station == 'NS24' & summary$date %in% 
                   as.Date(c('2023-09-11'))] = FALSE
dat$offshore[dat$station == 'NS25' & dat$date %in% 
               as.Date(c('2024-04-15', '2024-04-16', 
                         '2024-04-17', '2024-04-18'))] = FALSE
summary$offshore[summary$station == 'NS25' & summary$date %in% 
                   as.Date(c('2024-04-14', '2024-04-15', '2024-04-16', 
                             '2024-04-17', '2024-04-18'))] = FALSE
dat$offshore[dat$station == 'NS29' & dat$date %in% 
               as.Date(c('2024-05-15'))] = FALSE
summary$offshore[summary$station == 'NS29' & summary$date %in% 
                   as.Date(c('2024-05-15'))] = FALSE

## HRIII
for(st in unique(dat$station[dat$type_location == 'HRIII'])){
  sub_meta = meta_HRIII[meta_HRIII$WT.ID == st,]
  dates_station = c(dat$date[which(dat$station == st)],
                    summary$date[which(summary$station == st)]) |> 
    unique() |> sort()
  offshore_dates = c()
  for(i in seq_len(nrow(sub_meta))){
    start = sub_meta$Deployment.service.date[i]
    end = sub_meta$Recovery.date[i] 
    offshore_dates = c(offshore_dates, 
                       dates_station[dates_station > start &
                                       dates_station < (end - 2)] |> 
                         as.character())
  } 
  dat$offshore[which(dat$station == st)] = 
    (dat$night_date[which(dat$station == st)] %in% offshore_dates)
  summary$offshore[which(summary$station == st)] = 
    (summary$date[which(summary$station == st)] %in% offshore_dates)
}

# Clean up 2 ----

# Fix misnaming of Skjern_0912_2024_A_NS32_A
dat$station[dat$folder_name  == 'Skjern_0912_2024_A'] = 'Skjern'
summary$station[summary$folder_name == 'Skjern_0912_2024_A_NS32_A'] = 'Skjern'
summary$type_location[summary$folder_name == 'Skjern_0912_2024_A_NS32_A'] = 
  'land'

# Remove first deployment A01 og all of B01
summary = summary[!(summary$station == 'A01' & 
                      summary$date < as.Date('2023-10-01')),]
dat = dat[!(dat$station == 'A01' & 
              dat$date < as.Date('2023-10-01')),]
summary = summary[!summary$station == 'B01',]
dat = dat[!dat$station == 'B01',]

# Remove faulty first days Mandoe
dat = dat[!(dat$station == 'Mandoe' & 
              dat$date %in% seq(as.Date('2023-04-15'), 
                                as.Date('2023-04-20'), by = 'day')),]

# Remove wake-up on first of month
dat = dat[!(dat$station == 'HR3-4' & 
              dat$date %in% as.Date(c('2024-06-01', '2024-11-01',
                                      '2025-01-01', '2025-02-01',
                                      '2025-04-01'))),]
summary = summary[!(summary$station == 'HR3-4' & 
                      summary$date %in% as.Date(c('2024-06-01', '2024-11-01',
                                                  '2025-01-01', '2025-02-01',
                                                  '2025-04-01'))),]
dat = dat[!(dat$station == 'NS06' & 
              dat$date %in% as.Date(c('2024-04-01', '2025-01-01'))),]
summary = summary[!(summary$station == 'NS06' & 
                      summary$date %in% as.Date(c('2024-04-01',
                                                  '2025-01-01'))),]
dat = dat[!(dat$station == 'NS08' & 
              dat$date %in% as.Date(c('2024-08-01', '2025-01-01'))),]
summary = summary[!(summary$station == 'NS08' & 
                      summary$date %in% as.Date(c('2024-08-01', 
                                                  '2025-01-01'))),]
dat = dat[!(dat$station == 'NS12' & 
              dat$date %in% as.Date(c('2024-11-01', '2025-01-01',
                                      '2025-02-01'))),]
summary = summary[!(summary$station == 'NS12' & 
                      summary$date %in% as.Date(c('2024-11-01', '2025-01-01',
                                                  '2025-02-01'))),]
dat = dat[!(dat$station == 'NS13' & 
              dat$date %in% as.Date(c('2024-04-01', '2025-01-01'))),]
summary = summary[!(summary$station == 'NS13' & 
                      summary$date %in% as.Date(c('2024-04-01',
                                                  '2025-01-01'))),]
dat = dat[!(dat$station == 'NS14' & 
              dat$date %in% as.Date(c('2025-01-01'))),]
summary = summary[!(summary$station == 'NS14' & 
                      summary$date %in% as.Date(c('2025-01-01'))),]
dat = dat[!(dat$station == 'NS16' & 
              dat$date %in% as.Date(c('2024-08-01'))),]
summary = summary[!(summary$station == 'NS16' & 
                      summary$date %in% as.Date(c('2024-08-01'))),]
dat = dat[!(dat$station == 'NS19' & 
              dat$date %in% as.Date(c('2024-11-01','2025-01-01'))),]
summary = summary[!(summary$station == 'NS19' & 
                      summary$date %in% as.Date(c('2024-11-01',
                                                  '2025-01-01'))),]
dat = dat[!(dat$station == 'NS20' & 
              dat$date %in% as.Date(c('2024-08-01', '2024-11-01',
                                      '2025-04-01'))),]
summary = summary[!(summary$station == 'NS20' & 
                      summary$date %in% as.Date(c('2024-08-01',
                                                  '2024-11-01',
                                                  '2025-04-01'))),]
dat = dat[!(dat$station == 'NS21' & 
              dat$date %in% as.Date(c('2024-06-01', '2025-01-01'))),]
summary = summary[!(summary$station == 'NS21' & 
                      summary$date %in% as.Date(c('2024-06-01',
                                                  '2025-01-01'))),]
dat = dat[!(dat$station == 'NS24' & 
              dat$date %in% as.Date(c('2024-08-01', '2024-11-01'))),]
summary = summary[!(summary$station == 'NS24' & 
                      summary$date %in% as.Date(c('2024-08-01',
                                                  '2024-11-01'))),]
dat = dat[!(dat$station == 'NS26' & 
              dat$date %in% as.Date(c('2024-06-01', '2025-01-01'))),]
summary = summary[!(summary$station == 'NS26' & 
                      summary$date %in% as.Date(c('2024-06-01',
                                                  '2025-01-01'))),]
dat = dat[!(dat$station == 'NS27' & 
              dat$date %in% as.Date(c('2024-08-01', '2024-11-01',
                                      '2025-01-01', '2025-02-01'))),]
summary = summary[!(summary$station == 'NS27' & 
                      summary$date %in% as.Date(c('2024-08-01', '2024-11-01',
                                                  '2025-01-01', 
                                                  '2025-02-01'))),]
dat = dat[!(dat$station == 'NS28' & 
              dat$date %in% as.Date(c('2024-06-01', '2024-11-01',
                                      '2025-01-01', '2025-02-01'))),]
summary = summary[!(summary$station == 'NS28' & 
                      summary$date %in% as.Date(c('2024-06-01', '2024-11-01',
                                                  '2025-01-01', 
                                                  '2025-02-01'))),]
dat = dat[!(dat$station == 'NS29' & 
              dat$date %in% as.Date(c('2024-08-01', '2024-11-01'))),]
summary = summary[!(summary$station == 'NS29' & 
                      summary$date %in% as.Date(c('2024-08-01',
                                                  '2024-11-01'))),]
dat = dat[!(dat$station == 'NS30' & 
              dat$date %in% as.Date(c('2024-06-01', '2024-11-01',
                                      '2025-01-01'))),]
summary = summary[!(summary$station == 'NS30' & 
                      summary$date %in% as.Date(c('2024-06-01', '2024-11-01',
                                                  '2025-01-01'))),]
dat = dat[!(dat$station == 'NS31' & 
              dat$date %in% as.Date(c('2025-01-01'))),]
summary = summary[!(summary$station == 'NS31' & 
                      summary$date %in% as.Date(c('2025-01-01'))),]
dat = dat[!(dat$station == 'NS32' & 
              dat$date %in% as.Date(c('2024-08-01', '2024-11-01'))),]
summary = summary[!(summary$station == 'NS32' & 
                      summary$date %in% as.Date(c('2024-08-01',
                                                  '2024-11-01'))),]
dat = dat[!(dat$station == 'NS33' & 
              dat$date %in% as.Date(c('2025-01-01', '2025-02-01',
                                      '2025-04-01'))),]
summary = summary[!(summary$station == 'NS33' & 
                      summary$date %in% as.Date(c('2025-01-01', 
                                                  '2025-02-01',
                                                  '2025-04-01'))),]
dat = dat[!(dat$station == 'NS34' & 
              dat$date %in% as.Date(c('2024-06-01', '2025-04-01'))),]
summary = summary[!(summary$station == 'NS34' & 
                      summary$date %in% as.Date(c('2024-06-01',
                                                  '2025-04-01'))),]
dat = dat[!(dat$station == 'NS35' & 
              dat$date %in% as.Date(c('2024-06-01','2025-01-01'))),]
summary = summary[!(summary$station == 'NS35' & 
                      summary$date %in% as.Date(c('2024-06-01',
                                                  '2025-01-01'))),]
dat = dat[!(dat$station == 'A01' & 
              dat$date %in% as.Date(c('2024-09-04'))),]
summary = summary[!(summary$station == 'A01' & 
                      summary$date %in% as.Date(c('2024-09-04'))),]
dat = dat[!(dat$station == 'E07' & 
              dat$date %in% as.Date(c('2024-11-01','2025-01-01',
                                      '2025-02-01'))),]
summary = summary[!(summary$station == 'E07' & 
                      summary$date %in% as.Date(c('2024-11-01',
                                                  '2025-01-01',
                                                  '2025-02-01'))),]
dat = dat[!(dat$station == 'F01' & 
              dat$date %in% as.Date(c('2025-04-01'))),]
summary = summary[!(summary$station == 'F01' & 
                      summary$date %in% as.Date(c('2025-04-01'))),]
dat = dat[!(dat$station == 'F04' & 
              dat$date %in% as.Date(c('2025-04-01'))),]
summary = summary[!(summary$station == 'F04' & 
                      summary$date %in% as.Date(c('2025-04-01'))),]
dat = dat[!(dat$station == 'F07' & 
              dat$date %in% as.Date(c('2024-11-01', '2025-04-01'))),]
summary = summary[!(summary$station == 'F07' & 
                      summary$date %in% as.Date(c('2024-11-01',
                                                  '2025-04-01'))),]
dat = dat[!(dat$station == 'G05' & 
              dat$date %in% as.Date(c('2024-11-01', '2025-01-01',
                                      '2025-04-01'))),]
summary = summary[!(summary$station == 'G05' & 
                      summary$date %in% as.Date(c('2024-11-01',
                                                  '2025-01-01',
                                                  '2025-04-01'))),]
dat = dat[!(dat$station == 'G06' & 
              dat$date %in% as.Date(c('2024-08-01'))),]
summary = summary[!(summary$station == 'G06' & 
                      summary$date %in% as.Date(c('2024-08-01'))),]
dat = dat[!(dat$station == 'OSS' & 
              dat$date %in% as.Date(c('2024-11-01', '2025-01-01',
                                      '2025-04-01', '2025-04-02'))),]
summary = summary[!(summary$station == 'OSS' & 
                      summary$date %in% as.Date(c('2024-11-01', '2025-01-01',
                                                  '2025-04-01',
                                                  '2025-04-02'))),]

# Remove dates with overlap between activation and recovery (but before 
# deployment)
dat = dat[!(dat$station == 'NS08' & 
              dat$date %in% as.Date(c('2024-05-17'))),]
summary = summary[!(summary$station == 'NS08' & 
                      summary$date %in% as.Date(c('2024-05-17'))),]
dat = dat[!(dat$station == 'NS19' & 
              dat$date %in% as.Date(c('2025-02-20', '2025-02-21',
                                      '2025-02-22', '2025-02-23'))),]
summary = summary[!(summary$station == 'NS19' & 
                      summary$date %in% as.Date(c('2025-02-20', '2025-02-21',
                                                  '2025-02-22',
                                                  '2025-02-23'))),]
dat = dat[!(dat$station == 'NS29' & 
              dat$date %in% as.Date(c('2024-05-16'))),]
summary = summary[!(summary$station == 'NS29' & 
                      summary$date %in% as.Date(c('2024-05-16'))),]

# Create data frame for model ----

message('[UPDATE] [', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), 
        '] Starting creating dat_model.')

dat_model = data.frame()
## boejer
for(st in unique(meta_boejer$Station.ID)){
  sub_meta = meta_boejer[meta_boejer$Station.ID == st,]
  dates_station = dat$night_date[dat$station == st & !is.na(dat$species)] |> 
    unique() |> sort()
  all_dates = c()
  for(i in seq_len(nrow(sub_meta))){
    start = sub_meta$Deployment.date[i] + 1
    end = sub_meta$Recovery.date[i] - 2
    if(end < start) stop('End smaller than start!')
    all_dates = seq(start, end, by = 'day')
    triggers_station = dat[dat$station == st & dat$offshore,]
    all_dates = all_dates[all_dates %in% triggers_station$night_date]
    if(length(all_dates) < 1) next
    dat_model = rbind(dat_model, 
                      data.frame(
                        station = st,
                        date = all_dates,
                        detection = vapply(all_dates, function(date) 
                          date %in% dates_station, logical(1)),
                        subset = 'Buoys'
                      ))
  }
}
## HRIII
for(st in unique(meta_HRIII$WT.ID)){
  sub_meta = meta_HRIII[meta_HRIII$WT.ID == st,]
  dates_station = dat$night_date[dat$station == st & !is.na(dat$species)] |> 
    unique() |> sort()
  all_dates = c()
  for(i in seq_len(nrow(sub_meta))){
    start = sub_meta$Deployment.service.date[i]+1
    end = sub_meta$Recovery.date[i]-2 
    if(end < start) stop('End smaller than start!')
    all_dates = seq(start, end, by = 'day')
    triggers_station = dat[dat$station == st & dat$offshore,]
    all_dates = all_dates[all_dates %in% triggers_station$night_date]
    if(length(all_dates) < 1) next
    dat_model = rbind(dat_model, 
                      data.frame(
                        station = st,
                        date = all_dates,
                        detection = vapply(all_dates, function(date) 
                          date %in% dates_station, logical(1)),
                        subset = 'Windturbines'
                      ))
  }
}
dat_model$subset[dat_model$station == 'OSS'] = 'OSS'

# Remove first deployment for buoys (wrong settings)
to_remove = unique(dat[which(str_detect(dat$folder_name, 'Spring23') |
                               str_detect(dat$folder_name, 'NS20_B_Summer23') |
                               str_detect(dat$folder_name, 'NS13_B_Summer23')), 
                       c('station', 'night_date')])
for(row in seq_len(nrow(to_remove))){
  dat_model = dat_model[!(dat_model$station == to_remove$station[row] &
                            dat_model$date == to_remove$night_date[row]),]
}

# Remove nights that are not complete (mainly battery failure)
for(row in seq_len(nrow(removed_dates))){
  dat_model = dat_model[!(dat_model$station == removed_dates$station[row] &
                            dat_model$date == removed_dates$date[row]),]
}

# Remove last night per deployment
dat_model = dat_model %>%
  arrange(station, date) %>%
  group_by(station) %>%
  mutate(
    next_date = lead(date),
    gap = as.numeric(next_date - date),
    to_remove = ifelse(is.na(next_date) | gap > 2, TRUE, FALSE)
  ) %>%
  ungroup() %>%
  select(-next_date, -gap)
dat_model = dat_model[!dat_model$to_remove,]
dat_model$to_remove = NULL

# Add weather ----

message('[UPDATE] [', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), 
        '] Starting adding weather data.')

## add weather for each offshore night
message('Getting weather for offshore nights.')
for(st in unique(dat_model$station)){
  # print(st)
  ws = weather_stations$weather_station[weather_stations$station_id == st]
  if(length(ws) != 1) stop('Did not find ', st)
  weather = read.table(sprintf('%s/%s_era5.pre', path_weather, ws),
                       skip = 2, header = TRUE)
  weather$wind_direction = (270 - weather$wind_direction * 180 / pi) %% 360
  for(row in which(dat_model$station == st)){
    time_sunset = sun$Sunset[sun$Date == dat_model$date[row]] |>
      as.POSIXct(format = '%H:%M:%S') |>
      round_date(unit = 'hour') |>
      format('%H') |>
      as.numeric()
    sub = weather[weather$year == dat_model$date[row] |> str_sub(1, 4) &
                    weather$month == dat_model$date[row] |> str_sub(6, 7) |> 
                    as.numeric() &
                    weather$day == dat_model$date[row] |> str_sub(9, 10) |> 
                    as.numeric() &
                    weather$hour == time_sunset,]
    if(nrow(sub) != 1) stop('Weather not found for row ', row)
    dat_model[row, c('mean_temp', 'wind_speed', 'wind_direction', 'precip',
                     'cloud_coverage', 'atm_pressure')] = 
      sub[c('mean_temp', 'wind_speed', 'wind_direction', 'precip',
            'cloud_coverage', 'atm_pressure')]
  }
}

# # add weather for each detection (needed to compare weather with Vestas)
# message('Getting weather for Vestas stations.')
# for(st in c('A05', 'A06', 'C03')){
#   print(st)
#   ws = weather_stations$weather_station[weather_stations$station_id == st]
#   if(length(ws) != 1) stop('Did not find ', st)
#   weather = read.table(sprintf('%s/%s_era5.pre', path_weather, ws),
#                        skip = 2, header = TRUE)
#   weather$wind_direction = (270 - weather$wind_direction * 180 / pi) %% 360
#   for(row in which(dat$station == st)){
#     sub = weather[weather$year == dat$date[row] |> str_sub(1, 4) &
#                     weather$month == dat$date[row] |> str_sub(6, 7) |>
#                     as.numeric() &
#                     weather$day == dat$date[row] |> str_sub(9, 10) |>
#                     as.numeric() &
#                     weather$hour == dat$time[row] |> str_sub(1, 2) |>
#                     as.numeric(),]
#     if(nrow(sub) != 1) stop('Weather not found for row ', row)
#     dat[row, c('mean_temp', 'wind_speed', 'wind_direction', 'precip')] =
#       sub[c('mean_temp', 'wind_speed', 'wind_direction', 'precip')]
#   }
# }

# Add lunar phase ----

message('[UPDATE] [', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), 
        '] Starting adding lunar data.')

lunar_data = read.csv(path_lunar_data)
dat_model = merge(dat_model, lunar_data, by = 'date', 
                  all.x = TRUE, all.y = FALSE)

# Add GPS coordinates stations ----

message('[UPDATE] [', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), 
        '] Starting adding coordinates.')

dat_model = merge(dat_model, weather_stations[,c('station_id', 'lat', 'long')],
                  by.x = 'station', by.y = 'station_id', 
                  all.x = TRUE, all.y = FALSE)

# Calculate distance to coast and add to dat_model ----
stations = unique(dat_model[c('station', 'lat', 'long')])
world_coastline = ne_coastline(scale = 'large', returnclass = 'sf')
stations_sf = st_as_sf(stations, coords = c('long', 'lat'), crs = 4326)
nearest_coast_distance = st_distance(stations_sf, world_coastline)
stations$distance_to_coast = (nearest_coast_distance |> apply(1, min))/1000
dat_model = merge(dat_model, stations[c('station', 'distance_to_coast')], 
                  by = 'station', all.x = TRUE, all.y = FALSE)

# Colours species ----
colours = c(
  '#1f77b4', # A soft blue
  '#f8bbd0', # A pastel pink
  '#8d6e63', # A pastel brown
  '#2ca02c', # A strong green
  '#9467bd', # A moderate purple
  '#F7DC6F', # A soft yellow
  '#E67E22', # A pastel orange
  '#B03A2E', # A dark red
  '#cfd8dc'  # A pastel grey
)
species = c('Eser', 'M', 'Nnoc', 'ENV', 'Paur', 'Pnat', 'Ppip', 'Ppyg', 'Vmur')
names(colours) = species

# Create summary per station for Signe ----
sum_per_station_y1 = dat[dat$night_date < as.Date('2024-04-10') & 
                           dat$offshore,] |> 
  group_by(station) |> 
  summarise(n = n_distinct(night_date))
sum_per_station_y2 = dat[dat$night_date >= as.Date('2024-04-10') & 
                           dat$night_date < as.Date('2025-04-10') &
                           dat$offshore,] |> 
  group_by(station) |> 
  summarise(n = n_distinct(night_date))

## add distance to coast
sum_per_station_y1$dist_coast_km = 
  vapply(sum_per_station_y1$station, 
         function(st){
           return(dat_model$distance_to_coast[dat_model$station == st][1])
         }, numeric(1))
sum_per_station_y2$dist_coast_km = 
  vapply(sum_per_station_y2$station, 
         function(st){
           return(dat_model$distance_to_coast[dat_model$station == st][1])
         }, numeric(1))

# Store output ----

message('[UPDATE] [', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), 
        '] Starting storing data.')

save(dat, dat_model, summary, sun, colours, removed_dates,
     species_offshore, species, locations_all_buoys,
     file = path_combined_data)
write.csv(sum_per_station_y1, path_summary_per_station_y1, row.names = FALSE)
write.csv(sum_per_station_y2, path_summary_per_station_y2, row.names = FALSE)
message('[UPDATE] [', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), 
        '] Stored all data. Done.')

