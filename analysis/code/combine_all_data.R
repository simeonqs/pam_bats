# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Combines all data and meta data. 
# Output:
# 1) dat - data frame with an entry per wav file
# 2) dat_model - data frame with an entry per recording night for offshore
# 3) summary - data frame with an entry per date (not night), combination of
#    all summary files
# 4) summary_bats - data frame with an entry per file with bat detection
# 5) sun - data frame with sun set and rise
# 6) colours - named vector with colour codes per species
# 7) species_offshore - data frame with Signes species classification for 
#    offshore detections
# 8) species - vector with all species
# 9) locations_all_buoys - data frame with lat long for all buoys, including
#    ones without bat equipment
# All times should be in UTC. 
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
path_results = '/home/au472091/Documents/new_results_aspot'
path_detections_bats = '/home/au472091/Documents/results_aspot/defenitely_bats'
path_meta_togter = 'analysis/data/meta_data_bird_surveys.csv'
path_meta_boejer = 'analysis/data/meta_data_boejer.csv'
path_meta_HRIII = 'analysis/data/meta_data_HRIII.csv'
path_sun = 'analysis/data/sunrise_sunset_west_coast_DK.csv'
path_species_offshore = 'analysis/data/species_offshore.csv'
path_weather = 'analysis/data/weather/generated_data'
path_stations = 'analysis/data/weather/stations.csv'
path_all_buoys = 'analysis/data/locations_boejer.csv'
path_combined_data = 'analysis/results/combined_data.RData'
path_summary_per_station = 'analysis/results/summary_per_station.csv'
path_offshore_bats = 'analysis/results/offshore_detections_bats.csv'

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
meta_boejer = meta_boejer[!is.na(meta_boejer$recovery.date),]
meta_boejer$Deployment..Service.date = meta_boejer$Deployment..Service.date |> 
  as.character() |>
  as.Date(format = '%Y%m%d')
meta_boejer$recovery.date = meta_boejer$recovery.date |> 
  as.character() |>
  as.Date(format = '%Y%m%d')

## meta HRIII
meta_HRIII = read.csv(path_meta_HRIII)
meta_HRIII$Deployment.service.date = meta_HRIII$Deployment.service.date |> 
  as.character() |>
  as.Date(format = '%m/%d/%Y')
meta_HRIII$Recovery.date = meta_HRIII$Recovery.date |> 
  as.character() |>
  as.Date(format = '%m/%d/%Y')

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

# Clean up
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
dat$file_name = dat$file_name |> str_remove('.wav')

# List all summary files
files_summary = list.files(path_results, 
                           pattern = 'summary_',
                           recursive = TRUE, full.names = TRUE)

# Read all summary files
summary = lapply(files_summary, read.csv)
names(summary) = files_summary |> basename() |> 
  str_remove('summary_') |> str_remove('.csv')
summary = summary |> bind_rows(.id = 'folder_name')
summary$date = as.Date(summary$DATE, format = '%Y-%b-%d')
summary$DATE = NULL

# Add the type of location
type_locations = files_prediction_checks |> strsplit('/') |> 
  vapply(function(split) split[length(split)-2], character(1))
names(type_locations) = unique(dat$folder_name)
dat$type_location = type_locations[dat$folder_name]
summary$type_location = NA
summary$type_location = ifelse(str_detect(summary$folder_name, 'NS'), 
                               'boejer', 
                               summary$type_location)
summary$type_location = ifelse(str_detect(summary$folder_name, 'HR'), 
                               'HRIII', 
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

# Load sun
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

# Load species offshore
species_offshore = read.csv(path_species_offshore)

# Fix Signes species names
species_offshore$sp = species_offshore$art
species_offshore$sp = ifelse(str_detect(species_offshore$sp, 'rold'),
                             'Pnat', species_offshore$sp)
species_offshore$sp = ifelse(str_detect(species_offshore$sp, 'langøre'),
                             'NVE', species_offshore$sp)
species_offshore$sp = ifelse(str_detect(species_offshore$sp, 'ENV'),
                             'NVE', species_offshore$sp)
species_offshore$sp = ifelse(str_detect(species_offshore$sp, 'dværgflagermus'),
                             'Ppyg', species_offshore$sp)
species_offshore$sp = ifelse(str_detect(species_offshore$sp, 'Myo'),
                             'M', species_offshore$sp)

# Load location all buoys
locations_all_buoys = read.csv(path_all_buoys)

# Add and fix station names
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

meta_boejer$Station.ID = meta_boejer$Station.ID |>
  str_remove('T3/') |>
  str_replace('_', '-')

summary$station = summary$station |>
  str_remove('T3-') |>
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
  'NS6'

dat$station[dat$folder == 'NS14_juli24_A_locationISNS14butprefixsaysNS25'] = 
  'NS14'

dat$station[dat$folder == 'NS25_juli24_A_locationISNS25butprefixsaysNS6'] = 
  'NS25'

dat$station[dat$folder == 'NS13_A_Spring_2024'] = 
  'NS25'

dat$station[dat$folder == 'NS16_A_Spring2024'] = 
  'NS6'

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
  'NS6'

summary$station[summary$folder_name == 
                  'NS14_juli24_A_locationISNS14butprefixsaysNS25_NS25_A'] = 
  'NS14'

summary$station[summary$folder_name == 
                  'NS25_juli24_A_locationISNS25butprefixsaysNS6_NS6_A'] = 
  'NS25'

summary$station[summary$folder_name == 'NS13_A_Spring_2024_NS13_A'] = 
  'NS25'

summary$station[summary$folder_name == 'NS16_A_Spring2024_NS16_A'] = 
  'NS6'

summary$station[summary$folder_name == 'NS28_A_Spring2024_NS28_A'] = 
  'NS13'


# Add how many bats were found per file - this includes some onshore detections
## list all detection tables with bats
st_bats = load.selection.tables(path_detections_bats, recursive = TRUE)
summary_bats = data.frame(file = unique(st_bats$file),
                          n_bats = vapply(unique(st_bats$file), function(f)
                            nrow(st_bats[st_bats$file == f,]), numeric(1)))
dat$n_bats = vapply(seq_len(nrow(dat)), function(i){
  if(dat$type_location[i] == 'land') return(NA) else
    if(dat$file_name[i] %in% summary_bats$file) 
      return(summary_bats$n_bats[summary_bats$file == dat$file_name[i]]) else
        return(0)
}, numeric(1))

# Add if recordings were offshore
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
    start = sub_meta$Deployment..Service.date[i]
    end = sub_meta$recovery.date[i] 
    offshore_dates = c(offshore_dates, 
                       dates_station[dates_station > start + 1 &
                                       dates_station < (end - 3)] |> 
                         as.character())
  } 
  dat$offshore[which(dat$station == st)] = 
    (dat$date[which(dat$station == st)] %in% offshore_dates)
  summary$offshore[which(summary$station == st)] = 
    (summary$date[which(summary$station == st)] %in% offshore_dates)
}
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
    (dat$date[which(dat$station == st)] %in% offshore_dates)
  summary$offshore[which(summary$station == st)] = 
    (summary$date[which(summary$station == st)] %in% offshore_dates)
}

# Create data frame with all offshore days
dat_model = data.frame()
## boejer
for(st in unique(meta_boejer$Station.ID)){
  sub_meta = meta_boejer[meta_boejer$Station.ID == st,]
  dates_station = dat$night_date[dat$station == st & dat$n_bats > 0] |> 
    unique() |> sort()
  all_dates = c()
  for(i in seq_len(nrow(sub_meta))){
    start = sub_meta$Deployment..Service.date[i] + 1
    end = sub_meta$recovery.date[i] - 2
    if(end < start) stop('End smaller than start!')
    all_dates = seq(start, end, by = 'day')
    triggers_station = dat[dat$station == st,]
    all_dates = all_dates[all_dates %in% triggers_station$date]
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
  dates_station = dat$night_date[dat$station == st & dat$n_bats > 0] |> 
    unique() |> sort()
  all_dates = c()
  for(i in seq_len(nrow(sub_meta))){
    start = sub_meta$Deployment.service.date[i]+1
    end = sub_meta$Recovery.date[i]-2 
    if(end < start) stop('End smaller than start!')
    all_dates = seq(start, end, by = 'day')
    triggers_station = dat[dat$station == st,]
    all_dates = all_dates[all_dates %in% triggers_station$date]
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
dat_model = dat_model[dat_model$date < as.Date('2024-04-11'),]

# Add weather
weather_stations = read.csv(path_stations)
## add weather for each offshore night
message('Getting weather for offshore nights.')
for(st in unique(dat_model$station)){
  print(st)
  ws = weather_stations$weather_station[weather_stations$station_id == st]
  if(length(ws) != 1) stop('Did not find ', st)
  weather = read.table(sprintf('%s/%s_era5.pre', path_weather, ws),
                       skip = 2, header = TRUE)
  weather$wind_direction = (270 - weather$wind_direction * 180 / pi) %% 360
  for(row in which(dat_model$station == st)){
    sub = weather[weather$year == dat_model$date[row] |> str_sub(1, 4) &
                    weather$month == dat_model$date[row] |> str_sub(6, 7) |> 
                    as.numeric() &
                    weather$day == dat_model$date[row] |> str_sub(9, 10) |> 
                    as.numeric() &
                    weather$hour == 23,]
    if(nrow(sub) != 1) stop('Weather not found for row ', row)
    dat_model[row, c('mean_temp', 'wind_speed', 'wind_direction', 'precip')] = 
      sub[c('mean_temp', 'wind_speed', 'wind_direction', 'precip')]
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

# Remove first deployment A01 og all of B01
summary = summary[!(summary$station == 'A01' & 
                      summary$date < as.Date('2023-10-01')),]
summary = summary[!summary$station == 'B01',]
dat = dat[!dat$station == 'B01',]

# Add GPS coordinates stations
dat_model = merge(dat_model, weather_stations[,c('station_id', 'lat', 'long')],
                  by.x = 'station', by.y = 'station_id', 
                  all.x = TRUE, all.y = FALSE)

# Colours species 
colours = c(
  '#1f77b4', # A soft blue
  '#2ca02c', # A strong green
  '#FFC107', # A soft yellow
  '#B03A2E'  # A dark red
)
species = c('M', 'NVE', 'Pnat', 'Ppyg')
names(colours) = species

# Create summary per station for Signe
sum_per_station = dat_model |> group_by(station) |> summarise(n = n())

# Create data frame for all bat detections offshore
offshore_bats = st_bats[c('file', 'Begin.time..s.', 'End.time..s.')]
offshore_bats = merge(offshore_bats, species_offshore[c('Fil', 'sp')],
                      by.x = 'file', by.y = 'Fil', all.x = TRUE, all.y = FALSE)
offshore_bats = merge(offshore_bats, dat[c('file_name', 'station', 
                                           'type_location', 'offshore')],
                      by.x = 'file', by.y = 'file_name', 
                      all.x = TRUE, all.y = FALSE)
if(any(is.na(offshore_bats$offshore))) stop('Offshore column not complete!')
offshore_bats = offshore_bats[offshore_bats$offshore,]
  
# Store output
save(dat, dat_model, summary, summary_bats, sun, colours, 
     species_offshore, species, locations_all_buoys,
     file = path_combined_data)
write.csv(sum_per_station, path_summary_per_station, row.names = FALSE)
write.csv(offshore_bats, path_offshore_bats, row.names = FALSE)
message('Stored all data.')

