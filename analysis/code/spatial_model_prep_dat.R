# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Prepares data for spatial models.
# The dataframe with detailed information is stored as csv. All data, 
# including the summarised data for the model is stored as RData file.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'lubridate', 'geosphere', 'callsync')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
path_detections = '/home/au472091/Documents/results_aspot/defenitely_bats'
path_trigger = 'analysis/results/activity_overview/summaries_backup/detections'
path_meta_boejer = 'analysis/data/meta_data_boejer.csv'
path_meta_surveys = 'analysis/data/meta_data_bird_surveys.csv'
path_meta_HRIII = 'analysis/data/meta_data_HRIII.csv'
path_weather = 'analysis/data/weather/generated_data'
path_stations = 'analysis/data/weather/stations.csv'
path_out_all_detections = 
  'analysis/results/combined_results/all_offshore_detections.csv'
path_dat_model = 'analysis/results/spatial_model/dat_model.RData'
path_percentages_bats = 
  'analysis/results/combined_results/percentages_bats.csv'
path_sun = 'analysis/data/sunrise_sunset_west_coast_DK.csv'
path_table_wind_directions = 
  'analysis/results/spatial_model/wind_directions.csv'

# Load sun
sun = read.csv(path_sun)

# List all files with detections animal spot
files_all = list.files(path_detections, '*txt', 
                       recursive = TRUE, full.names = TRUE)
files = files_all[str_detect(files_all, 'NS') | str_detect(files_all, 'HR3-4')]
files = files[!str_detect(files, 'NS29')]

# List all files with detections recorder
files_detections = list.files(path_trigger, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)

# Load detections recorder
summary_detections = files_detections |>
  lapply(read.csv) |> bind_rows()

# Fix data and DATE column
summary_detections$date[is.na(summary_detections$date)] = 
  summary_detections$DATE[is.na(summary_detections$date)]

# Translate station names
summary_detections$station = 
  ifelse(summary_detections$station == 'HR3-4C', 'HR3-4S-C', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'T3-NS26C', 'T3-NS26-C', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'T3-NS26', 'T3-NS26-C', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'NS6', 'NS6-C', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'NS27', 'NS27S', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'NS19-LOT1', 'NS19', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'NS28', 'NS28S', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'NS24', 'NS24S', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'NS32', 'NS32S', 
         summary_detections$station)

summary_detections$station = 
  ifelse(summary_detections$station == 'HR-Y', 'B01', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'HR3-Y', 'E05', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'HR-V', 'F03', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'HR-X', 'A01', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'HR3-Z', 'C03', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'HR3-X', 'A02', 
         summary_detections$station)
summary_detections$station = str_remove(summary_detections$station, 'HR-')
summary_detections$station = str_remove(summary_detections$station, 'A-')
summary_detections$station = str_remove(summary_detections$station, 'B-')
summary_detections$station = str_remove(summary_detections$station, 'C-')

# Get date and time
dates = files |> basename() |> str_extract('\\d{8}')
times = files |> basename() |> strsplit('_') |> sapply(`[`, 3)

# Get station names
stations = files |> basename() |> 
  strsplit('_') |> sapply(`[`, 1)

# Fix station names
stations = ifelse(stations == 'HR3_4', 'HR3-4S-C', stations)
stations = ifelse(stations == 'T3-NS26', 'T3-NS26-C', stations)
stations = ifelse(stations == 'T3-NS26C', 'T3-NS26-C', stations)
stations = ifelse(stations == 'NS6', 'NS6-C', stations)
stations = ifelse(stations == 'NS6C', 'NS6-C', stations)
stations = ifelse(stations == 'NS24', 'NS24S', stations)
stations = ifelse(stations == 'NS19-LOT1', 'NS19', stations)
stations = ifelse(stations == 'NS28', 'NS28S', stations)

# Load weather stations
weather_stations = read.csv(path_stations)

# Load meta data
meta = read.csv(path_meta_boejer)
meta = meta[!is.na(meta$recovery.date),]
meta$Station.ID = ifelse(meta$Station.ID %in% c('HR3_4', 'HR3-4'), 'HR3-4S-C',
                         meta$Station.ID)
meta$Station.ID = ifelse(str_detect(meta$Station.ID, 'H_R3_6'), 'H_R3_6',
                         meta$Station.ID)
meta$Station.ID = ifelse(meta$Station.ID == 'T3/NS26', 'T3-NS26-C',
                         meta$Station.ID)
meta$Station.ID = ifelse(meta$Station.ID == 'NS6', 'NS6-C',
                         meta$Station.ID)
meta$Station.ID = ifelse(meta$Station.ID == 'NS24', 'NS24S',
                         meta$Station.ID)
meta$Station.ID = ifelse(meta$Station.ID == 'NS27', 'NS27S',
                         meta$Station.ID)
meta$Station.ID = ifelse(meta$Station.ID == 'NS28', 'NS28S',
                         meta$Station.ID)
meta$Station.ID = ifelse(meta$Station.ID == 'NS32', 'NS32S',
                         meta$Station.ID)

# Make proper date columns
meta$Deployment..Service.date = meta$Deployment..Service.date |> 
  as.character() |>
  as.Date(format = '%Y%m%d')
meta$recovery.date = meta$recovery.date |> 
  as.character() |>
  as.Date(format = '%Y%m%d')

# Combine data
dat = data.frame(file = basename(files) |> 
                   str_remove('_predict_output.log.annotation.result.txt'),
                 station = stations,
                 date = dates |> as.Date(format = '%Y%m%d'),
                 time = times)

# Remove on-shore detections
for(st in unique(dat$station)){
  sub_meta = meta[meta$Station.ID == st,]
  dates_station = dat$date[dat$station == st] |> unique() |> sort()
  keep_dates = c()
  for(i in seq_len(nrow(sub_meta))){
    start = sub_meta$Deployment..Service.date[i]
    end = sub_meta$recovery.date[i] 
    keep_dates = c(keep_dates, 
                   dates_station[dates_station > start &
                                   dates_station < (end - 2)] |> 
                     as.character())
  } 
  remove_dates = dates_station[!dates_station %in% keep_dates]
  dat = dat[!(dat$station == st & dat$date %in% remove_dates),]
}

# Add subset
dat$subset = 'Buoys'

# Add bird surveys
files = files_all[str_detect(files_all, 'ONBOARD')]

# Get date and time
dates = files |> basename() |> str_extract('\\d{8}')
times = files |> basename() |> strsplit('_') |> sapply(`[`, 3)

# Combine data
dat_togter = 
  data.frame(file = basename(files) |> 
               str_remove('_predict_output.log.annotation.result.txt'),
             date = dates |> as.Date(format = '%Y%m%d'),
             time = times)

# Find station
meta_surveys = read.csv(path_meta_surveys)
meta_surveys$start_location = as.POSIXct(paste(meta_surveys$at_location_date,
                                               meta_surveys$at_location_time),
                                         format = '%Y-%m-%d %H:%M')
meta_surveys$end_location = as.POSIXct(paste(meta_surveys$leave_location_date,
                                             meta_surveys$leave_location_time),
                                       format = '%Y-%m-%d %H:%M')
for(row in seq_len(nrow(dat_togter))){
  date_time = as.POSIXct(paste(dat_togter$date[row],
                               dat_togter$time[row]),
                         format = '%Y-%m-%d %H%M')
  sub = meta_surveys[meta_surveys$start_location < date_time &
                       meta_surveys$end_location > date_time,]
  if(nrow(sub) != 1) stop('Could not find survey location for row ', row)
  dat_togter$station[row] = sub$location
}

# Merge with other data
dat_togter$subset = 'Surveys'
dat = rbind(dat, dat_togter)

# Add HRIII
files = files_all[str_detect(files_all, 'HRIII')]

# Get date and time
dates = files |> basename() |> str_extract('\\d{8}')
times = files |> basename() |> strsplit('_') |> sapply(`[`, 3)

# Get station names
stations = files |> basename() |> 
  strsplit('_') |> sapply(`[`, 1)
stations = ifelse(stations == 'HR-Y', 'B01', stations)
stations = ifelse(stations == 'HR3-Y', 'E05', stations)
stations = ifelse(stations == 'HR-V', 'F03', stations)
stations = ifelse(stations == 'HR-X', 'A01', stations)
stations = ifelse(stations == 'HR3-Z', 'C03', stations)
stations = ifelse(stations == 'HR3-X', 'A02', stations)
stations = str_remove(stations, 'HR-')
stations = str_remove(stations, 'A-')
stations = str_remove(stations, 'B-')
stations = str_remove(stations, 'C-')

# Combine data
dat_HRIII = 
  data.frame(file = basename(files) |> 
               str_remove('_predict_output.log.annotation.result.txt'),
             station = stations,
             date = dates |> as.Date(format = '%Y%m%d'),
             time = times)

# Remove on-shore detections
meta_HRIII = read.csv(path_meta_HRIII)
meta_HRIII$Deployment.service.date = meta_HRIII$Deployment.service.date |> 
  as.character() |>
  as.Date(format = '%m/%d/%Y')
meta_HRIII$Recovery.date = meta_HRIII$Recovery.date |> 
  as.character() |>
  as.Date(format = '%m/%d/%Y')
for(st in unique(dat_HRIII$station)){
  sub_meta = meta_HRIII[meta_HRIII$WT.ID == st,]
  dates_station = dat_HRIII$date[dat_HRIII$station == st] |> unique() |> sort()
  keep_dates = c()
  for(i in seq_len(nrow(sub_meta))){
    start = sub_meta$Deployment.service.date[i]
    end = sub_meta$Recovery.date[i] 
    keep_dates = c(keep_dates, 
                   dates_station[dates_station > start &
                                   dates_station < (end - 2)] |> 
                     as.character())
  } 
  remove_dates = dates_station[!dates_station %in% keep_dates]
  dat_HRIII = dat_HRIII[!(dat_HRIII$station == st & 
                            dat_HRIII$date %in% remove_dates),]
}

# Merge with other data
dat_HRIII$subset = 'Windturbines'
dat = rbind(dat, dat_HRIII)

# Remove anything from Y2
dat = dat[dat$date <= as.Date('2024-04-20'),]

# Add weather information
for(st in unique(dat$station)){
  ws = weather_stations$weather_station[weather_stations$station_id == st]
  if(length(ws) != 1) stop('Did not find ', st)
  weather = read.table(sprintf('%s/%s_era5.pre', path_weather, ws),
                       skip = 2, header = TRUE)
  for(row in which(dat$station == st)){
    sub = weather[weather$year == dat$date[row] |> str_sub(1, 4) &
                    weather$month == dat$date[row] |> str_sub(6, 7) |> 
                    as.numeric() &
                    weather$day == dat$date[row] |> str_sub(9, 10) |> 
                    as.numeric() &
                    weather$hour == dat$time[row] |> str_sub(1, 2) |> 
                    as.numeric(),]
    if(nrow(sub) != 1) stop('Weather not found for row ', row)
    dat[row, c('mean_temp', 'wind_speed', 'wind_direction', 'precip')] = 
      sub[c('mean_temp', 'wind_speed', 'wind_direction', 'precip')]
  }
}

# Add GPS locations
for(row in seq_len(nrow(dat))){
  sub = weather_stations[weather_stations$station_id == dat$station[row],] 
  if(nrow(sub) != 1) stop('Station not found for row ', row)
  dat[row, c('lat', 'long')] = sub[,c('lat', 'long')] |> as.numeric()
}

# Add number of detections for each file
dat$n_detections = vapply(dat$file, function(file){
  full_file = files_all[str_detect(files_all, file)]
  st = load.selection.table(full_file)
  return(nrow(st))
}, numeric(1))

# Add date for that night
dates_times = paste(dat$date, dat$time, sep = '_')
new_dates_times = ymd_hms(dates_times) - hours(12)
dat$night_date = new_dates_times |> as.Date(format = '%Y:%m:%d')

# Write results
write.csv(dat, path_out_all_detections, row.names = FALSE)

# Create empty data.frame with all dates for all stations
dat_model = data.frame()
## boejer
for(st in unique(meta$Station.ID)){
  sub_meta = meta[meta$Station.ID == st,]
  dates_station = dat$night_date[dat$station == st] |> unique() |> sort()
  all_dates = c()
  for(i in seq_len(nrow(sub_meta))){
    start = sub_meta$Deployment..Service.date[i]
    end = sub_meta$recovery.date[i] 
    all_dates = seq(start+1, end-2, by = 'day')
    triggers_station = summary_detections[summary_detections$station == st,]
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
  dates_station = dat$night_date[dat$station == st] |> unique() |> sort()
  all_dates = c()
  for(i in seq_len(nrow(sub_meta))){
    start = sub_meta$Deployment.service.date[i]
    end = sub_meta$Recovery.date[i] 
    all_dates = seq(start+1, end-2, by = 'day')
    triggers_station = summary_detections[summary_detections$station == st,]
    all_dates = all_dates[all_dates %in% triggers_station$DATE]
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

# Only keep Y1
dat_model = dat_model[dat_model$date < as.Date('2024-04-10'),]

# Add average weather for detections
## takes the weather around sunset
for(st in unique(dat_model$station)){
  ws = weather_stations$weather_station[weather_stations$station_id == st]
  if(length(ws) != 1) stop('Did not find ', st)
  weather = read.table(sprintf('%s/%s_era5.pre', path_weather, ws),
                       skip = 2, header = TRUE)
  for(row in which(dat_model$station == st)){
    d = dat_model$date[row]
    sunset = strsplit(sun$Sunset[as.Date(sun$Date) == d],':')[[1]][1]
    sub = weather[weather$year == dat_model$date[row] |> str_sub(1, 4) &
                    weather$month == dat_model$date[row] |> str_sub(6, 7) |> 
                    as.numeric() &
                    weather$day == dat_model$date[row] |> str_sub(9, 10) |> 
                    as.numeric() &
                    weather$hour == sunset |> 
                    as.numeric(),]
    if(nrow(sub) != 1) stop('Weather not found for row ', row)
    dat_model[row, c('mean_temp', 'wind_speed', 'wind_direction', 'precip')] = 
      sub[c('mean_temp', 'wind_speed', 'wind_direction', 'precip')]
  }
}

# Add GPS locations
for(row in seq_len(nrow(dat_model))){
  sub = weather_stations[weather_stations$station_id == 
                           dat_model$station[row],] 
  if(nrow(sub) != 1) stop('Station not found for row ', row)
  dat_model[row, c('lat', 'long')] = sub[,c('lat', 'long')] |> as.numeric()
}

# Translate station to numeric
trans_stations = seq_along(unique(dat_model$station))
names(trans_stations) = sort(unique(dat_model$station))
dat_model$station_numeric = trans_stations[dat_model$station]

# Make distance matrix per station
combs = combn(trans_stations, 2)
out = sapply(seq_len(ncol(combs)), function(x) {
  st_1 = names(trans_stations)[combs[1,x]]
  l_1 = dat_model[c('long', 'lat')][
    dat_model$station == st_1,][1,]
  st_2 = names(trans_stations)[combs[2,x]]
  l_2 = dat_model[c('long', 'lat')][
    dat_model$station == st_2,][1,]
  return( distm(l_1, 
                l_2, 
                fun = distHaversine) )
})
out = out/max(out)
d_mat = o.to.m(out, names(trans_stations))

# Table % nights with bats
percentages_bats = dat_model |> group_by(station) |> 
  summarise(n_nights_bats = length(which(detection)),
            n_nights_total = length(detection))
percentages_bats$percentage_bats = 
  (percentages_bats$n_nights_bats / 
     percentages_bats$n_nights_total * 100) |> round(2)
write.csv(percentages_bats, path_percentages_bats, row.names = FALSE)

# # Table with wind directions
# table_wind = table(dat_model$station[dat_model$detection],
#                    round(dat_model$wind_direction[dat_model$detection]/2))
# colnames(table_wind) = c('ESE', 'WNW')
# write.csv(table_wind, path_table_wind_directions)

# Store data
save(dat, dat_model, trans_stations, d_mat, file = path_dat_model)

# Message
message('Stored data for model.')
