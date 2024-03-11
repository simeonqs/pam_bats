# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Prepares data for spatial models. Dates the start dates for 
# each night. So, 2023-01-02_02:00:00 becomes 2023-01-01. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'lubridate')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
path_detections = '/home/au472091/Documents/results_aspot/defenitely_bats'
path_meta = 'analysis/data/meta_data_boejer.csv'
path_dat_model = 'analysis/results/spatial_model/dat_model.RData'

# List all files with detections
files = list.files(path_detections, '*txt', recursive = TRUE)

# Remove STRANDET
files = files[!str_detect(files, 'STRAND')]

# Get night-dates
dates = files |> basename() |> str_extract('\\d{8}')
times = files |> basename() |> strsplit('_') |> sapply(`[`, 3)
date_time = paste(dates, times, sep = '_') |> 
  as.POSIXct(format = '%Y%m%d_%H%M%S')

# Add 12 hours to the time
modified_date_time = date_time - hours(12)

# Extract the date and time components
nights = data.frame(
  original_date = dates,
  modified_date = as.Date(modified_date_time),
  original_time = format(date_time, '%H:%M:%S'),
  modified_time = format(modified_date_time, '%H:%M:%S')
)

# Get station names
stations = files |> basename() |> 
  strsplit('_') |> sapply(`[`, 1)

# Load meta data
meta = read.csv(path_meta)
meta = meta[c('Station.ID', 'Lat..N.', 'Long..E.')]
meta = meta[!duplicated(meta$Station.ID),]

# Fix station names
stations = 
  ifelse(
    stations == 'HR3-4S-C', 'HR3_4',
    ifelse(
      stations %in% c('T3-NS26', 'T3-NS26-C', 'T3-NS26C'), 'T3/NS26',
      ifelse(
        stations == 'NS6-C', 'NS6',
        ifelse(
          stations == 'NS24S', 'NS24',
          ifelse(
            stations == 'NS6C', 'NS6',
            ifelse(
              stations == 'NS19-LOT1', 'NS19',
              stations))))))

# Combine data
dat = data.frame(date = nights$modified_date,
                 station = stations)
dat = unique(dat)

# Get missing dates
min_date = min(dat$date)
max_date = max(dat$date)
dat_model = data.frame()
for(date in seq(min_date, max_date, by = 'day')){
  for(station in unique(meta$Station.ID)){
    dat_model = 
      rbind(dat_model,
            data.frame(station = station,
                       date = as.Date(date),
                       present = as.numeric(any(dat$station == station &
                                                  dat$date == date))))
  }
}

# Add coordinates
dat_model = merge(dat_model, meta, by.x = 'station', by.y = 'Station.ID',
                  all.x = TRUE, all.y = FALSE)

# Store data
save(dat_model, file = path_dat_model)

# Message
message('Stored data for model.')