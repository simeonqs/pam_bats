# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Plots overview of what time recordings were made.  
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
path_data_bats = '/home/au472091/Documents/results_aspot/defenitely_bats'

# Get detections
detections = load.selection.tables(path_data_bats, recursive = TRUE)
stations = detections$file |> strsplit('_') |> sapply(`[`, 1) |> 
  str_remove('C') |> str_remove('LOT1') |> str_remove('T3') |> 
  str_remove_all('-')
times = detections$file |> strsplit('_') |> sapply(`[`, 3) |> 
  strptime(format = '%H%M%S') |> as.character()
times = ifelse(times < strptime('120000', format = '%H%M%S'),
               str_replace(times, str_sub(Sys.time(), 1, 10), '2000-01-02'),
               str_replace(times, str_sub(Sys.time(), 1, 10), '2000-01-01')) |>
  as.POSIXct(format = '%Y-%m-%d %H:%M:%S')

# Plot
unique_stations = unique(stations) |> sort()
trans_stations = seq_along(unique_stations)
names(trans_stations) = unique_stations
par(mar = c(5, 5, 1, 1))
plot(times, trans_stations[stations],
     xlim = as.POSIXct(c('2000-01-01 18:00:00',
                         '2000-01-02 04:00:00'),
                       format = '%Y-%m-%d %H:%M:%S'),
     yaxt = 'n', xlab = 'time [hh:mm]', ylab = '')
axis(2, trans_stations, names(trans_stations), las = 1)

