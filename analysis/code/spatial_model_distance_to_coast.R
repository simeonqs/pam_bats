# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Calculates distance to the coast for each station.  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'sf', 'rnaturalearth')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
path_combined_data = 'analysis/results/combined_data.RData'

# Load data
load(path_combined_data)

# Calculate distance
stations = unique(dat_model[c('station', 'lat', 'long')])
world_coastline = ne_coastline(scale = 'large', returnclass = 'sf')
stations_sf = st_as_sf(stations, coords = c('long', 'lat'), crs = 4326)
nearest_coast_distance = st_distance(stations_sf, world_coastline)
stations$distance_to_coast = (nearest_coast_distance |> apply(1, min))/1000

# Store as csv
write.csv(stations, 'analysis/results/spatial_model/distance_to_coast.csv',
          row.names = FALSE)

# Plot distance vs detection
merged = merge(dat_model, stations, by = 'station', 
               all.x = TRUE, all.y = FALSE)
dat_model$station_night = paste(dat_model$station, dat_model$date)
sum_dat = dat |> group_by(station, night_date) |> 
  summarise(n = sum(n_bats > 0, na.rm = TRUE))
sum_dat$station_night = paste(sum_dat$station, sum_dat$night_date)
merged = merge(merged, sum_dat, by = 'station_night',
               all.x = TRUE, all.y = FALSE)
plot(merged$distance_to_coast, log2(merged$n+1))
