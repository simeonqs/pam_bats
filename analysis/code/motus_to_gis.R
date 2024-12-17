# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Stores MOTUS data for GIS. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'sf')

for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_motus = 'analysis/data/motus/motus_data.csv'
path_motus_locations = 'analysis/data/motus/receiver-deployments.csv'
path_gis_out = 'analysis/results/motus/motus_data.shp'
path_gis_out_locations = 'analysis/results/motus/motus_locations.shp'
path_fino = 'analysis/results/motus/fino3.shp'

# Export data for GIS
gis_dat = read.csv(path_motus)
sf_object = st_as_sf(gis_dat, 
                     coords = c('long', 'lat'), 
                     crs = 4326)
UTM = st_transform(sf_object, crs = 25832)
utm_coords = st_coordinates(UTM)
gis_dat$UTM_E <- utm_coords[,1] |> round()
gis_dat$UTM_N <- utm_coords[,2] |> round()
gis_dat_sf = st_as_sf(gis_dat, coords = c('long', 'lat'), crs = 4326)
st_write(gis_dat_sf, path_gis_out, append = FALSE)

# Export receiver locations for GIS
stations = read.csv(path_motus_locations)
stations$dtStart = as.POSIXct(stations$dtStart, 
                              format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')
stations$dtEnd = as.POSIXct(stations$dtEnd, 
                              format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')
stations$dtEnd[is.na(stations$dtEnd)] = as.POSIXct('2024-12-01', tz = 'UTC')
stations = stations[stations$isMobile == 'false' & 
                     stations$dtStart < as.POSIXct('2023-11-01', tz = 'UTC') &
                      stations$dtEnd > as.POSIXct('2023-09-01', tz = 'UTC'),]
stations = stations[c('latitude', 'longitude')]
stations_sf = st_as_sf(stations, 
                       coords = c('longitude', 'latitude'), 
                       crs = 4326)
st_write(stations_sf, path_gis_out_locations, append = FALSE)

# Export Fino3
fino = data.frame(longitude = 7.1583,
                  latitude = 55.195)
fino_sf = st_as_sf(fino, 
                       coords = c('longitude', 'latitude'), 
                       crs = 4326)
st_write(fino_sf, path_fino, append = FALSE)
