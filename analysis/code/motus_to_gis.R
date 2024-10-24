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
path_gis_out = 'analysis/results/motus/motus_data.shp'

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
