# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Plots number detections on map.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'ggplot2', 'sf', 'rnaturalearth',
              'rnaturalearthdata', 'ggspatial')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_combined_data = 'analysis/results/combined_data.RData'
path_png = 'analysis/results/spatial_model/map_detections_Y2.png'
path_gis_out = 'analysis/results/spatial_model/bat_pam_results_Y2.shp'

# Load data
load(path_combined_data)

# Check if all observations have lat/long
missing = which(is.na(dat_model$lat) | is.na(dat_model$long))
if(length(missing) > 0) stop('Missing lat/long.')

# Subset dat
dat = dat[dat$date >= as.Date('2024-04-10'),]

# Summarise data
summary = dat[which(!is.na(dat$species) & 
                dat$offshore & 
                dat$type_location != 'fugletogter'),] |> 
  group_by(station, type_location) |> 
  summarise(n = length(unique(night_date)))
summary$col = 'Present'

# Adding missing stations
missing_stations = data.frame(
  station = unique(dat$station[!dat$station %in% summary$station]),
  n = 0,
  col = 'Missing')
plot_dat = rbind(summary, missing_stations)

# Add back information
info_dat = unique(dat_model[c('station', 'lat', 'long', 'subset')])
plot_dat = merge(plot_dat, info_dat, by = 'station')
plot_dat$type_location[is.na(plot_dat$type_location)] = 
  plot_dat$subset[is.na(plot_dat$type_location)]

# Make map
trans_loc = c(HRIII = 'Windturbines',
              boejer = 'Buoys',
              Windturbines = 'Windturbines',
              Buoys = 'Buoys')
plot_dat$cat = paste(plot_dat$col, trans_loc[plot_dat$type_location]) |> 
  factor(levels = c('Present Buoys', 'Present Windturbines',
                    'Missing Buoys', 'Missing Windturbines',
                    'Present OSS'))
plot_dat$cat[plot_dat$station == 'platform'] = 'Present OSS'
plot_dat$station[plot_dat$station == 'platform'] = 'OSS'
# theme_set(theme_bw())
# world = ne_countries(scale = 'large', returnclass = 'sf')
# plot = ggplot(data = world) +
#   # geom_tile(aes(x = Long..E., y = Lat..N., fill = predicted_prob),
#   #           data = new_data_grid, alpha = 0.7) +
#   scale_fill_gradient(low = 'white', high = 'darkgreen') +
#   geom_sf(fill = 'antiquewhite') + 
#   geom_point(data = plot_dat, aes(x = long, y = lat, 
#                                   col = cat, size = n + 1,
#                                   pch = subset)) +
#   scale_color_manual(values = c('springgreen4', 'springgreen4',
#                                 'darkred', 'darkred'),
#                      labels = c('Present at buoys', 
#                                 'Present at windturbines',
#                                 'Absent at buoys', 
#                                 'Absent at windturbines')) +
#   annotation_scale(location = 'bl', width_hint = 0.5) +
#   annotation_north_arrow(location = 'bl', which_north = 'true', 
#                          pad_x = unit(0.4, 'in'), pad_y = unit(0.5, 'in'),
#                          style = north_arrow_fancy_orienteering) +
#   coord_sf(xlim = c(6.5, 9.5), ylim = c(55.5, 56.6)) + 
#   labs(x = 'Longitude', y = 'Latitude',
#        fill = 'Predicted probability',
#        size = 'Nights with bats',
#        col = 'Type and colour') +
#   scale_size(breaks = c(1, 16, 1, 16),
#              range = c(3, 8)) +
#   guides(
#     color = guide_legend(order = 1, 
#                          override.aes = list(pch = c(16, 17, 16, 17),
#                                              cex = 3),
#                          position = 'inside'),
#     size = guide_legend(order = 2, 
#                         override.aes = list(color = 'springgreen4',
#                                             pch = c(16, 16, 17, 17)),
#                         position = 'inside'),
#     pch = 'none'
#   ) +
#   theme(panel.background = element_rect(fill = 'aliceblue'),
#         legend.background = element_rect(fill = NA, color = NA), 
#         legend.key = element_rect(fill = NA, color = NA),
#         legend.justification.inside = c(0.92, 0.5))
# print(plot)
# ggsave(path_png, plot, width = 8, height = 5.7, units = 'in', dpi = 800)

## Add non-bat stations for GIS map
gis_dat = plot_dat[c('station', 'cat', 'lat', 'long', 'n')]
non_bat = locations_all_buoys[!locations_all_buoys$Position.ID %in% 
                                gis_dat$station,
                              c('Position.ID', 'Deployment.LAT_DD', 
                                'Deployment.LON_DD')]
colnames(non_bat) = c('station', 'lat', 'long')
non_bat = non_bat[!duplicated(non_bat$station),]
non_bat$cat = ifelse(str_detect(non_bat$station, 'HR'), 
                     'Non-bat windturbines', 
                     'Non-bat buoys')
non_bat$n = 0
non_bat = non_bat[!non_bat$station == 'NS6',]
gis_dat = rbind(gis_dat, non_bat)
gis_dat$n = gis_dat$n/2 + 3

# Add leading zero and reorder
gis_dat$station = ifelse(
  grepl('^NS\\d$', gis_dat$station),
  sub('^NS(\\d)$', 'NS0\\1', gis_dat$station),
  gis_dat$station
)
gis_dat = gis_dat[order(gis_dat$station),]

# Export data for GIS
sf_object = st_as_sf(gis_dat, 
                     coords = c('long', 'lat'), 
                     crs = 4326)
UTM = st_transform(sf_object, crs = 25832)
utm_coords = st_coordinates(UTM)
gis_dat$UTM_E <- utm_coords[,1] |> round()
gis_dat$UTM_N <- utm_coords[,2] |> round()
gis_dat_sf = st_as_sf(gis_dat, coords = c('long', 'lat'), crs = 4326)
st_write(gis_dat_sf, path_gis_out, append = FALSE)
