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
path_png = 'analysis/results/spatial_model/map_detections.png'
path_gis_out = 'analysis/results/spatial_model/bat_data_gis.csv'
path_dat_model = 'analysis/results/spatial_model/dat_model.RData'

# Load data
load(path_dat_model)

# Summarise data
summary = dat |> group_by(station) |> summarise(n = length(unique(night_date)))
summary$col = 'Present'

# Adding missing stations
missing_stations = data.frame(
  station = unique(dat_model$station[!dat_model$station %in% 
                                              dat$station]),
  n = 1,
  col = 'Missing')
plot_dat = rbind(summary, missing_stations)

# Add back information
info_dat = unique(dat_model[c('station', 'lat', 'long', 'subset')])
plot_dat = merge(plot_dat, info_dat, by = 'station')

# # Create a grid of new data points
# new_data_grid = expand.grid(Lat..N. = seq(min(plot_dat$Lat..N.) - 0.5, 
#                                           max(plot_dat$Lat..N.) + 0.5, 
#                                           length.out = 100),
#                             Long..E. = seq(min(plot_dat$Long..E.) - 0.5, 
#                                            max(plot_dat$Long..E.) + 0.5, 
#                                            length.out = 100))
# new_data_grid$min_dist = vapply(seq_len(nrow(new_data_grid)), function(i)
#   return(min(st_distance(st_as_sf(new_data_grid[i, c('Lat..N.', 'Long..E.')], 
#                                   coords = c('Long..E.', 'Lat..N.')),
#                          st_as_sf(plot_dat[c('Lat..N.', 'Long..E.')], 
#                                   coords = c('Long..E.', 'Lat..N.'))))),
#   numeric(1))
# 
# new_data_grid = new_data_grid[new_data_grid$min_dist < 0.2,]
# 
# Predict probabilities
# new_data_grid$predicted_prob = predict(fit, newdata = new_data_grid, 
#                                        type = 'response')

# Make map
plot_dat$cat = paste(plot_dat$col, plot_dat$subset) |> 
  factor(levels = c('Present Buoys', 'Present Windturbines',
                    'Missing Buoys', 'Missing Windturbines'))
theme_set(theme_bw())
world = ne_countries(scale = 'large', returnclass = 'sf')
plot = ggplot(data = world) +
  # geom_tile(aes(x = Long..E., y = Lat..N., fill = predicted_prob),
  #           data = new_data_grid, alpha = 0.7) +
  scale_fill_gradient(low = 'white', high = 'darkgreen') +
  geom_sf(fill = 'antiquewhite') + 
  geom_point(data = plot_dat, aes(x = long, y = lat, 
                                  col = cat, size = n,
                                  pch = subset)) +
  scale_color_manual(values = c('springgreen4', 'springgreen4',
                                'darkred', 'darkred'),
                     labels = c('Present at buoys', 
                                'Present at windturbines',
                                'Absent at buoys', 
                                'Absent at windturbines')) +
  annotation_scale(location = 'bl', width_hint = 0.5) +
  annotation_north_arrow(location = 'bl', which_north = 'true', 
                         pad_x = unit(0.4, 'in'), pad_y = unit(0.5, 'in'),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(6.5, 9.5), ylim = c(55.5, 56.6)) + 
  labs(x = 'Longitude', y = 'Latitude',
       fill = 'Predicted probability',
       size = 'Nights with bats',
       col = 'Type and colour') +
  scale_size(breaks = c(1, 9, 1, 2),
             range = c(3, 8)) +
  guides(
    color = guide_legend(order = 1, 
                         override.aes = list(pch = c(16, 17, 16, 17),
                                             cex = 3),
                         position = 'inside'),
    size = guide_legend(order = 2, 
                        override.aes = list(color = 'springgreen4',
                                            pch = c(16, 16, 17, 17)),
                        position = 'inside'),
    pch = 'none'
  ) +
  theme(panel.background = element_rect(fill = 'aliceblue'),
        legend.background = element_rect(fill = NA, color = NA), 
        legend.key = element_rect(fill = NA, color = NA),
        legend.justification.inside = c(0.92, 0.5))
print(plot)
ggsave(path_png, plot, width = 8, height = 5.7, units = 'in', dpi = 800)

# Export data for GIS
sf_object = st_as_sf(plot_dat, 
                     coords = c('long', 'lat'), 
                     crs = 4326)
UTM = st_transform(sf_object, crs = 25832)
utm_coords = st_coordinates(UTM)
plot_dat$UTM_E <- utm_coords[,1]
plot_dat$UTM_N <- utm_coords[,2]
write.csv(plot_dat, path_gis_out, row.names = FALSE)
