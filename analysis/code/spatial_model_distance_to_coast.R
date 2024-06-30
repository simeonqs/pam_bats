# Run spatial_model_plot_detections.R first

stations = unique(plot_dat[c('station', 'lat', 'long', 'cat')])

library(rnaturalearth)
library(sf)

world_coastline <- ne_coastline(scale = "large", returnclass = "sf")
stations_sf <- st_as_sf(stations, coords = c("long", "lat"), crs = 4326)
nearest_coast_distance <- st_distance(stations_sf, world_coastline)
stations$distance_to_coast <- (nearest_coast_distance |> apply(1, min))/1000

write.csv(stations, 'analysis/results/spatial_model/distance_to_coast.csv',
          row.names = FALSE)