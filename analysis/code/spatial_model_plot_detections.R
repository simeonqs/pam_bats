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
path_aspot_bats = 'analysis/results/activity_overview/summaries/aspot_bats'
path_meta = 'analysis/data/meta_data_boejer.csv'
path_pdf = 'analysis/results/spatial_model/map_detections.pdf'
path_results = 'analysis/results/spatial_model/fit_and_dat_model.Data'

# Load
files_aspot_bats = list.files(path_aspot_bats, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)
summary_aspot_bats = files_aspot_bats |>
  lapply(read.csv) |> bind_rows()
meta = read.csv(path_meta)
meta = meta[c('Station.ID', 'Lat..N.', 'Long..E.')]
meta = meta[!duplicated(meta$Station.ID),]
load(path_results)

# Fix station names
summary_aspot_bats$station = 
  ifelse(
    summary_aspot_bats$station == 'HR3-4S-C', 'HR3_4',
    ifelse(
      summary_aspot_bats$station %in% c('T3-NS26', 'T3-NS26-C'), 'T3/NS26',
      ifelse(
        summary_aspot_bats$station == 'NS6-C', 'NS6',
        ifelse(
          summary_aspot_bats$station == 'NS24S', 'NS24',
          ifelse(
            summary_aspot_bats$station == 'NS6C', 'NS6',
            summary_aspot_bats$station)))))

# Summarise and merge
summary = summary_aspot_bats |> group_by(station) |> summarise(n = sum(n))
dat = merge(summary, meta, by.x = 'station', by.y = 'Station.ID',
            all.x = TRUE, all.y = FALSE)

# Adding missing stations
missing_stations = meta[!meta$Station.ID %in% dat$station,]
colnames(missing_stations)[
  colnames(missing_stations) == 'Station.ID'] = 'station'
missing_stations$n = exp(1)
missing_stations$col = 'missing'
dat$col = 'present'
plot_dat = rbind(dat, missing_stations)

# Create a grid of new data points
new_data_grid = expand.grid(Lat..N. = seq(min(plot_dat$Lat..N.) - 0.5, 
                                          max(plot_dat$Lat..N.) + 0.5, 
                                          length.out = 100),
                            Long..E. = seq(min(plot_dat$Long..E.) - 0.5, 
                                           max(plot_dat$Long..E.) + 0.5, 
                                           length.out = 100))
new_data_grid$min_dist = vapply(seq_len(nrow(new_data_grid)), function(i)
  return(min(st_distance(st_as_sf(new_data_grid[i, c('Lat..N.', 'Long..E.')], 
                                  coords = c('Long..E.', 'Lat..N.')),
                         st_as_sf(plot_dat[c('Lat..N.', 'Long..E.')], 
                                  coords = c('Long..E.', 'Lat..N.'))))),
  numeric(1))

new_data_grid = new_data_grid[new_data_grid$min_dist < 0.2,]

# Predict probabilities
new_data_grid$predicted_prob = predict(fit, newdata = new_data_grid, 
                                       type = 'response')

# Make map
theme_set(theme_bw())
world = ne_countries(scale = 'large', returnclass = 'sf')
plot = ggplot(data = world) +
  geom_tile(aes(x = Long..E., y = Lat..N., fill = predicted_prob),
            data = new_data_grid, alpha = 0.7) +
  scale_fill_gradient(low = 'white', high = 'darkgreen') +
  geom_sf(fill = 'antiquewhite') + 
  geom_point(data = plot_dat, aes(x = Long..E., y = Lat..N., 
                                  size = log(n), col = col)) +
  scale_color_manual(values = c('darkred', 'darkgreen')) +
  annotation_scale(location = 'bl', width_hint = 0.5) +
  annotation_north_arrow(location = 'bl', which_north = 'true', 
                         pad_x = unit(0.75, 'in'), pad_y = unit(0.5, 'in'),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(6.5, 9.5), ylim = c(55.5, 56.6)) + 
  labs(x = 'Longitude', y = 'Latitude',
       fill = 'Predicted probability',
       size = 'Log number recordings',
       col = 'Bats detected') +
  theme(panel.background = element_rect(fill = 'aliceblue'))
ggsave(path_pdf, plot, width = 10, height = 8)

