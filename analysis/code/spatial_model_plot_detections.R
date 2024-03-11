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

# Load
files_aspot_bats = list.files(path_aspot_bats, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)
summary_aspot_bats = files_aspot_bats |>
  lapply(read.csv) |> bind_rows()
meta = read.csv(path_meta)
meta = meta[c('Station.ID', 'Lat..N.', 'Long..E.')]
meta = meta[!duplicated(meta$Station.ID),]

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

# Make map
theme_set(theme_bw())
world = ne_countries(scale = 'large', returnclass = 'sf')
plot = ggplot(data = world) +
  geom_sf(fill = 'antiquewhite') + 
  geom_point(data = dat, aes(x = Long..E., y = Lat..N., size = log(n))) +
  annotation_scale(location = 'bl', width_hint = 0.5) +
  annotation_north_arrow(location = 'bl', which_north = 'true', 
                         pad_x = unit(0.75, 'in'), pad_y = unit(0.5, 'in'),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(6.5, 9.5), ylim = c(55.5, 56.6)) + 
  labs(x = 'Longitude', y = 'Latitude') +
  theme(panel.background = element_rect(fill = 'aliceblue'))
ggsave(path_pdf, plot, width = 10, height = 8)





