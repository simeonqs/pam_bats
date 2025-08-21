# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Plots overview of activity throughout night.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'callsync', 'lubridate', 'dplyr')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
path_species_overview = 'analysis/results/species_overview'
path_png = 
  'analysis/results/nightly_activity/nightly_activity_land_eser_y1.png'
path_combined_data = 'analysis/results/combined_data_land.RData'

# Load data
load(path_combined_data)
dat = dat[dat$type_location == 'land' & dat$station != 'Skagen',]
dat = dat[which(dat$date < as.Date('2024-04-10')),]

# Open png
png(path_png, 12, 8, units = 'in', res = 800)
layout(matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3,
                4, 4, 4, 5, 5, 5, 6, 6, 6,
                7, 7, 7, 8, 8, 8, 9, 9, 9,
                10, 10, 10, 11, 11, 11, 12, 12, 12),
              byrow = TRUE, nrow = 4, ncol = 9))
par(mar = rep(0.5, 4), oma = c(3.5, 3.5, 0.5, 0.5))

# Run through stations
for(station in sort(unique(dat$station))){
  
  # Subset data
  sub = dat[dat$station == station,]
  
  # Get activity minutes per species
  sub_with_detections = sub[!is.na(sub$species_combined),]
  rows_to_delete = c()
  new_dat = data.frame()
  for(row in seq_len(nrow(sub_with_detections))){
    if(str_detect(sub_with_detections$species_combined[row], ',')){
      species_found = str_split(sub_with_detections$species_combined[row], 
                                ', ')[[1]]
      rows_to_delete = c(rows_to_delete, row)
      for(sp in species_found){
        extra_entry = sub_with_detections[row,]
        extra_entry$species_combined = sp
        new_dat = rbind(new_dat, extra_entry)
      } 
    }
  }
  sub_with_detections = rbind(sub_with_detections[-rows_to_delete,],
                              new_dat)
  sub_with_detections$night_time_m = round(sub_with_detections$night_time_m)
  sub_with_detections = sub_with_detections[
    !duplicated(sub_with_detections[c('species', 
                                      'night_time_m', 
                                      'night_date')]),]
  sub_with_detections = 
    sub_with_detections[sub_with_detections$species_combined == 'Eser',]
  
  # Skip if not data
  if(nrow(sub_with_detections) == 0){
    plot(NULL, 
         xlim = as.Date(c('2024-04-09', '2025-04-09')), 
         ylim = c(0, 1440),
         xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    axis.Date(side = 1, at = seq(as.Date('2024-05-01'),
                                 as.Date('2025-04-01'),
                                 by = 'month'), 
              labels = '')
    text(as.Date('2024-04-15'), 0.93*1440, 
         station, font = 2, adj = 0, cex = 1.5)
    next
  }
  
  # Make empty plot
  plot(NULL, 
       xlim = as.Date(c('2023-04-10', '2024-04-10')), 
       ylim = c(0, 1440),
       xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
  axis.Date(side = 1, at = seq(as.Date('2023-05-01'),
                               as.Date('2024-04-01'),
                               by = 'month'), 
            labels = '')
  
  # Make shadow for night
  sun_sub = sun[sun$Date > as.Date('2023-04-09') &
                  sun$Date < as.Date('2024-04-10'),]
  polygon(x = c(as.Date(sun_sub$Date)+1, rev(as.Date(sun_sub$Date))),
          y = c(sun_sub$rise_min, rev(sun_sub$set_min)),
          col = '#212F3D', border = '#212F3D')
  
  # Mark missing dates
  all_dates = seq(from = as.Date('2023-04-10'),
                  to = as.Date('2024-04-10'),
                  by = 'day')
  missing_dates = all_dates[!all_dates %in% as.Date(sub$date)]
  for(d in missing_dates) 
    polygon(x = c(d-0.5, d+0.5),
            y = c(0, 1440),
            col = 'white', border = 'white')
  
  # Shuffle order
  sub_with_detections = sub_with_detections[sample(nrow(sub_with_detections)),]
  
  # Plot points
  points(sub_with_detections$night_date, 
         sub_with_detections$night_time, 
         pch = 20, col = colours[sub_with_detections$species_combined], 
         cex = 0.1)
  
  # Add info plot
  text(as.Date('2023-04-15'), 0.93*1440, station, font = 2, adj = 0, cex = 1.5)
  if(station %in% c('Ballum', 'Husby', 'Nyminde', 'Skjern')){
    axis(2, at = 60*c(2, 10, 18), c('14:00', '20:00', '06:00'), cex.axis = 1.4)
    mtext('Time (UTC)', 2, 2.8, cex = 1)
  }
  if(station %in% c('Roemoe', 'Skjern', 'Stadiloe')){
    axis.Date(side = 1, at = seq(as.Date('2023-05-01'),
                                 as.Date('2024-04-01'),
                                 by = 'month'),
              cex.axis = 1.4,
              labels = c('M', 'J', 'J', 'A',
                         'S', 'O', 'N', 'D', 'J',
                         'F', 'M', 'A'), format='%b')    
    mtext('Month', 1, 2.8, cex = 1)
  }
  
} # end station loop

# Close png
dev.off()

# Message
message('Printed all figures.')