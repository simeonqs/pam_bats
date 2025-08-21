# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Plots overview of activity throughout night for bøjer.
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
path_combined_data = 'analysis/results/combined_data.RData'
path_png = 'analysis/results/nightly_activity/nightly_activity_bøjer_y1.png'

# Load data and subset for boejer Y1
load(path_combined_data)
dat = dat[which(dat$type_location == 'boejer' & dat$offshore),]
dat = dat[dat$station != 'NS29',]
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
stations = unique(dat$station[!is.na(dat$species)])
stations_with_leading_0 = stations |>
  vapply(function(st) if(str_detect(st, 'NS')) 
    sprintf('NS%02d', as.numeric(strsplit(st, 'NS')[[1]][2])) else
      st, character(1))
for(i in order(stations_with_leading_0, decreasing = FALSE)){
  
  # Subset for station
  st = stations[i]
  sub = dat[which(dat$station == st),]
  sub_with_detections = sub[!is.na(sub$species) & sub$offshore,]
  
  # Skip if no dates left
  if(nrow(sub_with_detections) == 0) next
  
  # Add species(complex)
  species_station = vapply(sub_with_detections$file_name, function(x){
    species_offshore$sp[species_offshore$Fil == x]
  }, character(1))
  
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
  
  # Plot points
  points(sub_with_detections$night_date, 
         sub_with_detections$night_time, 
         pch = 20, col = colours[species_station], cex = 1)
  
  # Add info plot
  text(as.Date('2023-04-15'), 0.93*1440, stations_with_leading_0[i], 
       font = 2, adj = 0, cex = 1.5)
  if(st %in% c('HR3-4', 'NS25', 'NS30', 'NS34')){
    axis(2, at = 60*c(2, 10, 18), c('14:00', '20:00', '06:00'), cex.axis = 1.4)
    mtext('Time (UTC)', 2, 2.8, cex = 1)
  }
  if(st %in% c('NS35', 'NS33', 'NS34')){
    axis.Date(side = 1, at = seq(as.Date('2023-05-01'),
                                 as.Date('2024-04-01'),
                                 by = 'month'),
              cex.axis = 1.4,
              labels = c('M', 'J', 'J', 'A',
                         'S', 'O', 'N', 'D', 'J',
                         'F', 'M', 'A'), format='%b')    
    mtext('Month', 1, 2.8, cex = 1)
  }
  
} # end file loop

# Print legend
plot.new()
legend('bottom',
       legend = species[species %in% dat$species], 
       col = colours[names(colours) %in% dat$species], 
       pch = 16,
       cex = 1.5,
       ncol = 3,
       bty = 'n')

# Close png
dev.off()

# Message
message('Printed all figures.')