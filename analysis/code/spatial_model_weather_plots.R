# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Plots number detections on map.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

stop('Check wind direction!')

# Paths 
path_png = 'analysis/results/spatial_model/weather_plots.png'
path_dat_model = 'analysis/results/spatial_model/dat_model.RData'

# Load data
load(path_dat_model)

# Plot
png(path_png, 6, 4, units = 'in', res = 800)
par(mfrow = c(2, 2), oma = c(1, 3, 0, 0), mar = c(4, 1, 1, 1))
plot(dat_model$mean_temp, 
     as.numeric(dat_model$detection) + rnorm(nrow(dat_model), 0, 0.02),
     yaxt = 'n', pch = 16, col = '#2E86C1',
     xlab = 'Mean modelled temperature at sunset',
     ylab = '')
axis(2, c(0, 1), c('No', 'Yes'))
mtext('Bats detected', 2, 2.5, cex = 0.75)
plot(dat_model$precip,  
     as.numeric(dat_model$detection) + rnorm(nrow(dat_model), 0, 0.02),
     yaxt = 'n', pch = 16, col = '#2E86C1',
     xlab = 'Mean modelled precipitation at sunset',
     ylab = '')
plot(dat_model$wind_speed,  
     as.numeric(dat_model$detection) + rnorm(nrow(dat_model), 0, 0.02),
     yaxt = 'n', pch = 16, col = '#2E86C1',
     xlab = 'Mean modelled wind speed at sunset',
     ylab = '')
axis(2, c(0, 1), c('No', 'Yes'))
mtext('Bats detected', 2, 2.5, cex = 0.75)
plot(dat_model$wind_direction,  
     as.numeric(dat_model$detection) + rnorm(nrow(dat_model), 0, 0.02),
     yaxt = 'n', pch = 16, col = '#2E86C1',
     xlab = 'Mean modelled wind direction at sunset',
     ylab = '')
dev.off()

# Output extra csv for Signe
dat_detections = dat_model[dat_model$detection,]
dat_signe = dat[dat$station != 'sw',]
for(row in seq_len(nrow(dat_signe))){
  sub = dat_detections[dat_detections$station == dat_signe$station[row] &
                         dat_detections$date == dat_signe$night_date[row],]
  if(nrow(sub) != 1) stop('Not exactly one entry found.')
  dat_signe[row, 
            c('temp_sunset', 'wind_speed_sunset', 'wind_direction_sunset')] = 
    sub[c('mean_temp', 'wind_speed', 'wind_direction')]
}
par(mfrow = c(2, 2))
plot(dat_signe$mean_temp, dat_signe$temp_sunset)
plot(dat_signe$wind_speed, dat_signe$wind_speed_sunset)
plot(dat_signe$wind_direction, dat_signe$wind_direction_sunset)
write.csv2(dat_signe, '~/Desktop/dat_signe.csv', row.names = FALSE)
dev.off()
par(mfrow = c(2, 2))
plot(dat$mean_temp, dat$n_detections)
plot(dat$wind_speed, dat$n_detections)
plot(dat$wind_direction, dat$n_detections)
