# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Plot bat activity cross the night for offshore data. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'lubridate')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
path_combined_data = 'analysis/results/combined_data.RData'
path_png_buoy = 'analysis/results/spatial_model/figure_time_buoy.png'
path_png_hriii = 'analysis/results/spatial_model/figure_time_hriii.png'

# Load data
load(path_combined_data)
dat = dat[dat$offshore & !is.na(dat$species),]

# Add time after sunset
sun$Date = as.Date(sun$Date)
dat$date = as.Date(dat$date)
dat = merge(dat, sun[, c('Date', 'Sunset')], 
             by.x = 'date', by.y = 'Date', all.x = TRUE)
rec_dt = as.POSIXct(paste(dat$date, dat$time), 
                    format = '%Y-%m-%d %H:%M:%S')
sunset_dt = as.POSIXct(paste(dat$date, dat$Sunset), 
                       format = '%Y-%m-%d %H:%M:%S')
dat$hours_after_sunset = round(as.numeric(difftime(rec_dt, 
                                                   sunset_dt, 
                                                   units = 'hours')))
dat$hours_after_sunset[dat$hours_after_sunset < -10] = 
  dat$hours_after_sunset[dat$hours_after_sunset < -10] + 24

# Plot
png(path_png_buoy, width = 10, height = 4.2, units = 'in', res = 800)
par(mar = c(4, 4, 1, 4))
hist(dat$hours_after_sunset[dat$type_location == 'boejer' & 
                              dat$species == 'Pnat'],
     xlim = c(0, 8),
     main = '', xlab = 'Hour after sunset', 
     ylab = 'Number of recordings')
dev.off()

png(path_png_hriii, width = 10, height = 4.2, units = 'in', res = 800)
par(mar = c(4, 4, 1, 4))
hist(dat$hours_after_sunset[dat$type_location == 'HRIII' & 
                              dat$species == 'Pnat'],
     xlim = c(0, 8),
     main = '', xlab = 'Hour after sunset', 
     ylab = 'Number of recordings')
dev.off()




