# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Plot bat activity vs weather. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
path_combined_data = 'analysis/results/combined_data.RData'
path_png_temp = 'analysis/results/spatial_model/figure_temp_boejer.png'
path_png_vind_ret = 'analysis/results/spatial_model/figure_vind_ret_boejer.png'
path_png_vind_hast = 
  'analysis/results/spatial_model/figure_vind_hast_boejer.png'

# Load data
load(path_combined_data)

# Summarise data per night
dat_model = dat_model[str_detect(dat_model$station, 'NS') |
                        dat_model$station %in% c('HR3-4', 'HR3-6'),]
sum_dat = data.frame(date = unique(dat_model$date))
sum_dat$mean_temp = vapply(sum_dat$date, function(date) 
  mean(dat_model$mean_temp[dat_model$date == date]), numeric(1))
sum_dat$wind_direction = vapply(sum_dat$date, function(date) 
  mean(dat_model$wind_direction[dat_model$date == date]), numeric(1))
sum_dat$wind_speed = vapply(sum_dat$date, function(date) 
  mean(dat_model$wind_speed[dat_model$date == date]), numeric(1))
sum_dat$n_stations_with_bats = vapply(sum_dat$date, function(date) 
  sum(dat_model$detection[dat_model$date == date]), numeric(1))

# Plot
png(path_png_temp, width = 7, height = 5, units = 'in', res = 800)
par(mar = c(4, 4, 1, 4))
plot(sum_dat$date, sum_dat$mean_temp,
     col = '#D4E6F1', ylim = c(-5, 22),
     xlab = 'Date', ylab = 'Temperature (°C)', xaxt = 'n')
points(sum_dat$date, sum_dat$n_stations_with_bats * 2, 
       type = 'h', col = '#1F618D')
axis(4, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) * 2, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
unique_months = unique(format(ymd(sum_dat$date), '%Y-%m'))
axis(1, at = as.Date(paste0(unique_months, '-01')), 
     labels = paste0(unique_months, '-01'),
     cex.axis = 1)
mtext('Number of stations with bats that night', 4, 3)
dev.off()

png(path_png_vind_ret, width = 7, height = 5, units = 'in', res = 800)
par(mar = c(4, 4, 1, 4))
plot(sum_dat$date, sum_dat$wind_direction,
     col = '#D4E6F1', ylim = c(0, 360),
     xlab = 'Date', ylab = 'Wind direction', xaxt = 'n', yaxt = 'n')
points(sum_dat$date, sum_dat$n_stations_with_bats * 40, 
       type = 'h', col = '#1F618D')
unique_months = unique(format(ymd(sum_dat$date), '%Y-%m'))
axis(1, at = as.Date(paste0(unique_months, '-01')), 
     labels = paste0(unique_months, '-01'),
     cex.axis = 1)
axis(2, at = seq(0, 315, 45), 
     labels = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'))
axis(4, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) * 40, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
mtext('Number of stations with bats that night', 4, 3)
dev.off()

png(path_png_vind_hast, width = 7, height = 5, units = 'in', res = 800)
par(mar = c(4, 4, 1, 4))
plot(sum_dat$date, sum_dat$wind_speed,
     col = '#D4E6F1', ylim = c(0, 22),
     xlab = 'Date', ylab = 'Wind speed (m/s)', xaxt = 'n')
points(sum_dat$date, sum_dat$n_stations_with_bats * 2, 
       type = 'h', col = '#1F618D')
unique_months = unique(format(ymd(sum_dat$date), '%Y-%m'))
axis(1, at = as.Date(paste0(unique_months, '-01')), 
     labels = paste0(unique_months, '-01'),
     cex.axis = 1)
axis(4, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) * 2, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
mtext('Number of stations with bats that night', 4, 3)
dev.off()





