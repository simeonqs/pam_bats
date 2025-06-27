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
path_png_temp = 'analysis/results/spatial_model/figure_temp.png'
path_png_vind_ret = 'analysis/results/spatial_model/figure_vind_ret.png'
path_png_vind_hast = 'analysis/results/spatial_model/figure_vind_hast.png'

# Load data
load(path_combined_data)

# Summarise data per night
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
     ylim = c(-5, 22), cex = 0.75, 
     col = ifelse(sum_dat$n_stations_with_bats > 0,
                  '#1F618D', '#A9CCE3'), 
     pch = ifelse(sum_dat$n_stations_with_bats > 0, 19, 1),
     xlab = 'Month', ylab = 'Temperature (°C)', xaxt = 'n')
points(sum_dat$date, sum_dat$n_stations_with_bats * 2, 
       type = 'h', col = ifelse(sum_dat$n_stations_with_bats > 0,
                                '#1F618D', '#A9CCE3'))
axis(4, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) * 2, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
axis.Date(side = 1, at = seq(as.Date('2023-05-01'),
                             as.Date('2024-04-01'),
                             by = 'month'),
          labels = c('M', 'J', 'J', 'A',
                     'S', 'O', 'N', 'D', 'J',
                     'F', 'M', 'A'), format='%b')    
mtext('Number of stations with bats that night', 4, 3)
dev.off()

png(path_png_vind_ret, width = 7, height = 5, units = 'in', res = 800)
par(mar = c(4, 4, 1, 4))
plot(sum_dat$date, sum_dat$wind_direction,
     ylim = c(0, 360),
     col = ifelse(sum_dat$n_stations_with_bats > 0,
                  '#1F618D', '#A9CCE3'),
     pch = ifelse(sum_dat$n_stations_with_bats > 0, 19, 1),
     xlab = 'Month', ylab = 'Wind direction', xaxt = 'n', yaxt = 'n')
points(sum_dat$date, sum_dat$n_stations_with_bats * 40,
       type = 'h', col = ifelse(sum_dat$n_stations_with_bats > 0,
                                '#1F618D', '#A9CCE3'))
axis.Date(side = 1, at = seq(as.Date('2023-05-01'),
                             as.Date('2024-04-01'),
                             by = 'month'),
          labels = c('M', 'J', 'J', 'A',
                     'S', 'O', 'N', 'D', 'J',
                     'F', 'M', 'A'), format='%b')
axis(2, at = seq(0, 315, 45),
     labels = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'))
axis(4, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) * 40,
     c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
mtext('Number of stations with bats that night', 4, 3)

# Convert wind_direction from degrees to radians
sum_dat$wind_direction_rad = (sum_dat$wind_direction + 90) * pi / 180

# Define the radius of the inner circle
inner_radius = 0.3  

# Create circular plot without box or axes
par(fig = c(0.55, 0.88, 0.45, 0.99), new = TRUE, mar = c(0, 0, 0, 0))
plot(0, 0, type = 'n', 
     xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), xlab = '', ylab = '',
     asp = 1, axes = FALSE, frame.plot = FALSE)
rect(-1.1, -1.1, 1.1, 1.1, col = rgb(1, 1, 1, alpha = 0.7), border = NA)

# Draw bars starting from the inner circle
scale_factor = 0.1 
for (i in 1:nrow(sum_dat)) {
  lines(c(inner_radius * cos(sum_dat$wind_direction_rad[i]),
          (inner_radius + sum_dat$n_stations_with_bats[i] * scale_factor) *
            cos(sum_dat$wind_direction_rad[i])),
        c(inner_radius * sin(sum_dat$wind_direction_rad[i]),
          (inner_radius + sum_dat$n_stations_with_bats[i] * scale_factor) *
            sin(sum_dat$wind_direction_rad[i])), 
        lwd = 2, col = ifelse(sum_dat$n_stations_with_bats[i] > 0,
                              '#1F618D', '#A9CCE3'))
}

# Add labels for wind directions (cardinal points)
labels = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW')
text(1.1 * cos(seq(pi/2, 2*pi + pi/2, length.out = 9)[-9]),
     1.1 * sin(seq(pi/2, 2*pi + pi/2, length.out = 9)[-9]),
     labels = labels, cex = 0.8)
dev.off()

png(path_png_vind_hast, width = 7, height = 5, units = 'in', res = 800)
par(mar = c(4, 4, 1, 4))
plot(sum_dat$date, sum_dat$wind_speed,
     ylim = c(0, 22),
     col = ifelse(sum_dat$n_stations_with_bats > 0,
                  '#1F618D', '#A9CCE3'), 
     pch = ifelse(sum_dat$n_stations_with_bats > 0, 19, 1),
     xlab = 'Month', ylab = 'Wind speed (m/s)', xaxt = 'n')
points(sum_dat$date, sum_dat$n_stations_with_bats * 2, 
       type = 'h', col = ifelse(sum_dat$n_stations_with_bats > 0,
                                '#1F618D', '#A9CCE3'))
axis.Date(side = 1, at = seq(as.Date('2023-05-01'),
                             as.Date('2024-04-01'),
                             by = 'month'),
          labels = c('M', 'J', 'J', 'A',
                     'S', 'O', 'N', 'D', 'J',
                     'F', 'M', 'A'), format='%b')    
axis(4, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) * 2, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
mtext('Number of stations with bats that night', 4, 3)
dev.off()





