# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Plot bat activity vs weather. 
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
path_png_temp = 'analysis/results/spatial_model/figure_temp.png'
path_png_vind_ret = 'analysis/results/spatial_model/figure_vind_ret.png'
path_png_vind_hast = 'analysis/results/spatial_model/figure_vind_hast.png'
path_png_nedboer = 'analysis/results/spatial_model/figure_nedboer.png'
path_weather = 'analysis/data/weather/generated_data'

# Load data
load(path_combined_data)

# Summarise data per night and station
sum_dat = dat[dat$offshore,] |>
  group_by(station, night_date) |>
  summarise(bats_present = any(!is.na(species))) |>
  group_by(night_date) |>
  summarise(n_stations_with_bats = length(which(bats_present)))

# Add weather data
ws = 'Loc_17' # NS20, ca. i midten
weather = read.table(sprintf('%s/%s_era5.pre', path_weather, ws),
                     skip = 2, header = TRUE)
weather$wind_direction = (270 - weather$wind_direction * 180 / pi) %% 360
for(row in seq_len(nrow(sum_dat))){
  time_sunset = sun$Sunset[sun$Date == sum_dat$night_date[row]] |>
    as.POSIXct(format = '%H:%M:%S') |>
    round_date(unit = 'hour') |>
    format('%H') |>
    as.numeric()
  sub = weather[weather$year == sum_dat$night_date[row] |> str_sub(1, 4) &
                  weather$month == sum_dat$night_date[row] |> str_sub(6, 7) |> 
                  as.numeric() &
                  weather$day == sum_dat$night_date[row] |> str_sub(9, 10) |> 
                  as.numeric() &
                  weather$hour == time_sunset,]
  if(nrow(sub) != 1) stop('Weather not found for row ', row)
  sum_dat[row, c('mean_temp', 'wind_speed', 'wind_direction', 'precip',
                 'cloud_coverage', 'atm_pressure')] = 
    sub[c('mean_temp', 'wind_speed', 'wind_direction', 'precip',
          'cloud_coverage', 'atm_pressure')]
}

# Plot
png(path_png_temp, width = 10, height = 4.2, units = 'in', res = 800)
par(mar = c(4, 4, 1, 4))
plot(sum_dat$night_date, sum_dat$mean_temp,
     col = '#D4E6F1', ylim = c(-3, 22),
     xlab = 'Date', ylab = 'Temperature (°C)', xaxt = 'n')
points(sum_dat$night_date[sum_dat$n_stations_with_bats > 0], 
       sum_dat$mean_temp[sum_dat$n_stations_with_bats > 0],
       col = '#1F618D', pch = 16)
points(sum_dat$night_date, sum_dat$n_stations_with_bats * 2, 
       type = 'h', col = '#1F618D')
axis(4, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) * 2, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
unique_months = unique(format(ymd(sum_dat$night_date), '%Y-%m'))
axis(1, at = as.Date(paste0(unique_months, '-01')), 
     labels = unique_months,
     cex.axis = 1)
mtext('Number of stations with bats that night', 4, 3)
dev.off()

png(path_png_vind_ret, width = 10, height = 4.2, units = 'in', res = 800)
par(mar = c(4, 4, 1, 4))
plot(sum_dat$night_date, sum_dat$wind_direction,
     col = '#D4E6F1', ylim = c(0, 360),
     xlab = 'Date', ylab = 'Wind direction', xaxt = 'n', yaxt = 'n')
points(sum_dat$night_date[sum_dat$n_stations_with_bats > 0], 
       sum_dat$wind_direction[sum_dat$n_stations_with_bats > 0],
       col = '#1F618D', pch = 16)
points(sum_dat$night_date, sum_dat$n_stations_with_bats * 360/11, 
       type = 'h', col = '#1F618D')
unique_months = unique(format(ymd(sum_dat$night_date), '%Y-%m'))
axis(1, at = as.Date(paste0(unique_months, '-01')), 
     labels = unique_months,
     cex.axis = 1)
axis(2, at = seq(0, 315, 45), 
     labels = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'))
axis(4, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) * 360/11, 
     c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
mtext('Number of stations with bats that night', 4, 3)
dev.off()

png(path_png_vind_hast, width = 10, height = 4.2, units = 'in', res = 800)
par(mar = c(4, 4, 1, 4))
plot(sum_dat$night_date, sum_dat$wind_speed,
     col = '#D4E6F1', ylim = c(0, 22),
     xlab = 'Date', ylab = 'Wind speed (m/s)', xaxt = 'n')
points(sum_dat$night_date[sum_dat$n_stations_with_bats > 0], 
       sum_dat$wind_speed[sum_dat$n_stations_with_bats > 0],
       col = '#1F618D', pch = 16)
points(sum_dat$night_date, sum_dat$n_stations_with_bats * 2, 
       type = 'h', col = '#1F618D')
unique_months = unique(format(ymd(sum_dat$night_date), '%Y-%m'))
axis(1, at = as.Date(paste0(unique_months, '-01')), 
     labels = unique_months,
     cex.axis = 1)
axis(4, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) * 2, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
mtext('Number of stations with bats that night', 4, 3)
dev.off()

png(path_png_nedboer, width = 10, height = 4.2, units = 'in', res = 800)
par(mar = c(4, 4, 1, 4))
plot(sum_dat$night_date, sum_dat$precip,
     col = '#D4E6F1', ylim = c(0, 4),
     xlab = 'Date', ylab = 'Precipitation (mm)', xaxt = 'n')
points(sum_dat$night_date[sum_dat$n_stations_with_bats > 0], 
       sum_dat$precip[sum_dat$n_stations_with_bats > 0],
       col = '#1F618D', pch = 16)
points(sum_dat$night_date, sum_dat$n_stations_with_bats * 4/11, 
       type = 'h', col = '#1F618D')
unique_months = unique(format(ymd(sum_dat$night_date), '%Y-%m'))
axis(1, at = as.Date(paste0(unique_months, '-01')), 
     labels = unique_months,
     cex.axis = 1)
axis(4, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) * 4/11, 
     c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
mtext('Number of stations with bats that night', 4, 3)
dev.off()




