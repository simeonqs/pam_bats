# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Plots overview of active periods and detections in png. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'scales', 'lubridate')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_combined_data = 'analysis/results/combined_data_land.RData'
path_png = 'analysis/results/activity_overview/activity_overview_land_y1.png'

# Plotting function
points_custom <- function(x, y, shape, col = 'black', cex = 1, ...) {
  if(missing(shape)) {
    points(x, y, col = col, cex = cex, ...) 
  } 
  else {
    shape <- lapply(shape, function(z) z * cex)
    Map(function(x_i, y_i) {
      a <- grconvertX(grconvertX(x_i, 'user', 'inches') + 
                        shape$x, 'inches', 'user')
      b <- grconvertY(grconvertY(y_i, 'user', 'inches') + 
                        shape$y, 'inches', 'user')
      polygon(a, b, col = col, border = col, ...)
    }, x_i = x, y_i = y)
  }
  invisible(NULL)
}
dev = 2.5
my_shape1 = list(x = c(-0.1, 0.1, 0.1, -0.1)/ dev, 
                 y = c(-0.5, -0.5, 0.5, 0.5)/ dev) 

# Load data
load(path_combined_data)
dat = dat[dat$type_location == 'land' & dat$station != 'Skagen',]
summary = summary[summary$type_location == 'land' & 
                    summary$station != 'Skagen',]

# Plot
unique_stations = dat$station |> unique() |> sort(decreasing = TRUE)
labels_stations = unique_stations
trans_stations = seq_along(unique_stations)
names(trans_stations) = 
  unique_stations[order(labels_stations, decreasing = TRUE)]
labels_stations = sort(labels_stations, decreasing = TRUE)
## create colour gradient
colfunc = colorRampPalette(c('#EAEDED', '#5F6A6A'))
cols = colfunc(max(summary$n))
png(path_png,
    width = 40, height = 11, units = 'in', res = 800)
par(mar = c(5, 18, 1, 1), xaxs = 'i', yaxs = 'i')
## subset per season and adjust xlims
if(TRUE){
  sub = dat[which(dat$date < as.Date('2024-04-10')),]
  sub_summary = 
    summary[which(summary$date < as.Date('2024-04-10')),]
  xlim = as.Date(c('2023-04-10', '2024-04-10'))
} else {
  xlim = as.Date(c('2023-07-30', '2023-11-15'))
}
## create empty plot
ymin = min(trans_stations) - 0.45
ymax = max(trans_stations) + 0.45
plot(sub$date,
     trans_stations[sub$station],
     ylim = c(ymin, ymax),
     xlim = xlim,
     xaxt = 'n', yaxt = 'n', type = 'n',
     xlab = '', ylab = '')
mtext('Date', 1, 3.5, cex = 2.5)
mtext('Station', 2, 16, cex = 2.5)
## add shaded area for migration
polygon(as.Date(c('2023-04-10', '2023-04-10', '2023-05-15','2023-05-15')),
        c(ymin, ymax, ymax, ymin),
        col = '#E8DAEF', border = NA)
polygon(as.Date(c('2023-08-15', '2023-08-15', '2023-10-15','2023-10-15')),
        c(ymin, ymax, ymax, ymin),
        col = '#E8DAEF', border = NA)
polygon(as.Date(c('2024-04-01', '2024-04-01', '2024-04-10','2024-04-10')),
        c(ymin, ymax, ymax, ymin),
        col = '#E8DAEF', border = NA)
## add filled squares and colour by activity
for(i in seq_len(nrow(sub_summary))){
  points_custom(sub_summary$date[i],
                trans_stations[sub_summary$station[i]], 
                my_shape1, col = cols[sub_summary$n[i]])
}
## add scaled dots for number detections
summary_detections = sub |> group_by(station, date) |> 
  summarise(n = n(), .groups = 'drop')
points(summary_detections$date,
       trans_stations[summary_detections$station] + 0.15, pch = 16, 
       cex = log10(summary_detections$n)/4 + 0.1)
points(sub$date[!is.na(sub$species)],
       trans_stations[sub$station[!is.na(sub$species)]] - 0.15, pch = 16, 
       cex = 1.5, col = '#D68910')
## add axes
unique_months = unique(format(ymd(sub$date), '%Y-%m'))
axis(1, at = as.Date(paste0(unique_months, '-01')), 
     labels = paste0(unique_months, '-01'),
     cex.axis = 2)
axis(2, trans_stations, labels_stations, las = 1, cex.axis = 2)
dev.off() # close png