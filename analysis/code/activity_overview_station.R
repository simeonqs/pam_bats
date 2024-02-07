# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Plots overview of active periods and detections in png. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'lubridate', 'scales')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Settings
station = 'NS35'
ylim = c(-100, 2000)

# Paths 
path_summaries = 'analysis/results/activity_overview/summaries/summaries'
path_detections = 'analysis/results/activity_overview/summaries/detections'
path_aspot = 'analysis/results/activity_overview/summaries/aspot'
path_aspot_bats = 'analysis/results/activity_overview/summaries/aspot_bats'
path_meta = 'analysis/data/meta_data_boejer.csv'
path_pdf = 'analysis/results/activity_overview/stations'

# List files
files_summaries = list.files(path_summaries, pattern = '*.csv', 
                             recursive = TRUE, full.names = TRUE)
files_detections = list.files(path_detections, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)
files_aspot = list.files(path_aspot, pattern = '*.csv', 
                         recursive = TRUE, full.names = TRUE)
files_aspot_bats = list.files(path_aspot_bats, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)

# Read all files
summary = files_summaries |>
  lapply(read.csv) |> bind_rows()
summary_detections = files_detections |>
  lapply(read.csv) |> bind_rows()
summary_aspot = files_aspot |>
  lapply(read.csv) |> bind_rows()
summary_aspot_bats = files_aspot_bats |>
  lapply(read.csv) |> bind_rows()
meta = read.csv(path_meta)

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
dev = 4
my_shape1 = list(x = c(-0.1, 0.1, 0.1, -0.1)/ dev, 
                 y = c(-0.2, -0.2, 0.2, 0.2)/ dev) 

# Fix station name
meta$Station.ID = str_remove(meta$Station.ID, 'T3/')

# Subset for station and plot
# create colour gradient
colfunc = colorRampPalette(c('#FAD7A0', '#0B5345'))
cols = colfunc(max(summary$n))
# get data
x = as.Date(summary_detections$date[summary_detections$station == station], 
            format = '%Y-%m-%d')
y = summary_detections$n[summary_detections$station == station][order(x)]
x = x[order(x)]
pdf(sprintf('%s/%s.pdf', path_pdf, station), 5, 3)
layout(matrix(c(1, 1, 2, 2, 3, 3, 3, 3,
                4, 4, 4, 4, 4, 4, 4, 4,
                4, 4, 4, 4, 4, 4, 4, 4,
                4, 4, 4, 4, 4, 4, 4, 4,
                4, 4, 4, 4, 4, 4, 4, 4,
                4, 4, 4, 4, 4, 4, 4, 4,
                4, 4, 4, 4, 4, 4, 4, 4,
                4, 4, 4, 4, 4, 4, 4, 4), 
              byrow = TRUE, nrow = 8, ncol = 8))
# station
par(mar = c(0, 4, 0.5, 0.5))
plot(c(0, 1), c(0, 1), type = 'n', 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
text(0.5, 0.5, station, cex = 1.5, font = 2, col = '#0B5345')
# empty space
plot.new()
# meta data
par(mar = c(0, 0.5, 0.5, 0.5))
plot(c(0, 1), c(0, 1), type = 'n', 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
text(0.5, 0.5, 
     sprintf('# filer: %s, lat: %s, long: %s', 
             sum(summary_detections$n[summary_detections$station == station]),
             meta$Lat..N.[meta$Station.ID == station][1],
             meta$Long..E.[meta$Station.ID == station][1]), 
     cex = 1, font = 2, col = '#0B5345')
# actual plot
par(mar = c(4, 4, 0.5, 0.5))
plot(x, y, 
     ylim = ylim, 
     xaxt = 'n', yaxt = 'n', type = 'n', yaxs = 'i',
     xlab = 'dato', ylab = '# detektioner per dag')
axis(2, c(0, 200, 400, 600))
abline(v = as.Date(meta$Deployment..Service.date[
  meta$Station.ID == station][1]), 
  lty = 2, lwd = 2)
for(i in seq_len(nrow(summary[summary$station == station,]))){
  points_custom(as.Date(summary[summary$station == station,]$DATE[i], 
                        format = '%Y-%b-%d'), -50, 
                my_shape1, col = cols[summary[
                  summary$station == station,]$n[i]])
}
lines(x, y, col = '#2471A3', lwd = 2)
x = summary_aspot$DATE[summary_aspot$station == station] |> as.Date()
y = summary_aspot$n[summary_aspot$station == station]
lines(sort(x), y[order(x)], 
      col = '#E74C3C', lwd = 2)
points(summary_aspot_bats$DATE[summary_aspot_bats$station == station] |> 
         as.Date(), 
       summary_aspot_bats$n[summary_aspot_bats$station == station], 
       col = '#F4D03F', pch = 19, cex = 1.3)
unique_months = unique(format(ymd(x), '%Y-%m'))
axis(1, at = as.Date(paste0(unique_months, '-01')), 
     labels = paste0(unique_months, '-01') |> str_sub(6, 10))
axis(1, at = as.Date(paste0(unique_months, '-10')), 
     labels = paste0(unique_months, '-10') |> str_sub(6, 10))
axis(1, at = as.Date(paste0(unique_months, '-20')), 
     labels = paste0(unique_months, '-20') |> str_sub(6, 10))
legend('topright', 
       legend = c('detektor', 'Aspot', 'flagermus'), 
       col = c('#2471A3', '#E74C3C', '#F4D03F'), 
       lty = c(1, 1, NA), pch = c(NA, NA, 19), lwd = 2)
dev.off()





