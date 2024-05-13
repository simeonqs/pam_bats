# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Plots overview of active periods and detections in png. 
# This version is for fugletogter recordings. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'lubridate', 'scales')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_summaries = 'analysis/results/activity_overview/summaries/summaries'
path_detections = 'analysis/results/activity_overview/summaries/detections'
path_aspot = 'analysis/results/activity_overview/summaries/aspot'
path_aspot_bats = 'analysis/results/activity_overview/summaries/aspot_bats'
path_png = 'analysis/results/activity_overview'

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

# List files
files_summaries = list.files(path_summaries, pattern = '*.csv', 
                             recursive = TRUE, full.names = TRUE)
files_summaries = files_summaries[str_detect(files_summaries, 'ONBOARD')]
files_detections = list.files(path_detections, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)
files_detections = files_detections[str_detect(files_detections, 'ONBOARD')]
files_aspot = list.files(path_aspot, pattern = '*.csv', 
                         recursive = TRUE, full.names = TRUE)
files_aspot = files_aspot[str_detect(files_aspot, 'togter_no_season')]

# Read all files
summary = files_summaries |>
  lapply(read.csv) |> bind_rows()
summary_detections = files_detections |>
  lapply(read.csv) |> bind_rows()
summary_aspot = files_aspot |>
  lapply(read.csv) |> bind_rows()

# Plot
unique_stations = summary_detections$station |> unique() |> 
  sort(decreasing = TRUE)
trans_stations = seq_along(unique_stations)
names(trans_stations) = unique_stations
## create colour gradient
colfunc = colorRampPalette(c('#FAD7A0', '#0B5345'))
cols = colfunc(max(summary$n))
for(season in c('Forår 2023', 'Efterår 2023')){
  png(sprintf('%s/%s_fugletogter.png', path_png, season),
      width = 12.5, height = 2, units = 'in', res = 1000) # open PNG
  # season = 'Forår 2023'
  par(mar = c(5, 7, 3, 1))
  ## subset per season and adjust xlims
  if(season == 'Forår 2023'){
    sub = summary[which(as.Date(summary$DATE, format = '%Y-%b-%d') <= 
                          as.Date('2023-07-15')),]
    sub_detections = 
      summary_detections[which(as.Date(summary_detections$date, 
                                       format = '%Y-%m-%d') <=
                                 as.Date('2023-07-15')),]
    sub_aspot = 
      summary_aspot[which(as.Date(summary_aspot$DATE) <= 
                            as.Date('2023-07-15')),]
    xlim = as.Date(c('2023-04-10', '2023-06-30'))
  } else {
    sub = summary[which(as.Date(summary$DATE, format = '%Y-%b-%d') > 
                          as.Date('2023-07-15')),]
    sub_detections = 
      summary_detections[which(as.Date(summary_detections$date, 
                                       format = '%Y-%m-%d') > 
                                 as.Date('2023-07-15')),]
    sub_aspot = 
      summary_aspot[which(as.Date(summary_aspot$DATE) > 
                            as.Date('2023-07-15')),]
    xlim = as.Date(c('2023-07-30', '2023-11-15'))
  }
  ## create empty plot
  plot(as.Date(sub$DATE, format = '%Y-%b-%d'),
       trans_stations[sub$station],
       ylim = c(min(trans_stations) - 0.5, max(trans_stations) + 0.5),
       xlim = xlim,
       xaxt = 'n', yaxt = 'n', type = 'n',
       xlab = 'Dato', ylab = '', main = season)
  ## add filled squares and colour by activity
  for(i in seq_len(nrow(sub))){
    points_custom(as.Date(sub$DATE[i], format = '%Y-%b-%d'),
                  trans_stations[sub$station[i]], 
                  my_shape1, col = cols[sub$n[i]])
  }
  ## add scaled dots for number detections
  points(as.Date(sub_detections$date, format = '%Y-%m-%d'),
         trans_stations[sub_detections$station] + 0.15, pch = 16, 
         cex = log10(sub_detections$n)/4 + 0.1)
  ## add scaled dots for number detections from aspot
  points(as.Date(sub_aspot$DATE),
         trans_stations[sub_aspot$station] - 0.15, pch = 16, 
         cex = log10(sub_aspot$n)/4 + 0.1)
  ## add axes
  unique_months = unique(format(ymd(sub$DATE), '%Y-%m'))
  axis(1, at = as.Date(paste0(unique_months, '-01')), 
       labels = paste0(unique_months, '-01') |> str_sub(6, 10))
  axis(1, at = as.Date(paste0(unique_months, '-10')), 
       labels = paste0(unique_months, '-10') |> str_sub(6, 10))
  axis(1, at = as.Date(paste0(unique_months, '-20')), 
       labels = paste0(unique_months, '-20') |> str_sub(6, 10))
  dev.off() # close PNG
}

