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
path_summaries = 
  'analysis/results/activity_overview/summaries_backup/summaries'
path_detections = 
  'analysis/results/activity_overview/summaries_backup/detections'
path_aspot = 'analysis/results/activity_overview/summaries/aspot'
path_aspot_bats = 'analysis/results/activity_overview/summaries/aspot_bats'
path_pdf = 'analysis/results/activity_overview/activity_overview_'
path_meta = 'analysis/data/meta_data_bird_surveys.csv'

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
# files_aspot = list.files(path_aspot, pattern = '*.csv', 
#                          recursive = TRUE, full.names = TRUE)
# files_aspot = files_aspot[str_detect(files_aspot, 'togter_no_season')]
files_aspot_bats = list.files(path_aspot_bats, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)
files_aspot_bats = files_aspot_bats[str_detect(files_aspot_bats, 
                                               'togter')]

# Read all files
summary = files_summaries |>
  lapply(read.csv) |> bind_rows()
summary_detections = files_detections |>
  lapply(read.csv) |> bind_rows()
# summary_aspot = files_aspot |>
#   lapply(read.csv) |> bind_rows()
summary_aspot_bats = files_aspot_bats |>
  lapply(read.csv) |> bind_rows()
meta = read.csv(path_meta, na.strings = '')

# Subset for only start and end dates
meta = meta[,c('start_date', 'start_time', 'end_date', 'end_time')]
if(nrow(meta) < 3) stop('Meta not complete.')
for(i in 2:nrow(meta)){
  if(is.na(meta$start_date[i])){
    meta$start_date[i] = meta$start_date[i-1]
    meta$start_time[i] = meta$start_time[i-1]
  }
}
for(i in (nrow(meta)-1):1){
  if(is.na(meta$end_date[i])){
    meta$end_date[i] = meta$end_date[i+1]
    meta$end_time[i] = meta$end_time[i+1]
  }
}
meta = unique(meta)

# Make proper date columns
summary$DATE = as.Date(summary$DATE, format = '%Y-%b-%d')
summary_detections$date = as.Date(summary_detections$date, 
                                  format = '%Y-%m-%d')
summary_aspot_bats$DATE = as.Date(summary_aspot_bats$DATE)
meta$start_date = as.Date(meta$start_date)
meta$end_date = as.Date(meta$end_date)

# Plot
pdf(sprintf('%sfugle_togter.pdf', path_pdf),
    width = 40, height = 2.5) 
par(mar = c(5, 7, 3, 1))
## create colour gradient
colfunc = colorRampPalette(c('#FAD7A0', '#0B5345'))
cols = colfunc(max(summary$n))
## subset per season and adjust xlims
if(TRUE){
  sub = summary[which(summary$DATE < as.Date('2024-04-10')),]
  sub_detections = 
    summary_detections[which(summary_detections$date < as.Date('2024-04-10')),]
  # sub_aspot = 
  #   summary_aspot[which(as.Date(summary_aspot$DATE) < 
  #                         as.Date('2024-04-10')),]
  sub_aspot_bats =
    summary_aspot_bats[which(summary_aspot_bats$DATE <
                               as.Date('2024-04-10')),]
  xlim = as.Date(c('2023-04-10', '2024-04-10'))
} else {}
## create empty plot
plot(sub$DATE,
     ylim = c(0, 1),
     xlim = xlim,
     xaxt = 'n', yaxt = 'n', type = 'n',
     xlab = 'Dato', ylab = '')
## add filled squares and colour by activity
for(i in seq_len(nrow(sub))){
  points_custom(sub$DATE[i], 0.5,
                my_shape1, col = cols[sub$n[i]])
}
# ## add scaled dots for number detections
points(sub_detections$date, rep(0.5 + 0.15, nrow(sub_detections)), 
       pch = 16, cex = log10(sub_detections$n)/4 + 0.1)
## add scaled dots for number detections from aspot
# points(as.Date(sub_aspot$DATE),
#        trans_stations[sub_aspot$station] - 0.15, pch = 16, 
#        cex = log10(sub_aspot$n)/4 + 0.1)
points(sub_aspot_bats$DATE, rep(0.5 - 0.15, nrow(sub_aspot_bats)), 
       pch = 16, cex = log10(sub_aspot_bats$n)/4 + 0.1, col = '#28B463')
## add axes
unique_months = unique(format(ymd(sub$DATE), '%Y-%m'))
axis(1, at = as.Date(paste0(unique_months, '-01')), 
     labels = paste0(unique_months, '-01'), las = 1)
# axis(1, at = as.Date(paste0(unique_months, '-10')), 
#      labels = paste0(unique_months, '-10'), las = 0)
# axis(1, at = as.Date(paste0(unique_months, '-20')), 
#      labels = paste0(unique_months, '-20'), las = 0.5)
dev.off() # close PDF


