# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Plots overview of active periods and detections in png. 
# This version is for HRIII recordings. 
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
# path_aspot = 'analysis/results/activity_overview/summaries/aspot'
path_aspot_bats = 'analysis/results/activity_overview/summaries/aspot_bats'
path_meta_HRIII = 'analysis/data/meta_data_HRIII.csv'
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
files_summaries = files_summaries[str_detect(files_summaries, 'HR')]
files_summaries = files_summaries[!str_detect(files_summaries, 'HR3-4S')]
files_detections = list.files(path_detections, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)
files_detections = files_detections[str_detect(files_detections, 'HR')]
files_detections = files_detections[!str_detect(files_detections, 'HR3-4')]
# files_aspot = list.files(path_aspot, pattern = '*.csv', 
#                          recursive = TRUE, full.names = TRUE)
# files_aspot = files_aspot[str_detect(files_aspot, 'HR')]
# files_aspot = files_aspot[!str_detect(files_aspot, 'HR3-4S')]
files_aspot_bats = list.files(path_aspot_bats, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)
files_aspot_bats = files_aspot_bats[str_detect(files_aspot_bats, 'HR')]

# Read all files
summary = files_summaries |>
  lapply(read.csv) |> bind_rows()
summary_detections = files_detections |>
  lapply(read.csv) |> bind_rows()
# summary_aspot = files_aspot |>
#   lapply(read.csv) |> bind_rows()
summary_aspot_bats = files_aspot_bats |>
  lapply(read.csv) |> bind_rows()

# Translate station names
summary$station = ifelse(summary$station == 'HR-Y', 'B01', summary$station)
summary$station = ifelse(summary$station == 'HR3-Y', 'E05', summary$station)
summary$station = ifelse(summary$station == 'HR-V', 'F03', summary$station)
summary$station = ifelse(summary$station == 'HR-X', 'A01', summary$station)
summary$station = ifelse(summary$station == 'HR3-Z', 'C03', summary$station)
summary$station = ifelse(summary$station == 'HR3-X', 'A02', summary$station)
summary$station = str_remove(summary$station, 'HR-')
summary$station = str_remove(summary$station, 'A-')
summary$station = str_remove(summary$station, 'B-')
summary$station = str_remove(summary$station, 'C-')

summary_detections$station = 
  ifelse(summary_detections$station == 'HR-Y', 'B01', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'HR3-Y', 'E05', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'HR-V', 'F03', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'HR-X', 'A01', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'HR3-Z', 'C03', 
         summary_detections$station)
summary_detections$station = 
  ifelse(summary_detections$station == 'HR3-X', 'A02', 
         summary_detections$station)
summary_detections$station = str_remove(summary_detections$station, 'HR-')
summary_detections$station = str_remove(summary_detections$station, 'A-')
summary_detections$station = str_remove(summary_detections$station, 'B-')
summary_detections$station = str_remove(summary_detections$station, 'C-')

summary_aspot_bats$station = 
  ifelse(summary_aspot_bats$station == 'HR3-Z', 'C03', 
         summary_aspot_bats$station)
summary_aspot_bats$station = 
  ifelse(summary_aspot_bats$station == 'HR3-Y', 'E05', 
         summary_aspot_bats$station)
summary_aspot_bats$station = 
  ifelse(summary_aspot_bats$station == 'HR3-X', 'A02', 
         summary_aspot_bats$station)
summary_aspot_bats$station = str_remove(summary_aspot_bats$station, 'HR-')
summary_aspot_bats$station = str_remove(summary_aspot_bats$station, 'A-')
summary_aspot_bats$station = str_remove(summary_aspot_bats$station, 'B-')

# Make proper date columns
summary$DATE = as.Date(summary$DATE, format = '%Y-%b-%d')
summary_detections$DATE = as.Date(summary_detections$DATE, 
                                  format = '%Y-%m-%d')
summary_aspot_bats$DATE = as.Date(summary_aspot_bats$DATE)

# Remove on-shore detections
unique_stations = summary_detections$station |> unique() |> 
  sort(decreasing = TRUE)
unique_stations = unique_stations[unique_stations != 'B01']
trans_stations = seq_along(unique_stations)
names(trans_stations) = unique_stations
meta_HRIII = read.csv(path_meta_HRIII)
meta_HRIII$Deployment.service.date = meta_HRIII$Deployment.service.date |> 
  as.character() |>
  as.Date(format = '%m/%d/%Y')
meta_HRIII$Recovery.date = meta_HRIII$Recovery.date |> 
  as.character() |>
  as.Date(format = '%m/%d/%Y')
for(st in unique_stations){
  sub_meta = meta_HRIII[meta_HRIII$WT.ID == st,]
  dates_station = c(summary$DATE, summary_detections$DATE)
  keep_dates = c()
  for(i in seq_len(nrow(sub_meta))){
    start = sub_meta$Deployment.service.date[i]
    end = sub_meta$Recovery.date[i] 
    keep_dates = c(keep_dates, 
                   dates_station[dates_station > start &
                                   dates_station < (end - 2)] |> 
                     as.character())
  } 
  remove_dates = dates_station[!dates_station %in% keep_dates]
  summary = summary[!(summary$station == st & summary$DATE %in% remove_dates),]
  summary_detections = 
    summary_detections[!(summary_detections$station == st & 
                           summary_detections$DATE %in% remove_dates),]
  summary_aspot_bats = 
    summary_aspot_bats[!(summary_aspot_bats$station == st & 
                           summary_aspot_bats$DATE %in% remove_dates),]
}

# Plot
## create colour gradient
colfunc = colorRampPalette(c('#EAEDED', '#5F6A6A'))
cols = colfunc(max(summary$n))
png(sprintf('%s/activity_overview_HRIII.png', path_png),
    width = 40, height = 9.5, units = 'in', res = 800)
par(mar = c(5, 8, 1, 1), xaxs = 'i', yaxs = 'i')
## subset per season and adjust xlims
if(TRUE){
  sub = summary[which(as.Date(summary$DATE, format = '%Y-%b-%d') <
                        as.Date('2024-04-10')),]
  sub_detections = 
    summary_detections[which(as.Date(summary_detections$DATE, 
                                     format = '%Y-%m-%d') <
                               as.Date('2024-04-10')),]
  # sub_aspot = 
  #   summary_aspot[which(as.Date(summary_aspot$DATE) <
  #                         as.Date('2024-04-10')),]
  sub_aspot_bats = 
    summary_aspot_bats[which(as.Date(summary_aspot_bats$DATE) <
                               as.Date('2024-04-10')),]
  xlim = as.Date(c('2023-04-10', '2024-04-10'))
} else {
  sub = summary[which(as.Date(summary$DATE, format = '%Y-%b-%d') >= 
                        as.Date('2023-07-15')),]
  sub_detections = 
    summary_detections[which(as.Date(summary_detections$DATE, 
                                     format = '%Y-%m-%d') >= 
                               as.Date('2023-07-15')),]
  # sub_aspot = 
  #   summary_aspot[which(as.Date(summary_aspot$DATE) >= 
  #                         as.Date('2023-07-15')),]
  sub_aspot_bats = 
    summary_aspot_bats[which(as.Date(summary_aspot_bats$DATE) >= 
                               as.Date('2023-07-15')),]
  xlim = as.Date(c('2023-07-30', '2023-11-15'))
}
## create empty plot
ymin = min(trans_stations) - 0.45
ymax = max(trans_stations) + 0.45
plot(as.Date(sub$DATE, format = '%Y-%b-%d'),
     trans_stations[sub$station],
     ylim = c(ymin, ymax),
     xlim = xlim,
     xaxt = 'n', yaxt = 'n', type = 'n',
     xlab = '', ylab = '')
mtext('Date', 1, 3.5, cex = 2.5)
mtext('Station', 2, 5.5, cex = 2.5)
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
for(i in seq_len(nrow(sub))){
  points_custom(as.Date(sub$DATE[i], format = '%Y-%b-%d'),
                trans_stations[sub$station[i]], 
                my_shape1, col = cols[sub$n[i]])
}
## add scaled dots for number detections
points(as.Date(sub_detections$DATE, format = '%Y-%m-%d'),
       trans_stations[sub_detections$station] + 0.15, pch = 16, 
       cex = log10(sub_detections$n)/4 + 0.1)
## add scaled dots for number detections from aspot
# points(as.Date(sub_aspot$DATE),
#        trans_stations[sub_aspot$station] - 0.15, pch = 16, 
#        cex = log10(sub_aspot$n)/4 + 0.1)
points(as.Date(sub_aspot_bats$DATE),
       trans_stations[sub_aspot_bats$station] - 0.15, pch = 16, 
       cex = 1.1, col = '#D68910')
## add axes
unique_months = unique(format(ymd(sub$DATE), '%Y-%m'))
axis(1, at = as.Date(paste0(unique_months, '-01')), 
     labels = paste0(unique_months, '-01'), las = 1,
     cex.axis = 2)
# axis(1, at = as.Date(paste0(unique_months, '-10')), 
#      labels = paste0(unique_months, '-10'), las = 0)
# axis(1, at = as.Date(paste0(unique_months, '-20')), 
#      labels = paste0(unique_months, '-20'), las = 0.5)
axis(2, trans_stations, names(trans_stations), las = 1, cex.axis = 2)
dev.off() # close png
