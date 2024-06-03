# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Plots overview of active periods and detections in pdf. 
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
path_aspot = 'analysis/results/activity_overview/summaries_backup/aspot'
path_aspot_bats = 
  'analysis/results/activity_overview/summaries_backup/aspot_bats'
path_png = 'analysis/results/activity_overview/activity_overview'

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
files_summaries = files_summaries[str_detect(files_summaries, 'NS') | 
                                    str_detect(files_summaries, 'HR3-4')]
files_detections = list.files(path_detections, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)
files_detections = files_detections[str_detect(files_detections, 'NS') | 
                                      str_detect(files_detections, 'HR3-4')]
files_aspot = list.files(path_aspot, pattern = '*.csv', 
                         recursive = TRUE, full.names = TRUE)
files_aspot = files_aspot[str_detect(files_aspot, 'NS') | 
                            str_detect(files_aspot, 'HR3')]
files_aspot_bats = list.files(path_aspot_bats, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)
files_aspot_bats = files_aspot_bats[str_detect(files_aspot_bats, 'NS') | 
                                      str_detect(files_aspot_bats, 'HR3')]

# Read all files
summary = files_summaries |>
  lapply(read.csv) |> bind_rows()
summary_detections = files_detections |>
  lapply(read.csv) |> bind_rows()
summary_aspot = files_aspot |>
  lapply(read.csv) |> bind_rows()
summary_aspot_bats = files_aspot_bats |>
  lapply(read.csv) |> bind_rows()

# Fix station names
summary_aspot$station = 
  ifelse(summary_aspot$station == 'HR3_4', 'HR3-4S-C',
         ifelse(
           summary_aspot$station == 'T3-NS26', 'T3-NS26-C',
           ifelse(
             summary_aspot$station == 'NS6', 'NS6-C',
             ifelse(
               summary_aspot$station == 'NS24', 'NS24S',
               ifelse(
                 summary_aspot$station == 'NS32', 'NS32S',
                 ifelse(
                   summary_aspot$station == 'NS28', 'NS28S',
                   ifelse(
                     summary_aspot$station == 'NS6', 'NS6C',
                     summary_aspot$station)))))))
summary_aspot_bats$station = 
  ifelse(
    summary_aspot_bats$station == 'HR3_4', 'HR3-4S-C',
    ifelse(
      summary_aspot_bats$station == 'T3-NS26', 'T3-NS26-C',
      ifelse(
        summary_aspot_bats$station == 'NS6', 'NS6-C',
        ifelse(
          summary_aspot_bats$station == 'NS24', 'NS24S',
          ifelse(
            summary_aspot_bats$station == 'NS6C', 'NS6-C',
            summary_aspot_bats$station)))))
summary$station = 
  ifelse(summary$station == 'T3-NS26C', 'T3-NS26-C',
         ifelse(summary$station == 'NS6', 'NS6-C',
                ifelse(summary$station == 'NS19-LOT1', 'NS19',
                       ifelse(summary$station == 'NS28', 'NS28S',
                              ifelse(summary$station == 'NS24', 'NS24S',
                                     ifelse(summary$station == 'NS32', 'NS32S',
                                            summary$station))))))
summary_detections$station = 
  ifelse(summary_detections$station == 'T3-NS26C', 'T3-NS26-C',
         ifelse(
           summary_detections$station == 'NS6', 'NS6-C',
           ifelse(
             summary_detections$station == 'NS19-LOT1', 'NS19',
             ifelse(
               summary_detections$station == 'NS24', 'NS24S',
               ifelse(
                 summary_detections$station == 'NS28', 'NS28S',
                 ifelse(
                   summary_detections$station == 'NS32', 'NS32S',
                   summary_detections$station))))))

# Plot
unique_stations = summary_detections$station |> unique() |> 
  sort(decreasing = TRUE)
trans_stations = seq_along(unique_stations)
names(trans_stations) = unique_stations
## create colour gradient
colfunc = colorRampPalette(c('#FAD7A0', '#0B5345'))
cols = colfunc(max(summary$n))
pdf(sprintf('%s_bøjer.pdf', path_png),
    width = 40, height = 12) # , units = 'in', res = 1000
par(mar = c(5, 7, 1, 1))
## subset per season and adjust xlims
if(TRUE){
  sub = summary[which(as.Date(summary$DATE, format = '%Y-%b-%d') < 
                        as.Date('2024-04-10')),]
  sub_detections = 
    summary_detections[which(as.Date(summary_detections$date, 
                                     format = '%Y-%m-%d') < 
                               as.Date('2024-04-10')),]
  sub_aspot = 
    summary_aspot[which(as.Date(summary_aspot$DATE) < 
                          as.Date('2024-04-10')),]
  sub_aspot_bats = 
    summary_aspot_bats[which(as.Date(summary_aspot_bats$DATE) < 
                               as.Date('2024-04-10')),]
  xlim = as.Date(c('2023-04-10', '2024-04-10'))
} else {
  sub = summary[which(as.Date(summary$DATE, format = '%Y-%b-%d') >= 
                        as.Date('2023-07-15')),]
  sub_detections = 
    summary_detections[which(as.Date(summary_detections$date, 
                                     format = '%Y-%m-%d') >= 
                               as.Date('2023-07-15')),]
  sub_aspot = 
    summary_aspot[which(as.Date(summary_aspot$DATE) >= 
                          as.Date('2023-07-15')),]
  sub_aspot_bats = 
    summary_aspot_bats[which(as.Date(summary_aspot_bats$DATE) >= 
                               as.Date('2023-07-15')),]
  xlim = as.Date(c('2023-07-30', '2023-11-15'))
}
## create empty plot
plot(as.Date(sub$DATE, format = '%Y-%b-%d'),
     trans_stations[sub$station],
     ylim = c(min(trans_stations) - 0.5, max(trans_stations) + 0.5),
     xlim = xlim,
     xaxt = 'n', yaxt = 'n', type = 'n',
     xlab = 'Dato', ylab = '')
mtext('Station', 2, 5)
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
# points(as.Date(sub_aspot$DATE),
#        trans_stations[sub_aspot$station] - 0.15, pch = 16, 
#        cex = log10(sub_aspot$n)/4 + 0.1)
points(as.Date(sub_aspot_bats$DATE),
       trans_stations[sub_aspot_bats$station] - 0.15, pch = 16, 
       cex = log10(sub_aspot_bats$n)/4 + 0.1, col = '#28B463')
## add axes
unique_months = unique(format(ymd(sub$DATE), '%Y-%m'))
axis(1, at = as.Date(paste0(unique_months, '-01')), 
     labels = paste0(unique_months, '-01'))
axis(2, trans_stations, names(trans_stations), las = 1)
dev.off() # close PNG


