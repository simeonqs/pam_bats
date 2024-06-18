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

# Paths 
path_summaries = 'analysis/results/activity_overview/summaries/summaries'
path_detections = 'analysis/results/activity_overview/summaries/detections'
# path_aspot = 'analysis/results/activity_overview/summaries/aspot'
path_aspot_bats = 'analysis/results/activity_overview/summaries/aspot_bats'
path_png = 'analysis/results/activity_overview/activity_overview_'

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
files_summaries = files_summaries[!str_detect(files_summaries, 'NS') & 
                                    !str_detect(files_summaries, 'HR') &
                                    # !str_detect(files_summaries, 'TWJ') &
                                    !str_detect(files_summaries, 'BOARD')]
files_detections = list.files(path_detections, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)
files_detections = files_detections[!str_detect(files_detections, 'NS') & 
                                      !str_detect(files_detections, 'HR') &
                                      # !str_detect(files_detections, 'TWJ') &
                                      !str_detect(files_detections, 'BOARD')]
# files_aspot = list.files(path_aspot, pattern = '*.csv', 
#                          recursive = TRUE, full.names = TRUE)
# files_aspot = files_aspot[!str_detect(files_aspot, 'NS') & 
#                             !str_detect(files_aspot, 'HR') &
#                             # !str_detect(files_aspot, 'TWJ') &
#                             !str_detect(files_aspot, 'togter')]
files_aspot_bats = list.files(path_aspot_bats, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)
files_aspot_bats = files_aspot_bats[!str_detect(files_aspot_bats, 'NS') & 
                                      !str_detect(files_aspot_bats, 'HR') &
                                      # !str_detect(files_aspot_bats, 'TWJ') &
                                      !str_detect(files_aspot_bats, 'togter')]
# Read all files
summary = files_summaries |>
  lapply(read.csv) |> bind_rows()
summary_detections = files_detections |>
  lapply(read.csv) |> bind_rows()
# summary_aspot = files_aspot |>
#   lapply(read.csv) |> bind_rows()
summary_aspot_bats = files_aspot_bats |>
  lapply(read.csv) |> bind_rows()

# Fix station names
summary$station = ifelse(summary$station == 'KAMMER', 'KAMMERSLUSEN', 
                         summary$station) 
summary$station = ifelse(summary$station == 'LAND-BALLUM', 'BALLUM', 
                         summary$station) 
summary$station = ifelse(summary$station == 'LAND-MANDØ', 'MANDØ', 
                         summary$station) 
summary$station = ifelse(summary$station == 'LAND1', 'KAMMERSLUSEN', 
                         summary$station) 
summary$station = ifelse(summary$station == 'LAND10-LOT1', 'REJSBY', 
                         summary$station) 
summary$station = ifelse(summary$station == 'LAND2', 'BLÅVAND', 
                         summary$station) 
summary$station = ifelse(summary$station == 'LAND3', 'SKJERN', 
                         summary$station) 
summary$station = ifelse(summary$station == 'LAND4', 'STADILØ', 
                         summary$station) 
summary$station = ifelse(summary$station == 'LAND5', 'HUSBY', 
                         summary$station) 
summary$station = ifelse(summary$station == 'LAND6-LOT1', 'BALLUM', 
                         summary$station) 
summary$station = ifelse(summary$station == 'LAND7-LOT1', 'MANDØ', 
                         summary$station) 
summary$station = ifelse(summary$station == 'LAND8-LOT1', 'NYMINDE', 
                         summary$station) 
summary$station = ifelse(summary$station == 'LAND9-LOT1', 'FANØ', 
                         summary$station) 
summary$station = ifelse(summary$station == 'MANDO', 'MANDØ', 
                         summary$station) 
summary$station = ifelse(summary$station == 'NYMND-PLTG', 'NYMINDE', 
                         summary$station) 
summary$station = ifelse(summary$station == 'ROEMOE', 'RØMØ', 
                         summary$station) 
summary$station = ifelse(summary$station == 'STADILOE', 'STADILØ', 
                         summary$station) 
summary$station = ifelse(summary$station == 'BLAAVAND', 'BLÅVAND', 
                         summary$station) 
summary$station = ifelse(summary$station == 'FANO', 'FANØ', 
                         summary$station) 
summary$station = ifelse(summary$station == 'TWJ-08', 'MANDØ', 
                         summary$station) 

summary_detections$station = 
  ifelse(summary_detections$station == 'KAMMER', 'KAMMERSLUSEN', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'LAND-BALLUM', 'BALLUM', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'LAND-MANDØ', 'MANDØ', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'LAND1', 'KAMMERSLUSEN', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'LAND10-LOT1', 'REJSBY', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'LAND2', 'BLÅVAND', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'LAND3', 'SKJERN', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'LAND4', 'STADILØ', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'LAND5', 'HUSBY', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'LAND6-LOT1', 'BALLUM', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'LAND7-LOT1', 'MANDØ', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'LAND8-LOT1', 'NYMINDE', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'LAND9-LOT1', 'FANØ', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'MANDO', 'MANDØ', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'NYMND-PLTG', 'NYMINDE', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'ROEMOE', 'RØMØ', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'STADILOE', 'STADILØ', 
         summary_detections$station) 
summary_detections$station =
  ifelse(summary_detections$station == 'BLAAVAND', 'BLÅVAND', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'FANO', 'FANØ', 
         summary_detections$station) 
summary_detections$station = 
  ifelse(summary_detections$station == 'TWJ-08', 'MANDØ', 
         summary_detections$station) 

# summary_aspot$station = summary_aspot$station |>
#   toupper() 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'KAMMER', 'KAMMERSLUSEN', 
#          summary_aspot$station) 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'LAND-BALLUM', 'BALLUM', 
#          summary_aspot$station) 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'LAND-MANDØ', 'MANDØ', 
#          summary_aspot$station) 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'LAND1', 'KAMMERSLUSEN', 
#          summary_aspot$station) 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'LAND10-LOT1', 'REJSBY', 
#          summary_aspot$station) 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'LAND2', 'BLÅVAND', 
#          summary_aspot$station) 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'LAND3', 'SKJERN', 
#          summary_aspot$station) 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'LAND4', 'STADILØ', 
#          summary_aspot$station) 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'LAND5', 'HUSBY', 
#          summary_aspot$station) 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'LAND6-LOT1', 'BALLUM', 
#          summary_aspot$station) 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'LAND7-LOT1', 'MANDØ', 
#          summary_aspot$station) 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'LAND8-LOT1', 'NYMINDE', 
#          summary_aspot$station) 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'LAND9-LOT1', 'FANØ', 
#          summary_aspot$station) 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'MANDO', 'MANDØ', 
#          summary_aspot$station) 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'NYMND-PLTG', 'NYMINDE', 
#          summary_aspot$station) 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'ROEMOE', 'RØMØ', 
#          summary_aspot$station) 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'STADILOE', 'STADILØ', 
#          summary_aspot$station) 
# summary_aspot$station =
#   ifelse(summary_aspot$station == 'BLAAVAND', 'BLÅVAND', 
#          summary_aspot$station) 
# summary_aspot$station = 
#   ifelse(summary_aspot$station == 'FANO', 'FANØ', 
#          summary_aspot$station) 

summary_aspot_bats$station = summary_aspot_bats$station |>
  toupper() |> 
  str_remove('LAND-')
summary_aspot_bats$station = 
  ifelse(summary_aspot_bats$station == 'BLAAVAND', 'BLÅVAND', 
         summary_aspot_bats$station) 
summary_aspot_bats$station = 
  ifelse(summary_aspot_bats$station == 'FANOE', 'FANØ', 
         summary_aspot_bats$station) 
summary_aspot_bats$station = 
  ifelse(summary_aspot_bats$station == 'MANDOE', 'MANDØ', 
         summary_aspot_bats$station) 
summary_aspot_bats$station = 
  ifelse(summary_aspot_bats$station == 'ROEMOE', 'RØMØ', 
         summary_aspot_bats$station) 
summary_aspot_bats$station = 
  ifelse(summary_aspot_bats$station == 'STADILOE', 'STADILØ', 
         summary_aspot_bats$station) 

# Plot
unique_stations = summary_detections$station |> unique() |> 
  sort(decreasing = TRUE)
unique_stations = unique_stations[unique_stations != 'SKAGEN']
trans_stations = seq_along(unique_stations)
names(trans_stations) = unique_stations
## create colour gradient
colfunc = colorRampPalette(c('#EAEDED', '#5F6A6A'))
cols = colfunc(max(summary$n))
png(sprintf('%sland.png', path_png),
    width = 40, height = 11, units = 'in', res = 800)
par(mar = c(5, 18, 1, 1), xaxs = 'i', yaxs = 'i')
## subset per year and adjust xlims
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
                               as.Date('2023-12-31')),]
  # sub_aspot = 
  #   summary_aspot[which(as.Date(summary_aspot$DATE) >= 
  #                         as.Date('2023-12-31')),]
  sub_aspot_bats = 
    summary_aspot_bats[which(as.Date(summary_aspot_bats$DATE) >= 
                               as.Date('2023-12-31')),]
  xlim = as.Date(c('2024-01-01', '2024-12-31'))
}
## remove two points from Mandø
sub_detections = 
  sub_detections[!(sub_detections$station == 'MANDØ' & 
                     sub_detections$DATE < as.Date('2023-05-01')),]
sub_aspot_bats = 
  sub_detections[!(sub_aspot_bats$station == 'MANDØ' & 
                     sub_aspot_bats$DATE < as.Date('2023-05-01')),]
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
dev.off() # close PNG


