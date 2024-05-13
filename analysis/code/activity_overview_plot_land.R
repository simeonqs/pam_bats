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
files_summaries = files_summaries[!str_detect(files_summaries, 'NS') & 
                                    !str_detect(files_summaries, 'HR') &
                                    !str_detect(files_summaries, 'TWJ') &
                                    !str_detect(files_summaries, 'BOARD')]
files_detections = list.files(path_detections, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)
files_detections = files_detections[!str_detect(files_detections, 'NS') & 
                                      !str_detect(files_detections, 'HR') &
                                      !str_detect(files_detections, 'TWJ') &
                                      !str_detect(files_detections, 'BOARD')]
files_aspot = list.files(path_aspot, pattern = '*.csv', 
                         recursive = TRUE, full.names = TRUE)
files_aspot = files_aspot[!str_detect(files_aspot, 'NS') & 
                            !str_detect(files_aspot, 'HR') &
                            !str_detect(files_aspot, 'TWJ') &
                            !str_detect(files_aspot, 'togter')]
files_aspot_bats = list.files(path_aspot_bats, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)
files_aspot_bats = files_aspot_bats[!str_detect(files_aspot_bats, 'NS') & 
                                      !str_detect(files_aspot_bats, 'HR') &
                                      !str_detect(files_aspot_bats, 'TWJ') &
                                      !str_detect(files_aspot_bats, 'togter')]
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

summary_aspot$station = toupper(summary_aspot$station)
summary_aspot$station = ifelse(summary_aspot$station == 'BLAAVAND', 'BLÅVAND', 
                               summary_aspot$station) 
summary_aspot$station = ifelse(summary_aspot$station == 'FANOE', 'FANØ', 
                               summary_aspot$station) 
summary_aspot$station = ifelse(summary_aspot$station == 'MANDO', 'MANDØ', 
                               summary_aspot$station) 
summary_aspot$station = ifelse(summary_aspot$station == 'ROEMOE', 'RØMØ', 
                               summary_aspot$station) 
summary_aspot$station = ifelse(summary_aspot$station == 'STADILOE', 'STADILØ', 
                               summary_aspot$station) 

summary_aspot_bats$station = toupper(summary_aspot_bats$station)
summary_aspot_bats$station = 
  ifelse(summary_aspot_bats$station == 'BLAAVAND', 'BLÅVAND', 
         summary_aspot_bats$station) 
summary_aspot_bats$station = 
  ifelse(summary_aspot_bats$station == 'FANOE', 'FANØ', 
         summary_aspot_bats$station) 
summary_aspot_bats$station = 
  ifelse(summary_aspot_bats$station == 'MANDO', 'MANDØ', 
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
trans_stations = seq_along(unique_stations)
names(trans_stations) = unique_stations
## create colour gradient
colfunc = colorRampPalette(c('#FAD7A0', '#0B5345'))
cols = colfunc(max(summary$n))
for(season in c('Forår 2023', 'Efterår 2023')){
  pdf(sprintf('%s/%s_land.pdf', path_png, str_replace(season, ' ', '_')),
      width = 15, height = 12) # , units = 'in', res = 1000
  # season = 'Forår 2023'
  par(mar = c(5, 12, 3, 1))
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
    sub_aspot_bats = 
      summary_aspot_bats[which(as.Date(summary_aspot_bats$DATE) <= 
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
    sub_aspot_bats = 
      summary_aspot_bats[which(as.Date(summary_aspot_bats$DATE) > 
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
  mtext('Station', 2, 10)
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
  points(as.Date(sub_aspot_bats$DATE),
         trans_stations[sub_aspot_bats$station] - 0.15, pch = 16, 
         cex = log10(sub_aspot_bats$n)/4 + 0.1, col = '#28B463')
  ## add axes
  unique_months = unique(format(ymd(sub$DATE), '%Y-%m'))
  axis(1, at = as.Date(paste0(unique_months, '-01')), 
       labels = paste0(unique_months, '-01') |> str_sub(6, 10))
  axis(1, at = as.Date(paste0(unique_months, '-10')), 
       labels = paste0(unique_months, '-10') |> str_sub(6, 10))
  axis(1, at = as.Date(paste0(unique_months, '-20')), 
       labels = paste0(unique_months, '-20') |> str_sub(6, 10))
  axis(2, trans_stations, names(trans_stations), las = 1)
  dev.off() # close PNG
}


