# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Makes two plots in one pdf with bat activity for one station.  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'lubridate', 'scales', 'callsync')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Settings
station = 'NS31'
ylim = c(-5, 150)
xlim = as.Date(c('2023-07-30', '2023-10-05'))

# Paths 
path_aspot = 'analysis/results/activity_overview/summaries/aspot'
path_aspot_bats = 'analysis/results/activity_overview/summaries/aspot_bats'
path_meta = 'analysis/data/meta_data_boejer.csv'
path_pdf = 'analysis/results/activity_overview/stations'
path_data_bats = '/home/au472091/Documents/results_aspot/defenitely_bats'

# List summary files
files_aspot_bats = list.files(path_aspot_bats, pattern = '*.csv', 
                              recursive = TRUE, full.names = TRUE)

# Read all files
summary_aspot_bats = files_aspot_bats |>
  lapply(read.csv) |> bind_rows()
meta = read.csv(path_meta)

# Fix station name
meta$Station.ID = str_remove(meta$Station.ID, 'T3/')

# Load all detections of bats for station
detections = load.selection.tables(path_data_bats, recursive = TRUE)
detections = detections[str_detect(detections$file, station),]

# Get time
detections$date = detections$file |> 
  str_extract('\\d{8}') |> 
  as.Date(format = '%Y%m%d')
detections$time = detections$file |> 
  strsplit('_') |> sapply(`[`, 3)

# Subset for station and plot
pdf(sprintf('%s/calls_%s.pdf', path_pdf, station), 5, 5)
## calls per day
par(mar = c(4, 4, 0.5, 0.5), mfrow = c(2, 1))
x = summary_aspot_bats$DATE[summary_aspot_bats$station == station] |> 
  as.Date()
y = summary_aspot_bats$n[summary_aspot_bats$station == station]
plot(sort(x), y[order(x)], 
     xlim = xlim, ylim = ylim, pch = 16, col = '#0B5345', cex = 2, 
     xaxt = 'n', yaxt = 'n', yaxs = 'i', 
     xlab = 'dato', ylab = '# kald per dag')
axis(2, seq(0, 150, 50))
unique_months = unique(format(ymd(x), '%Y-%m'))
axis(1, at = as.Date(paste0(unique_months, '-01')), 
     labels = paste0(unique_months, '-01') |> str_sub(6, 10))
axis(1, at = as.Date(paste0(unique_months, '-10')), 
     labels = paste0(unique_months, '-10') |> str_sub(6, 10))
axis(1, at = as.Date(paste0(unique_months, '-20')), 
     labels = paste0(unique_months, '-20') |> str_sub(6, 10))
## timing of calls
detections$time_fixed = as.POSIXct(detections$time, format = '%H%M%S')
plot(detections$date, detections$time_fixed, 
     xlab = 'dato', ylab = 'tid på dagen', xaxt = 'n', yaxt = 'n',
     xlim = xlim, ylim = as.POSIXct(c('000000', '240000'), format = '%H%M%S'))
unique_months = unique(format(ymd(detections$date), '%Y-%m'))
axis(1, at = as.Date(paste0(unique_months, '-01')), 
     labels = paste0(unique_months, '-01') |> str_sub(6, 10))
axis(1, at = as.Date(paste0(unique_months, '-10')), 
     labels = paste0(unique_months, '-10') |> str_sub(6, 10))
axis(1, at = as.Date(paste0(unique_months, '-20')), 
     labels = paste0(unique_months, '-20') |> str_sub(6, 10))
axis(2, at = as.POSIXct(c('000000', '060000', '120000', '180000', '240000'), 
                        format = '%H%M%S'), 
     labels = c('00:00', '06:00', '12:00', '18:00', '24:00'))
dev.off()





