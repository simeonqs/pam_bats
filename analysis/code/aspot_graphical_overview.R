# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Takes Raven selection tables and creates graphical overviews
# of the detections.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'callsync')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_detections = 'aspot/m15/selection_tables'

# Load selection tables
files = list.files(path_detections, full.names = TRUE)
detections = lapply(files, load.selection.table)

# Get info
n_detects = vapply(detections, nrow, numeric(1))
splits = lapply(basename(files), str_split, '_')
date_times = vapply(splits, function(split){
  date = split[[1]][2] |> as.Date(format = '%Y%m%d') 
  time =  split[[1]][3] |> strptime(format = '%H%M%S') 
  date_time = as.POSIXct(paste(date, format(time, '%H:%M:%S')))
  return(as.character(date_time))
}, character(1)) |> as.POSIXct()

# Plot number detections
plot(date_times, n_detects, xlab = 'date', ylab = '# calls per file', 
     type = 'h', col = '#1ABC9C', yaxs = 'i')
