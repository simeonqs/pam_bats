# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Plot spectrograms for all detections from folder. Also stores
# wavs clips in same folder.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('callsync', 'stringr')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Settings
bandpass = c(1000, 95000)
resample_rate = 192000

# Paths 
path_detections = 'aspot/models/m38/selection_tables_1000'
path_audio = '/home/au472091/Documents/large_data/NS26_1000_files'
path_pdf = 'aspot/models/m38/specs_detections'

# Load selection tables
detections = load.selection.tables(path_detections)

# Function to plot specs
plot.spec = function(detection_row, path_audio = NULL, path_pdf = NULL){
  
  if(!is.null(path_pdf)) pdf(sprintf('%s/%s_%s.pdf', 
                                     path_pdf, 
                                     detection_row$file,
                                     detection_row$Selection))
  wave = readWave(sprintf('%s/%s.wav', path_audio, detection_row$file), 
                  from = detection_row$Begin.time..s. - 0.01, 
                  to = detection_row$End.time..s. + 0.01,
                  units = 'seconds')
  better.spectro(wave, xlim = c(0, 0.05))
  abline(v = c(0.01, 
               detection_row$End.time..s. - 
                 detection_row$Begin.time..s. + 0.01),
         lty = 2, lwd = 2)
  if(!is.null(path_pdf)) dev.off()
  writeWave(wave, sprintf(sprintf('%s/%s_%s.wav', 
                                  path_pdf, 
                                  detection_row$file,
                                  detection_row$Selection)))
}

# Run function on all detections
lapply(seq_len(nrow(detections)), function(i) 
  plot.spec(detections[i,], path_audio, path_pdf))
