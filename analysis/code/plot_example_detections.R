# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Plot spectrograms few seconds with ground truth and detections.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('callsync', 'stringr', 'seewave', 'tuneR', 'scales')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
folder = '~/Documents/large_data/defenitely_bats/NS33_A_Fall2023_Recovered'
path_detections = sprintf(
  '%s/NS33_20230827_233738_predict_output.log.annotation.result.txt', folder)
path_audio = sprintf('%s/NS33_20230827_233738.wav', folder)
path_ground_truth = paste0('aspot/test_data_sets/boeje_test_data/',
                           'txt/NS33_20230827_233738.Table.1.selections.txt')

path_detections = '/home/au472091/Documents/results_aspot/defenitely_bats/fall/T3-NS26_A_Fall2023_Recovered/T3-NS26C_20230906_213232_predict_output.log.annotation.result.txt'
path_audio = '/home/au472091/Documents/results_aspot/defenitely_bats/fall/T3-NS26_A_Fall2023_Recovered/T3-NS26C_20230906_213232.wav'

path_out = '~/Desktop/test.pdf'

# Settings
from = 8
to = 8.5

# Load audio
wave = readWave(path_audio, from = from, to = to, units = 'seconds')
# wave = ffilter(wave, from = 1000, output = 'Wave')

# Open PDF
pdf(path_out, 7, 4)

# Plot spectrogram
better.spectro(wave, ylim = c(10000, 75000))

# Plot detections
detections = load.selection.table(path_detections)
for(i in seq_len(nrow(detections))){
  graphics::rect(xleft = detections$Begin.time..s.[i] - from,
                 xright = detections$End.time..s.[i] - from,
                 ybottom = par('usr')[3], ytop = par('usr')[4],
                 border = NA, col = alpha('#3a586e', 0.3))
  abline(v = detections$Begin.time..s.[i] - from, lty = 2,
         col = '#3a586e', lwd = 3)
  abline(v = detections$End.time..s.[i] - from, lty = 2,
         col = '#3a586e', lwd = 3)
}

# Plot ground truth
# ground_truth = load.selection.table(path_ground_truth)
# for(i in seq_len(nrow(ground_truth))){
#   graphics::rect(xleft = ground_truth$Begin.Time..s.[i] - from,
#                  xright = ground_truth$End.Time..s.[i] - from,
#                  ybottom = par('usr')[3], ytop = par('usr')[4],
#                  border = NA, col = alpha('#28B463', 0.1))
#   abline(v = ground_truth$Begin.Time..s.[i] - from, lty = 2,
#          col = '#28B463', lwd = 3)
#   abline(v = ground_truth$End.Time..s.[i] - from, lty = 2,
#          col = '#28B463', lwd = 3)
# }

# Close PDF
dev.off()




