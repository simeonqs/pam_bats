# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Plotting spectrogram of buzz activity.   
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'lubridate', 'scales', 'callsync', 'tuneR',
              'seewave', 'oce')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
path_wav = paste0('/home/au472091/Documents/new_results_aspot/',
                  'defenitely_bats/HRIII/HR-A-A05_20230520_203051.wav')
path_pdf = 'analysis/results/example_buzz_activity_HRIII.pdf'

# Read wave
wave = readWave(path_wav)

# Create long-term spectrogram
pdf(path_pdf, 7, 4)
par(mfrow = c(2, 1), oma = c(2, 2, 0, 0))
for(i in c(5, 10)){
  better.spectro(wave[((i-5)*wave@samp.rate):(i*wave@samp.rate)],
                 mar = c(2, 2, 1, 1),
                 xlim = c(0, 5),
                 ylim = c(10000, 90000),
                 col = hcl.colors(20, palette = 'Tropic', rev = TRUE))
  mtext('Frequency [Hz]', 2, 2.5, cex = 1)
}
mtext('Time [s]', 1, 2.5, cex = 1)
dev.off()

