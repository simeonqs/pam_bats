# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Plots spectrogram of a sequence.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'seewave', 'tuneR', 'callsync', 'oce')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_audio = paste0('/home/au472091/Documents/results_aspot/defenitely_bats',
                    '/NS25_B_Fall2023/NS25_20230831_215125.wav')
path_pdf = 'analysis/results/examples_sequence.pdf'

# Plot spectrogram
wave = readWave(path_audio, from = 4.5, to = 5.5, units = 'seconds')
pdf(path_pdf, 5, 3)
better.spectro(wave, ovl = 450, ylim = c(0, 60000),
               col = oceColorsViridis())
dev.off()
