# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Plots 18 example spectrograms per species from the Aspot
# trainings data. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'seewave', 'tuneR', 'callsync')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Settings
data_set = 16
set.seed(1)

# Paths 
path_clips = sprintf('aspot/data_sets_s/data_%s', data_set)
path_pdfs = 'analysis/results/examples_species'

# List clips
clips = list.files(path_clips, '*.wav', full.names = TRUE)

# Plot 18 examples per species
species = clips |> basename() |> strsplit('-') |> sapply(`[`, 1)
for(sp in unique(species)){
  sub = sample(clips[species == sp], 18)
  pdf(sprintf('%s/%s.pdf', path_pdfs, sp), 12, 7)
  par(mfrow = c(3, 6))
  for(cl in sub) better.spectro(readWave(cl),
                                main = cl |> basename() |> strsplit('_') |>
                                  sapply(`[`, 2),
                                xlim = c(0, 0.05),
                                ylim = c(0, 100000))
  dev.off()
}
  
