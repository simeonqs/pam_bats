library(tuneR)
library(stringr)
files = list.files('/Users/ssmeele/ownCloud - ssmeele@ab.mpg.de@owncloud.gwdg.de/Simeon/MPI AB/Side projects/Puffin detector/grunt/validation data', 
                   pattern = 'wav*', recursive = T, full.names = T)
for(file in files){
  wave = readWave(file)
  if(wave@stereo) writeWave(mono(wave, 'right'), file, extensible = F)
  # writeWave(wave, file, extensible = F)
}

# for(file in files) file.rename(file, str_replace(file, 'HUMAN', 'noise-tonal'))
