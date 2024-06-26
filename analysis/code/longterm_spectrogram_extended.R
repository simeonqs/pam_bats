# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Generates long-term spectrogram with higher resolution.  
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
path_wavs = '/media/au472091/Samsung_T5/long_term_noise'
path_spec_files = 'analysis/results/longterm_spectrograms/spec_files'
path_pdf = 
  'analysis/results/longterm_spectrograms/longterm_spectrogram_noise.pdf'

# List files
files = list.files(path_wavs, full.names = TRUE)
file_backup = files[1]

# Settings 
window_length = 512

# Function to load wave and create average spectrum 
create.spec = function(file, path_out){
  wave = readWave(file)
  duration = wave@left |> length()
  start = sample(duration - window_length, 1)
  wave = wave[start:(start+window_length*3)]
  # wave = ffilter(wave, wl = window_length,
  #                from = 5000, to = 100000, output = 'Wave')
  wave = wave[window_length:(window_length+window_length)]
  spectrum = log(spec(wave, plot = FALSE, norm = FALSE)[,2])
  save(spectrum, file = sprintf('%s/%s.RData',
                                path_spec_files,
                                str_remove(basename(file), '.wav')))
}

# Get dates and keep one file per hour
dts = files |> str_extract('\\d{8}_\\d{6}') |> 
  as.POSIXct(format = '%Y%m%d_%H%M%S') |> round('mins') 
keep = !duplicated(dts)
files = files[keep]
dts = dts[keep]

# Run for all that have not yet been done
files_done = list.files(path_spec_files, full.names = TRUE)
files = files[!str_remove(basename(files), '.wav') %in%
                str_remove(basename(files_done), '.RData')]
files |> lapply(create.spec, path_out = path_spec_files)

# Load results
files_results = list.files(path_spec_files, full.names = TRUE)
spectra = vapply(files_results, function(f){
  load(f) 
  return(spectrum)
}, numeric(256))

# Add empty spectra for missing minutes
new_spectra = spectra[,1]
new_dts = dts[1]
pad = min(spectra)
for(i in seq_along(dts)[-1]){
  
  if(dts[i] - dts[i-1] == 1){
    
    new_spectra = cbind(new_spectra, spectra[,i])
    new_dts = c(new_dts, dts[i])
    
  } else {
    
    while(dts[i] - new_dts[length(new_dts)] != 1){
      new_spectra = cbind(new_spectra, rep(pad, nrow(spectra)))
      new_dts = c(new_dts, new_dts[length(new_dts)] + 60)
    }
    
    new_spectra = cbind(new_spectra, spectra[,i])
    new_dts = c(new_dts, dts[i])
    
  } # end else loop
  
} # end i loop

# Create long-term spectrogram
pdf(path_pdf, 12, 3)
wave = readWave(file_backup[1])
s = spec(wave[1:(1+window_length)], plot = FALSE)
imagep(t(new_spectra[1:180,]), drawPalette = FALSE, axes = FALSE, 
       decimate = FALSE, mar = c(4, 4, 1, 3))
dates = as.Date(new_dts)
index = seq(1, length(new_dts), length.out = 4)
axis(1, index, dates[index])
index = seq(1, 180, 50)
axis(2, index, round(s[index,1]))
mtext('frequency [kHz]', 2, 2)
dev.off()

