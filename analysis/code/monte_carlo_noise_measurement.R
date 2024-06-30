# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Calculates noise in frequency range.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'lubridate', 'callsync', 'tuneR',
              'parallel', 'seewave')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_audio = '/media/au472091/Samsung_T5/long_term_noise'
path_results = 'analysis/results/monte_carlo/noise_measurements'
path_pdf = 'analysis/results/monte_carlo/noise_measurements.pdf'

# Function to calculate RMS over frequency range
calc.noise.range = function(file, wl = 512, ovl = wl/2, 
                          freq_range = c(15000, 100000),
                          path_out = NULL){
  
  # Read wave
  wave = readWave(file)
  
  # Get duration and run tests
  if(ovl >= wl) stop('ovl must be smaller than wl.')
  if(round(ovl) - ovl != 0) warning('ovl was not integer, has been rounded.')
  duration = length(wave@left)
  if(duration < wl) stop('wave too short, pick wl < ', duration, '.')
  
  # Get start times
  start_times = seq(1, duration, ovl)
  start_times = start_times[start_times < duration - wl]
  if(length(start_times) == 0) stop('no start times.')
  
  # Get noise for each start time
  noise_vector = vapply(start_times, function(st){
    s = spec(wave[st:(st+wl)], plot = FALSE, norm = FALSE)
    s[s[,1] > freq_range[1]/1000 & s[,1] < freq_range[2]/1000,2]^2 |> sum()
  }, numeric(1))
  
  # Return result
  noise = mean(noise_vector)
  if(is.null(path_out)) return(noise) else 
    save(noise, file = sprintf('%s/%s.RData',
                              path_out,
                              file |> 
                                basename() |> 
                                str_remove('.wav') |> 
                                str_remove('.WAV')))
  
} # end calc.noise.range

# List files and run for all that have not yet been done
files = list.files(path_audio, '*wav', full.names = TRUE)
files_done = list.files(path_results, full.names = TRUE)
files = files[!str_remove(basename(files), '.wav') %in%
                str_remove(basename(files_done), '.RData')]
. = mclapply(files, calc.noise.range, path_out = path_results, mc.cores = 5)

# Load results
files_results = list.files(path_results, full.names = TRUE)
noise_measurements = vapply(files_results, function(f){
  load(f) 
  return(noise)
}, numeric(1))

# Extract other info
dts = files_results |> str_extract('\\d{8}_\\d{6}') |> 
  as.POSIXct(format = '%Y%m%d_%H%M%S') |> round('mins') 

# Plot
pdf(path_pdf, 10, 4)
plot(dts, noise_measurements, type = 'h', ylim = c(0, 5e12),
     xlab = 'Date', ylab = 'Summed amplitude')
dev.off()


