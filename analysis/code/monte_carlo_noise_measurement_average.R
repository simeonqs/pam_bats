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
path_results = 'analysis/results/monte_carlo/noise_measurements'
path_pdf = 'analysis/results/monte_carlo/noise_measurement_average_night.pdf'

# Load results
files_results = list.files(path_results, full.names = TRUE)
noise_measurements = vapply(files_results, function(f){
  load(f) 
  return(noise)
}, numeric(1))

# Extract other info
dts = files_results |> str_extract('\\d{8}_\\d{6}') |> 
  as.POSIXct(format = '%Y%m%d_%H%M%S') |> round('mins') 
times = files_results |> str_extract('\\d{8}_\\d{6}') |> str_sub(10, 16)
dates = as.Date(dts)

# Make night belong to one date
dates_times = paste(dates, times, sep = '_')
new_dates_times = ymd_hms(dates_times) - hours(12)
new_dates = new_dates_times |>
  as.Date(format = '%Y:%m:%d')
new_times_strings = new_dates_times |> as.character() |> strsplit(' ') |> 
  sapply(`[`, 2)
new_times = vapply(new_times_strings, function(time_str) {
  as.numeric(strsplit(time_str, ':')[[1]]) %*% c(60, 1, 1/60)
}, numeric(1))

# Plot
pdf(path_pdf, 5, 3)
par(mar = c(4, 4, 1, 1))
plot(NULL, 
     xlim = c(min(new_times), max(new_times)),
     ylim = c(8, 13),
     xaxt = 'n',
     xlab = 'Time [HH:MM]', ylab = 'Noise level')
axis(1, c(8*60, 12*60, 16*60), c('20:00', '24:00', '04:00'))
for(date in unique(new_dates)){
  sub_times = new_times[new_dates == date]
  sub_noise = noise_measurements[new_dates == date]
  lines(sort(sub_times), 
        log10(sub_noise[order(sub_times)]),
        col = '#AED6F1', lwd = 0.5)
}
ut = new_times |> unique() |> sort()
means = vapply(ut, function(time) 
  mean(noise_measurements[new_times == time]),
  numeric(1))
lines(ut, 
      log10(means),
      col = '#21618C', lwd = 2)
dev.off()