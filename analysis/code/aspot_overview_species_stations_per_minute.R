# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Plots overview of species and stations.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'callsync', 'lubridate')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
path_species_overview = 'analysis/results/species_overview'
path_trigger = 'analysis/results/activity_overview/summaries/detections'
path_png = 'analysis/results/species_overview/species_overview_per_minute'

# Load data
files = list.files(path_species_overview, '*csv', full.names = TRUE)
files_trigger = list.files(path_trigger, pattern = '*.csv', 
                           recursive = TRUE, full.names = TRUE)

# Settings
ymax = 400
sp = 'Pnat'

# Load data
files = list.files(path_species_overview, '*csv', full.names = TRUE)

# Remove Skagen
files = files[!str_detect(files, 'Skagen')]

# Colours species 
colours = c(
  '#1f77b4', # A soft blue
  '#2ca02c', # A strong green
  '#9467bd', # A moderate purple
  '#F7DC6F', # A soft yellow
  '#E67E22', # A pastel orange
  '#B03A2E'  # A dark red
)
species = c('M', 'NVE', 'Paur', 'Pnat', 'Ppip', 'Ppyg')
names(colours) = species

# Open png
png(sprintf('%s_%s.png', path_png, sp |> str_replace('NVE', 'ENV')), 
    12, 8, units = 'in', res = 800)
layout(matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3,
                4, 4, 4, 5, 5, 5, 6, 6, 6,
                7, 7, 7, 8, 8, 8, 9, 9, 9,
                10, 10, 10, 11, 11, 11, 12, 12, 12),
              byrow = TRUE, nrow = 4, ncol = 9))
par(mar = rep(0.5, 4), oma = c(3.5, 3.5, 0.5, 0.5))

# Run through stations
for(file in files){
  
  # Make empty plot
  plot(NULL, 
       xlim = as.Date(c('2023-04-10', '2024-04-10')), 
       ylim = c(0.3, ymax),
       xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
  
  # Load data
  dat = read.csv(file)
  dat = dat[which(dat$species == sp),]
  dat = dat[which(dat$n_detections > 5),]
  station = file |> basename() |> str_remove('.csv')
  
  # Fix dates and times
  split = dat$file |> strsplit('_')
  dates = sapply(split, function(x) x[length(x)-1])
  times = sapply(split, function(x) x[length(x)])
  rounded_times = format(strptime(times, '%H%M%S'), '%H:%M')
  dates_times = paste(dates, times, sep = '_')
  new_dates_times = ymd_hms(dates_times) - hours(12)
  new_dates = new_dates_times |>
    as.Date(format = '%Y:%m:%d')
  
  # Get summary data
  file_trigger_station = 
    files_trigger[str_detect(files_trigger |> 
                               str_replace('Ø', 'OO') |>
                               str_replace('Å', 'AA') |> 
                               str_replace('LAND1_', 'KAMMER') |> 
                               str_replace('LAND2', 'BLAA') |> 
                               str_replace('LAND3', 'SKJERN') |> 
                               str_replace('LAND4', 'STAD') |> 
                               str_replace('LAND5', 'HUSB') |> 
                               str_replace('LAND6', 'BALLUM') |> 
                               str_replace('LAND7', 'MAND') |> 
                               str_replace('LAND8', 'NYMI') |> 
                               str_replace('NYMN', 'NYMI') |> 
                               str_replace('LAND10', 'REJS') |> 
                               str_replace('LAND9', 'FANOE') |> 
                               str_replace('TWJ', 'MANDOE'), 
                             station |> str_sub(1, 4) |> toupper())]
  summary = file_trigger_station |>
    lapply(read.csv) |> bind_rows()
  summary = summary[as.Date(summary$DATE) > as.Date('2023-04-10') &
                      as.Date(summary$DATE) < as.Date('2024-04-10'),]
  
  # Mark missing dates
  all_dates = seq(from = as.Date('2023-04-10'),
                  to = as.Date('2024-04-10'),
                  by = 'day')
  missing_dates = all_dates[!all_dates %in% as.Date(summary$DATE)]
  for(d in missing_dates) 
    polygon(x = c(d-0.5, d+0.5),
            y = c(0, ymax),
            col = '#BDC3C7', border = '#BDC3C7')
  
  # Make summary per date
  all_dates = seq(as.Date('2023-04-10'), as.Date('2024-04-10'), 'day')
  n = vapply(all_dates, function(date){
    times_sub = rounded_times[which(new_dates == date)]
    return(length(unique(times_sub)))
  }, numeric(1))
  lines(all_dates, n, col = colours[which(species == sp)], lwd = 2)
  
  # Add info plot
  text(as.Date('2023-04-15'), 0.93*ymax, station, font = 2, adj = 0, cex = 1.5)
  if(station %in% c('Ballum', 'Husby', 'Nyminde', 'Skjern')){
    axis(2, seq(0, ymax, length.out = 5), cex.axis = 1.4)
    mtext('Activity minutes', 2, 2.5, cex = 1)
  }
  if(station %in% c('Roemoe', 'Skjern', 'Stadiloe')){
    axis.Date(side = 1, at = c(as.Date('2023-05-01'), 
                               as.Date('2023-08-01'), 
                               as.Date('2023-11-01'), 
                               as.Date('2024-02-01')),
              cex.axis = 1.4,
              labels = c('May', 'Aug', 'Nov', 'Feb'), format='%b')    
    mtext('Month', 1, 2.8, cex = 1)
  }
  
} # end file loop

# Close png
dev.off()

# Message
message('Printed all figures.')







