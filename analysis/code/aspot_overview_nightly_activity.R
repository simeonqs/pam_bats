# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Plots overview of activity throughout night.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'callsync', 'lubridate', 'scales')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
path_species_overview = 'analysis/results/species_overview'
path_sun = 'analysis/data/sunrise_sunset_west_coast_DK.csv'
path_pdf = 'analysis/results/nightly_activity.pdf'

# Load data
sun = read.csv(path_sun)
files = list.files(path_species_overview, '*csv', full.names = TRUE)

# Translate sun times
sun$rise_min = vapply(sun$Sunrise, function(time_str) {
  as.numeric(strsplit(time_str, ':')[[1]]) %*% c(60, 1, 1/60)
}, numeric(1)) - # making time to minutes
  ifelse((as.Date(sun$Date) > as.Date('2023-03-25') &
            as.Date(sun$Date) < as.Date('2023-10-29')) |
           (as.Date(sun$Date) > as.Date('2024-03-30') &
              as.Date(sun$Date) < as.Date('2024-10-27')),
         60, 0) + # removing summertime 
  12*60 - # fixing time so that the date runs from 12:00 to 12:00
  1*60 # making time to UTC
sun$set_min = vapply(sun$Sunset, function(time_str) {
  as.numeric(strsplit(time_str, ':')[[1]]) %*% c(60, 1, 1/60)
}, numeric(1)) - 
  ifelse((as.Date(sun$Date) > as.Date('2023-03-25') &
            as.Date(sun$Date) < as.Date('2023-10-29')) |
           (as.Date(sun$Date) > as.Date('2024-03-30') &
              as.Date(sun$Date) < as.Date('2024-10-27')),
         60, 0) - 
  12*60 - 
  1*60

# Colours stations 
colours = c(
  '#1f77b4', # A soft blue
  '#ff7f0e', # A vivid orange
  '#2ca02c', # A strong green
  '#d62728', # A deep red
  '#9467bd', # A moderate purple
  '#8c564b', # A muted brown
  '#e377c2', # A light pink
  '#7f7f7f', # A neutral grey
  '#bcbd22', # A bright olive green
  '#17becf', # A turquoise
  '#f7b6d2', # A pale pink
  '#c5b0d5'  # A light lavender
)
stations = files |> basename() |> str_remove('.csv')

# Open PDF
pdf(path_pdf, 8, 7)
layout(matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 13,
                4, 4, 4, 5, 5, 5, 6, 6, 6, 13,
                7, 7, 7, 8, 8, 8, 9, 9, 9, 14,
                10, 10, 10, 11, 11, 11, 12, 12, 12, 14),
              byrow = TRUE, nrow = 4, ncol = 10))
par(mar = rep(0.5, 4), oma = c(3.5, 3.5, 0.5, 0.5))

# Run through stations
for(file in files){
  
  # Make empty plot
  plot(NULL, 
       xlim = as.Date(c('2023-04-20', '2024-04-20')), 
       ylim = c(0, 1440),
       xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
  
  # Make shadow for night
  sun_sub = sun[sun$Date > as.Date('2023-04-20') &
                  sun$Date < as.Date('2024-04-20'),]
  polygon(x = c(as.Date(sun_sub$Date)+1, rev(as.Date(sun_sub$Date))),
          y = c(sun_sub$rise_min, rev(sun_sub$set_min)),
          col = '#D6DBDF', border = '#D6DBDF')
  
  # Load data
  dat = read.csv(file)
  dat = dat[which(dat$n_detections > 5),]
  dat = dat[!str_detect(dat$file, 'TWJ'),]
  station = file |> basename() |> str_remove('.csv')
  
  # Fix dates and times
  split = dat$file |> strsplit('_')
  dates = sapply(split, function(x) x[length(x)-1])
  times = sapply(split, function(x) x[length(x)])
  rounded_times = format(strptime(times, '%H%M%S'), '%H:%M')
  dates_times = paste(dates, times, sep = '_')
  new_dates_times = ymd_hms(dates_times) - hours(12)
  dat$new_dates = new_dates_times |>
    as.Date(format = '%Y:%m:%d')
  new_times_strings = new_dates_times |> as.character() |> strsplit(' ') |> 
    sapply(`[`, 2)
  dat$new_times = vapply(new_times_strings, function(time_str) {
    as.numeric(strsplit(time_str, ':')[[1]]) %*% c(60, 1, 1/60)
  }, numeric(1))
  
  # Run through species
  species = c('M', 'NVE', 'Paur', 'Pnat', 'Ppip', 'Ppyg')
  for(sp in species){
    
    # Subset for species
    sub = dat[dat$species == sp,]
    
    # Only keep single entry per minute
    sub = sub[!duplicated(sub$new_times),]
    
    # Plot
    points(sub$new_dates, sub$new_times, 
           pch = 20, col = alpha(colours[which(species == sp)], 0.1))
   
  } # end species loop
 
  # Add info plot
  text(as.Date('2023-05-01'), 0.95*1400, station, font = 2, adj = 0)
  if(station %in% c('Ballum', 'Husby')){
    axis(2, at = 60*c(0, 6, 12, 18), c('12:00', '18:00', '24:00', '06:00'))
    mtext('Time [hh:mm]', 2, 2.5, cex = 0.75)
  }
  if(station %in% c('Pnat', 'Ppip', 'Ppyg')){
    axis.Date(side = 1, at = c(as.Date('2023-05-01'), 
                               as.Date('2023-08-01'), 
                               as.Date('2023-11-01'), 
                               as.Date('2024-02-01')), 
              labels = c('May', 'Aug', 'Nov', 'Feb'), format='%b')    
    mtext('Month', 1, 2.5, cex = 0.75)
  }
 
} # end file loop

# Print legend
plot.new()
plot.new()
plot.new()
plot.new()
plot.new()
plot.new()
plot.new()
legend('topright', legend = species, col = colours, pch = 16)

# Close PDF
dev.off()

# Message
message('Printed all figures.')