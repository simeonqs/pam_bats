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
path_pdf = 'analysis/results/nightly_activity'

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

# Run through years
for(year in c(2023, 2024)){
  
  print(year)
  
  # Open PDF
  pdf(sprintf('%s_%s.pdf', path_pdf, year), 10, 4)
  layout(matrix(c(1, 1, 2, 2, 3, 3, 7,
                  4, 4, 5, 5, 6, 6, 7),
                byrow = TRUE, nrow = 2, ncol = 7))
  par(mar = rep(0.5, 4), oma = c(3.5, 3.5, 0.5, 0.5))
  
  # Run through species
  species = c('M', 'NVE', 'Paur', 'Pnat', 'Ppip', 'Ppyg')
  for(sp in species){
    
    print(sp)
    
    # Make empty plot
    plot(NULL, 
         xlim = as.Date(c(sprintf('%s-01-01', year), 
                          sprintf('%s-12-31', year))), 
         ylim = c(0, 1440),
         xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    
    # Make shadow for night
    sun_sub = sun[str_detect(sun$Date, as.character(year)),]
    polygon(x = c(as.Date(sun_sub$Date)+1, rev(as.Date(sun_sub$Date))),
            y = c(sun_sub$rise_min, rev(sun_sub$set_min)),
            col = '#D6DBDF', border = '#D6DBDF')
    
    # Run through stations
    for(file in files){
      
      # Load data
      dat = read.csv(file)
      dat = dat[which(dat$species == sp),]
      dat = dat[which(dat$n_detections > 5),]
      dat = dat[!str_detect(dat$file, 'TWJ'),]
      dat = dat[str_detect(dat$file, paste0('_', year)),]
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
      
      # Only keep single entry per minute
      dat = dat[!duplicated(dat$new_times),]
      
      # Plot
      points(dat$new_dates, dat$new_times, 
             pch = 20, col = alpha(colours[which(files == file)], 0.1))
      
    } # end file loop
    
    # Add info plot
    text(as.Date(sprintf('%s-01-15', year)), 0.95*1400, sp, font = 2, adj = 0)
    if(sp %in% c('M', 'Pnat')){
      axis(2, at = 60*c(0, 6, 12, 18), c('12:00', '18:00', '24:00', '06:00'))
      mtext('Time [hh:mm]', 2, 2.5, cex = 0.75)
    }
    if(sp %in% c('Pnat', 'Ppip', 'Ppyg')){
      axis.Date(side = 1, at = c(as.Date(sprintf('%s-01-01', year)), 
                                 as.Date(sprintf('%s-04-01', year)), 
                                 as.Date(sprintf('%s-07-01', year)), 
                                 as.Date(sprintf('%s-10-01', year))), 
                labels = c('Jan', 'Apr', 'Jul', 'Oct'), format='%b')    
      mtext('Month', 1, 2.5, cex = 0.75)
    }
    
  } # end species loop
  
  # Print legend
  plot.new()
  legend('topright', legend = stations, col = colours, pch = 16)
  
  # Close PDF
  dev.off()
  
} # end year loop

# Message
message('Printed all figures.')