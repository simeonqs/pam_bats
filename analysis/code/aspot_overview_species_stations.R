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
path_pdf = 'analysis/results/species_overview/species_overview'

# Settings
ymax = 3.5

# Load data
files = list.files(path_species_overview, '*csv', full.names = TRUE)

# Colours stations 
colours = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', 
            '#CC79A7', '#999999', '#4D4D4D', '#A56EFF')
stations = files |> basename() |> str_remove('.csv')

# Run through years
for(year in c(2023, 2024)){
  
  # Open PDF
  pdf(sprintf('%s_%s.pdf', path_pdf, year), 10, 4)
  layout(matrix(c(1, 1, 2, 2, 3, 3, 7,
                  4, 4, 5, 5, 6, 6, 7), 
                byrow = TRUE, nrow = 2, ncol = 7))
  par(mar = rep(0.5, 4), oma = c(3.5, 3.5, 0.5, 0.5))
  
  # Run through species
  species = c('M', 'NVE', 'Paur', 'Pnat', 'Ppip', 'Ppyg')
  for(sp in species){
    
    # Make empty plot
    plot(NULL, 
         xlim = as.Date(c(sprintf('%s-01-01', year), 
                          sprintf('%s-12-31', year))), 
         ylim = c(0.3, ymax),
         xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    
    # Run through stations
    for(file in files){
      
      # Load data
      dat = read.csv(file)
      dat = dat[which(dat$species == sp),]
      dat = dat[which(dat$n_detections > 5),]
      station = file |> basename() |> str_remove('.csv')
      
      # Fix dates and times
      split = dat$file |> strsplit('_')
      dates = sapply(split, function(x) x[length(x)-1])
      times = sapply(split, function(x) x[length(x)])
      dates_times = paste(dates, times, sep = '_')
      new_dates_times = ymd_hms(dates_times) - hours(12)
      dat$date = new_dates_times |>
        as.Date(format = '%Y:%m:%d')
      
      # Make summary per date
      all_dates = seq(as.Date(sprintf('%s-01-01', year)), 
                      as.Date(sprintf('%s-12-31', year)), 'day')
      n = vapply(all_dates, function(date) length(which(dat$date == date)),
                 numeric(1))
      lines(all_dates, log10(n+1), col = colours[which(files == file)])
      
    } # end file loop
    
    # Add info plot
    text(as.Date(sprintf('%s-01-15', year)), 0.9*ymax, sp, font = 2, adj = 0)
    if(sp %in% c('M', 'Pnat')){
      axis(2, c(0, 1, 2, 3),  c(1, 10, 100, 1000))
      mtext('Activity', 2, 2.5, cex = 0.75)
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
  legend('topright', legend = stations, col = colours, lwd = 1)
  
  # Close PDF
  dev.off()
  
} # end year loop

# Message
message('Printed all figures.')








