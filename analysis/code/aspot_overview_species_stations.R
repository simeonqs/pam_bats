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
path_pdf = 'analysis/results/species_overview/species_overview.pdf'

# Settings
ymax = 3.5

# Load data
files = list.files(path_species_overview, '*csv', full.names = TRUE)

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
       xlim = as.Date(c('2023-04-10', '2024-04-10')), 
       ylim = c(0.3, ymax),
       xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
  
  # Load data
  dat = read.csv(file)
  dat = dat[which(dat$n_detections > 5),]
  dat = dat[!str_detect(dat$file, 'TWJ'),]
  dat = dat[dat$species %in% species,]
  station = file |> basename() |> str_remove('.csv')
  
  # Run through species
  for(sp in species){
    
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
    dat = dat[as.Date(dat$new_dates) > as.Date('2023-04-10') &
                as.Date(dat$new_dates) < as.Date('2024-04-10'),]
    
    # Make summary per date
    all_dates = seq(from = as.Date('2023-04-10'),
                    to = as.Date('2024-04-10'),
                    by = 'day')
    n = vapply(all_dates, function(date) 
      length(which(dat[dat$species == sp,]$new_dates == date)),
               numeric(1))
    lines(all_dates, log10(n+1), col = colours[which(species == sp)])
    
  } # end species loop
  
  # Add info plot
  text(as.Date('2023-04-20'), 0.9*ymax, sp, font = 2, adj = 0)
  if(station %in% c('Ballum', 'Husby')){
    axis(2, c(0, 1, 2, 3),  c(1, 10, 100, 1000))
    mtext('Activity', 2, 2.5, cex = 0.75)
  }
  if(station %in% c('Nyminde', 'Kammerslusen', 'Mandoe')){
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
legend('topright', legend = species, col = colours, pch = 16)

# Close PDF
dev.off()


# Message
message('Printed all figures.')








