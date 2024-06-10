# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Plots overview of activity throughout night.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'callsync', 'lubridate', 'dplyr')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
path_species_overview = 'analysis/results/species_overview'
path_sun = 'analysis/data/sunrise_sunset_west_coast_DK.csv'
path_trigger = 'analysis/results/activity_overview/summaries/detections'
path_pdf = 'analysis/results/nightly_activity/nightly_activity'

# Load data
sun = read.csv(path_sun)
files = list.files(path_species_overview, '*csv', full.names = TRUE)
files_trigger = list.files(path_trigger, pattern = '*.csv', 
                           recursive = TRUE, full.names = TRUE)

# Remove Skagen
files = files[!str_detect(files, 'Skagen')]

# Translate sun times
sun$rise_min = vapply(sun$Sunrise, function(time_str) {
  as.numeric(strsplit(time_str, ':')[[1]]) %*% c(60, 1, 1/60)
}, numeric(1)) - # making time to minutes
  ifelse((as.Date(sun$Date) > as.Date('2023-03-25') &
            as.Date(sun$Date) < as.Date('2023-10-29')) |
           (as.Date(sun$Date) > as.Date('2024-03-30') &
              as.Date(sun$Date) < as.Date('2024-10-27')),
         60, 0) + # removing summertime 
  12*60 # fixing time so that the date runs from 12:00 to 12:00
sun$set_min = vapply(sun$Sunset, function(time_str) {
  as.numeric(strsplit(time_str, ':')[[1]]) %*% c(60, 1, 1/60)
}, numeric(1)) - 
  ifelse((as.Date(sun$Date) > as.Date('2023-03-25') &
            as.Date(sun$Date) < as.Date('2023-10-29')) |
           (as.Date(sun$Date) > as.Date('2024-03-30') &
              as.Date(sun$Date) < as.Date('2024-10-27')),
         60, 0) - 
  12*60 

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

# Run through species
for(sp in species){
  
  # Open PDF
  pdf(sprintf('%s_%s.pdf', path_pdf, sp), 12, 7)
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
         ylim = c(0, 1440),
         xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    axis.Date(side = 1, at = c(as.Date('2023-05-01'), 
                               as.Date('2023-08-01'), 
                               as.Date('2023-11-01'), 
                               as.Date('2024-02-01')), 
              labels = rep('', 4))
    
    # Load data
    dat = read.csv(file)
    dat = dat[which(dat$n_detections > 5),]
    dat = dat[dat$species == sp,]
    
    # Fix TWJ
    dat$file = ifelse(str_detect(dat$file, 'TWJ'),
                      str_remove(dat$file, '_000'),
                      dat$file)
    
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
    
    # Get summary data
    station = file |> basename() |> str_remove('.csv')
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
              y = c(0, 1440),
              col = '#BDC3C7', border = '#BDC3C7')
    
    # Make shadow for night
    sun_sub = sun[sun$Date > as.Date('2023-04-09') &
                    sun$Date < as.Date('2024-04-10'),]
    polygon(x = c(as.Date(sun_sub$Date)+1, rev(as.Date(sun_sub$Date))),
            y = c(sun_sub$rise_min, rev(sun_sub$set_min)),
            col = '#212F3D', border = '#212F3D')
    
    # Only keep single entry per minute for each species
    dat = dat[!duplicated(dat[c('species', 'new_times')]),]
    
    # Shuffle order
    dat = dat[sample(nrow(dat)),]
    
    # Plot points
    points(dat$new_dates, dat$new_times, 
           pch = 20, col = colours[dat$species], cex = 0.1)
    
    # Add info plot
    text(as.Date('2023-04-15'), 0.93*1440, station, font = 2, adj = 0)
    if(station %in% c('Ballum', 'Husby', 'Nyminde', 'Skjern')){
      axis(2, at = 60*c(0, 6, 12, 18), c('12:00', '18:00', '24:00', '06:00'))
      mtext('Time [hh:mm UTC]', 2, 2.5, cex = 0.75)
    }
    if(station %in% c('Roemoe', 'Skjern', 'Stadiloe')){
      axis.Date(side = 1, at = c(as.Date('2023-05-01'), 
                                 as.Date('2023-08-01'), 
                                 as.Date('2023-11-01'), 
                                 as.Date('2024-02-01')), 
                labels = c('May', 'Aug', 'Nov', 'Feb'), format='%b')    
      mtext('Month', 1, 2.5, cex = 0.75)
    }
    
  } # end file loop
  
  # Close PDF
  dev.off()
  
} # end sp loop

# Message
message('Printed all figures.')