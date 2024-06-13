# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Plots overview of activity throughout night for bøjer.
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
path_sun = 'analysis/data/sunrise_sunset_west_coast_DK.csv'
path_png = 'analysis/results/nightly_activity/nightly_activity_bøjer.png'
path_data_bats = '/home/au472091/Documents/results_aspot/defenitely_bats'
path_meta_boejer = 'analysis/data/meta_data_boejer.csv'
path_trigger = 'analysis/results/activity_overview/summaries_backup/detections'

# Load data
sun = read.csv(path_sun)
folders = list.files(path_data_bats, full.names = TRUE)
folders = folders[str_detect(folders, 'NS')]
meta = read.csv(path_meta_boejer)

# List files with summaries of detections
files_trigger = list.files(path_trigger, pattern = '*.csv', 
                           recursive = TRUE, full.names = TRUE)

# Fix station names
meta$Station.ID = ifelse(meta$Station.ID %in% c('HR3_4', 'HR3-4'), 'HR3-4S-C',
                         meta$Station.ID)
meta$Station.ID = ifelse(str_detect(meta$Station.ID, 'H_R3_6'), 'H_R3_6',
                         meta$Station.ID)
meta$Station.ID = ifelse(meta$Station.ID == 'T3/NS26', 'T3-NS26-C',
                         meta$Station.ID)
meta$Station.ID = ifelse(meta$Station.ID == 'NS6', 'NS6-C',
                         meta$Station.ID)
meta$Station.ID = ifelse(meta$Station.ID == 'NS24', 'NS24S',
                         meta$Station.ID)
meta$Station.ID = ifelse(meta$Station.ID == 'NS27', 'NS27S',
                         meta$Station.ID)
meta$Station.ID = ifelse(meta$Station.ID == 'NS28', 'NS28S',
                         meta$Station.ID)
meta$Station.ID = ifelse(meta$Station.ID == 'NS32', 'NS32S',
                         meta$Station.ID)

# Translate times and dates
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
meta$Deployment..Service.date = meta$Deployment..Service.date |> 
  as.character() |>
  as.Date(format = '%Y%m%d')
meta$recovery.date = meta$recovery.date |> 
  as.character() |>
  as.Date(format = '%Y%m%d')


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
png(path_png, 12, 6, units = 'in', res = 800)
layout(matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3,
                4, 4, 4, 5, 5, 5, 6, 6, 6,
                7, 7, 7, 8, 8, 8, 9, 9, 9),
              byrow = TRUE, nrow = 3, ncol = 9))
par(mar = rep(0.5, 4), oma = c(3.5, 4, 0.5, 0.5))

# Run through stations
for(folder in folders){
  
  # Get detections
  st = folder |> basename() |> strsplit('_') |> sapply(`[`, 1)
  files = list.files(folder, '*wav')
  dates = files |> strsplit('_') |> sapply(`[`, 2) |> 
    as.Date(format = '%Y%m%d')
  times = files |> strsplit('_') |> sapply(`[`, 3) |> str_remove('.wav') 
  
  # Remove entries when onshore
  sub_meta = meta[meta$Station.ID == st,]
  dates_station = dates |> unique() |> sort()
  keep_dates = c()
  for(i in seq_len(nrow(sub_meta))){
    start = sub_meta$Deployment..Service.date[i]
    end = sub_meta$recovery.date[i] 
    keep_dates = c(keep_dates, 
                   dates_station[dates_station > start &
                                   dates_station < (end - 2)] |> 
                     as.character())
  } 
  remove_dates = dates_station[!dates_station %in% keep_dates]
  times = times[!dates %in% remove_dates]
  dates = dates[!dates %in% remove_dates]
  
  # Skip if no dates left
  if(length(dates) == 0) next
  
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
  
  # Fix dates and times
  rounded_times = format(strptime(times, '%H%M%S'), '%H:%M')
  dates_times = paste(dates, times, sep = '_')
  new_dates_times = ymd_hms(dates_times) - hours(12)
  new_dates = new_dates_times |>
    as.Date(format = '%Y:%m:%d')
  new_times_strings = new_dates_times |> as.character() |> strsplit(' ') |> 
    sapply(`[`, 2)
  new_times = vapply(new_times_strings, function(time_str) {
    as.numeric(strsplit(time_str, ':')[[1]]) %*% c(60, 1, 1/60)
  }, numeric(1))

  # Get summary data
  print(st)
  file_trigger_station = 
    files_trigger[str_detect(files_trigger, st)]
  summary = file_trigger_station |>
    lapply(read.csv) |> bind_rows()
  summary$date[is.na(summary$date)] = 
    summary$DATE[is.na(summary$date)]
  summary = summary[as.Date(summary$date) > as.Date('2023-04-10') &
                      as.Date(summary$date) < as.Date('2024-04-10'),]
  
  # Mark missing dates
  all_dates = seq(from = as.Date('2023-04-10'),
                  to = as.Date('2024-04-10'),
                  by = 'day')
  missing_dates = all_dates[!all_dates %in% as.Date(summary$date)]
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
  
  # # Only keep single entry per minute for each species
  # dat = dat[!duplicated(dat[c('species', 'new_times')]),]
  # 
  # # Shuffle order
  # dat = dat[sample(nrow(dat)),]
  
  # Plot points
  points(new_dates, new_times, 
         pch = 20, col = 'white', cex = 1)
  
  # Add info plot
  text(as.Date('2023-04-15'), 0.93*1440, st, font = 2, adj = 0, cex = 1.5)
  if(st %in% c('NS19', 'NS30', 'NS33')){
    axis(2, at = 60*c(2, 10, 18), c('14:00', '20:00', '06:00'), cex.axis = 1.4)
    mtext('Time (UTC)', 2, 2.8, cex = 1)
  }
  if(st %in% c('NS35', 'NS33', 'NS34')){
    axis.Date(side = 1, at = c(as.Date('2023-05-01'), 
                               as.Date('2023-08-01'), 
                               as.Date('2023-11-01'), 
                               as.Date('2024-02-01')),
              cex.axis = 1.4,
              labels = c('May', 'Aug', 'Nov', 'Feb'), format='%b')    
    mtext('Month', 1, 2.8, cex = 1)
  }
  
} # end file loop

# # Print legend
# plot.new()
# legend('bottomright', legend = species, col = colours, pch = 16)

# Close png
dev.off()

# Message
message('Printed all figures.')