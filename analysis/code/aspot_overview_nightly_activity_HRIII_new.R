# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Plots overview of activity throughout night for HRIII.
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
path_combined_data = 'analysis/results/combined_data.RData'
path_png = 'analysis/results/nightly_activity/nightly_activity_HRIII_new.png'

# Load data and subset for boejer
load(path_combined_data)
dat = dat[which(dat$type_location == 'HRIII'),]

# Open png
png(path_png, 5, 3.5, units = 'in', res = 800)
par(mar = c(4, 4, 1, 1))

# Subset
sub_with_detections = dat[dat$n_bats > 0 & dat$offshore,]

# Add species(complex)
species_station = vapply(sub_with_detections$file_name, function(x){
  species_offshore$sp[species_offshore$Fil == x]
}, character(1))

# for(i in 1:nrow(sub_with_detections)){
#   x = sub_with_detections$file_name[i]
#   print(x)
#   print(species_offshore$sp[species_offshore$Fil == x])
# }

# Make empty plot
plot(NULL, 
     xlim = as.Date(c('2023-04-10', '2024-04-10')), 
     ylim = c(0, 1440),
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
axis.Date(side = 1, at = seq(as.Date('2023-05-01'),
                             as.Date('2024-04-01'),
                             by = 'month'), 
          labels = '')

# Mark missing dates
all_dates = seq(from = as.Date('2023-04-10'),
                to = as.Date('2024-04-10'),
                by = 'day')
missing_dates = all_dates[!all_dates %in% as.Date(dat$date)]
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

# Plot points
points(sub_with_detections$night_date, 
       sub_with_detections$night_time, 
       pch = 20, col = colours[species_station], cex = 1)

# Add info plot
axis(2, at = 60*c(2, 10, 18), c('14:00', '20:00', '06:00'), cex.axis = 1)
mtext('Time (UTC)', 2, 2.8, cex = 1)
axis.Date(side = 1, at = seq(as.Date('2023-05-01'),
                             as.Date('2024-04-01'),
                             by = 'month'),
          cex.axis = 1,
          labels = c('M', 'J', 'J', 'A',
                     'S', 'O', 'N', 'D', 'J',
                     'F', 'M', 'A'), format='%b')    
mtext('Month', 1, 2.8, cex = 1)


# Print legend
# plot.new()
legend('bottomright', legend = (species |> str_replace('NVE', 'ENV'))[c(2, 3)], 
       col = colours[c(2, 3)], pch = 16,
       cex = 0.75)

# Close png
dev.off()

# Message
message('Printed all figures.')