# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Script to check data for spatial model  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'lubridate')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
path_combined_data = 'analysis/results/combined_data.RData'
path_pdf = 'analysis/results/spatial_model/data_check.pdf'

# Load data
load(path_combined_data)

# Check per station for gaps
dat_model$pot_prob = 1
for(station in unique(dat_model$station)){
  sub = dat_model[dat_model$station == station,]
  sub = sub[order(sub$date),]
  diff = diff(sub$date)
  pot_probs = which(diff > 1 & diff < 10)
  dat_model$pot_prob[dat_model$station == station &
                       dat_model$date %in% sub$date[pot_probs]] = 2
}

# Plot dates and bat detections
trans_stations = seq_len(length(unique(dat_model$station)))
names(trans_stations) = sort(unique(dat_model$station))
pdf(path_pdf, 20, 10)
par(mar = c(5, 7, 1, 3), xaxs = 'i', yaxs = 'i')
plot(dat_model$date, trans_stations[dat_model$station], 
     col = as.numeric(dat_model$detection) + 2, cex = dat_model$pot_prob,
     ylim = c(0, max(trans_stations)+1),
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
abline(h = c(0.5, trans_stations + 0.5), lty = 2)
unique_months = unique(format(ymd(dat_model$date), '%Y-%m'))
axis(1, at = as.Date(paste0(unique_months, '-01')), 
     labels = unique_months,
     cex.axis = 1)
axis(2, trans_stations, names(trans_stations), las = 1, cex.axis = 1)
mtext('Date', 1, 3, cex = 1.5)
mtext('Station', 2, 5, cex = 1.5)
dev.off()
