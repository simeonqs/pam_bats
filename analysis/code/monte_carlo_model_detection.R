# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Models SNR vs detection by Aspot.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'callsync')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_detections = 
  '/home/au472091/Documents/results_aspot/defenitely_bats'
path_ground_truth = 'analysis/results/signal_to_noise_test_boejer.csv'
path_png = 'analysis/results/signal_to_noise_vs_detection.png'

# Load data
detections = load.selection.tables(path_detections, recursive = TRUE)
ground_truth = read.csv(path_ground_truth)

# Run through ground truths and add if they have been detected
for(i in seq_len(nrow(ground_truth))){
  sub = detections[detections$file == ground_truth$file[i] &
                     ((detections$Begin.time..s. <= 
                         ground_truth$End.Time..s.[i] &
                         detections$Begin.time..s. >= 
                         ground_truth$Begin.Time..s.[i]) |
                        (detections$End.time..s. >= 
                           ground_truth$Begin.Time..s.[i] &
                           detections$End.time..s. <= 
                           ground_truth$End.Time..s.[i]) |
                        (detections$Begin.time..s. <= 
                           ground_truth$Begin.Time..s.[i] &
                           detections$End.time..s. >= 
                           ground_truth$End.Time..s.[i])),]
  ground_truth$detected[i] = as.numeric(nrow(sub) > 0)
}

# Plot SNR vs detected
png(path_png, width = 6, height = 5, units = 'in', res = 1000)
plot(ground_truth$SNR, ground_truth$detected, 
     main = 'SNR vs detection', xlab = 'SNR', ylab = 'detection probability')
# for(x in unique(round(ground_truth$SNR, -1))){
#   y = ground_truth$detected[round(ground_truth$SNR, -1) == x]
#   points(x, mean(y), col = 2, cex = 2)
#   lines(rep(x, 2), c(mean(y) - sd(y), mean(y) + sd(y)), col = 2)
# }
text(40, 0.4, sprintf('proportion detected: %s', 
                      round(sum(ground_truth$detected)/nrow(ground_truth), 2)))

# Simple linear model
fit = glm(ground_truth$detected ~ ground_truth$SNR, family = 'binomial')
points(ground_truth$SNR, fitted(fit), col = 3, cex = 0.3)
dev.off()







