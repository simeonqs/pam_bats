# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Calculate performance for the Animal spot predictions.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('callsync', 'stringr')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_gt = 'analysis/results/test_data/ground_truth_selection_tables'
path_d = 'aspot/m03/selection_tables'
path_performance = 'analysis/results/test_data/performance_aspot/'

# List files
gt_files = list.files(path_gt, full.names = TRUE)
d_files = list.files(path_d, full.names = TRUE)

# Function to process file
process.file = function(path){
  
  # Load detections
  d = load.selection.table(path)
  file_short = path |> basename() |> 
    str_remove('_predict_output.log.annotation.result.txt')
  
  # Compute performance
  gt = load.selection.table(gt_files[str_detect(gt_files, file_short)])
  gt = gt[gt$View == 'Waveform 1',]
  if(nrow(gt) > 0){
    gt = data.frame(start = gt$Begin.Time..s.,
                    end = gt$End.Time..s.,
                    file = 'nr')
  } else {
    d = data.frame(start = numeric(),
                   end = numeric(),
                   file = character())
  }
  if(nrow(d) > 0){
    d = data.frame(start = d$Begin.time..s.,
                   end = d$End.time..s.,
                   file = 'nr')
  } else {
    d = data.frame(start = numeric(),
                   end = numeric(),
                   file = character())
  }
  performance = calc.perf(d, gt)
  
  # Store performance
  sink(sprintf('%s%s.txt', path_performance, file_short))
  print(performance)
  sink()
  
} # end process.file

# Create overview
out = lapply(d_files, process.file)
message('Stored performance files.')
