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
path_d = 'aspot/m16/selection_tables'
path_performance = 'analysis/results/test_data/performance_aspot/'

# List files
gt_files = list.files(path_gt, full.names = TRUE)
d_files = list.files(path_d, full.names = TRUE)

# Function to process file
process.file = function(path){
  
  # Print path
  print(path)
  
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

# Calculate performance
out = lapply(d_files, process.file)

# Make summary
perf_files = list.files(path_performance, pattern = '*.txt', full.names = TRUE)
n_calls = perf_files |> basename() |> str_remove('.txt') |> 
  vapply(function(file) 
    load.selection.table(gt_files[str_detect(gt_files, file)]) |> nrow(), 
    numeric(1))
perf_overview = data.frame(file = basename(perf_files),
                           n_calls = n_calls,
                           n_fp = numeric(length(perf_files)),
                           n_fn = numeric(length(perf_files)),
                           fp_rate = numeric(length(perf_files)),
                           fn_rate = numeric(length(perf_files)))
for(i in seq_along(perf_files)){
  perf = read.table(perf_files[i], sep = ';')
  l1 = which(perf == '$fp')
  l2 = which(perf == '$fn')
  l3 = which(perf == '$fp_rate')
  l4 = which(perf == '$fn_rate')
  # if n_fp is NULL, return zero, else split into all detections and subtract
  # 1, because the string also includes [1] 
  perf_overview$n_fp[i] = ifelse(perf[l1+1,] == 'NULL', 0, 
                                 length(str_split(perf[l1+1,], ' ')[[1]]) - 1)
  perf_overview$n_fn[i] = ifelse(perf[l2+1,] == 'NULL', 0, 
                                 length(str_split(perf[l2+1,], ' ')[[1]]) - 1)
  perf_overview$fp_rate[i] = perf[l3+1,] |> 
    str_remove('\\[1\\] ') |> 
    as.numeric() |> round(3)
  perf_overview$fn_rate[i] = perf[l4+1,] |> 
    str_remove('\\[1\\] ') |> 
    as.numeric() |> round(3)
}
write.csv(perf_overview, sprintf('%s0_summary.csv', path_performance))

# Message
message('Stored performance files.')
