# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats
# Author: Simeon Q. Smeele
# Description: Testing which threshold has best performance. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Import libraries
library(callsync)
library(seewave)
library(tuneR)
library(scales)
library(stringr)
library(parallel)

# Source custom functions
source('analysis/code/test_plot_specs.R')

# List paths
path_files = 'analysis/data/test_data'
path_selection_tables = 
  'analysis/results/test_data/ground_truth_selection_tables'
path_results = 'analysis/results/test_data/threshold_results.csv'

# Settings
# thresholds = seq(0.04, 0.07, 0.0025)
thresholds = seq(50, 70, 5)

# List files to detect on 
files = list.files(path_files, recursive = TRUE, full.names = TRUE)
# files = files[c(5, 10)]

# List selection tables
selection_table_files = list.files(path_selection_tables, 
                                   recursive = TRUE, full.names = TRUE)

# Function to process file
process.file = function(file, threshold){
  
  # Get file name for storing
  file_short = file |> basename() |> str_remove('.WAV') |> str_remove('.wav')
  
  # Run amplitude threshold on one file
  wave = readWave(file)
  orig_max = max(abs(wave@left))
  wave = ffilter(wave, from = 15000, to = 90000, output = 'Wave')
  wave@left = round(wave@left / max(abs(wave@left)) * orig_max)
  detections = call.detect.multiple(wave, threshold = threshold, min_dur = 0,
                                    save_extra = 0.01, env_type = 'summed',
                                    bin_depth = 50, merge_overlap = TRUE)
  
  # Compute performance
  gt = load.selection.table(
    selection_table_files[str_detect(selection_table_files, file_short)])
  gt = gt[gt$View == 'Waveform 1',]
  if(nrow(gt) > 0){
    gt = data.frame(start = gt$Begin.Time..s.,
                    end = gt$End.Time..s.,
                    file = 'nr')
  } else {
    gt = data.frame(start = numeric(),
                    end = numeric(),
                    file = character())
  }
  d = detections/wave@samp.rate
  if(nrow(d) == 0) d$file = character() else d$file = 'nr'
  performance = calc.perf(d, gt)
  duration_detected = sum(d$end - d$start)
  performance$prop_d = duration_detected / (length(wave@left)/wave@samp.rate)
  
  # Return performance
  return(performance)
  
} # end process.file

# Create overview
out = lapply(thresholds, function(threshold) 
  lapply(files, process.file, threshold))

fn_rate = data.frame(files = paste(basename(files), 'fn_rate'))
fn_rate$n_calls = vapply(files, function(file){
  file_short = file |> basename() |> str_remove('.WAV') |> str_remove('.wav')
  gt = load.selection.table(
    selection_table_files[str_detect(selection_table_files, file_short)])
  return(nrow(gt)/2)
}, numeric(1))
for(i in seq_len(length(thresholds))) fn_rate[[as.character(thresholds[i])]] =
  vapply(seq_len(length(files)), function(j) round(out[[i]][[j]]$fn_rate, 3), 
         numeric(1)) 

prop_d = data.frame(files = paste(basename(files), 'prop_d'))
prop_d$n_calls = vapply(files, function(file){
  file_short = file |> basename() |> str_remove('.WAV') |> str_remove('.wav')
  gt = load.selection.table(
    selection_table_files[str_detect(selection_table_files, file_short)])
  return(nrow(gt)/2)
}, numeric(1))
for(i in seq_len(length(thresholds))) prop_d[[as.character(thresholds[i])]] =
  vapply(seq_len(length(files)), function(j) round(out[[i]][[j]]$prop_d, 3), 
         numeric(1)) 

overview = rbind(fn_rate, prop_d)

# Store file
write.csv(overview, path_results, row.names = FALSE)
message('Stored performance file.')
