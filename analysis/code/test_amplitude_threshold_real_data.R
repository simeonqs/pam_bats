# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats
# Author: Simeon Q. Smeele
# Description: Testing an amplitude threshold detector on the PAM data. 
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
path_detections_pdf = 'analysis/results/test_data/detections_pdf/'
path_detections_txt = 'analysis/results/test_data/detections_txt/'
path_performance = 'analysis/results/test_data/performance/'
path_selection_tables = 
  'analysis/results/test_data/ground_truth_selection_tables'

# List files to detect on 
files = list.files(path_files, recursive = TRUE, full.names = TRUE)
# files = files[1:2]

# List selection tables
selection_table_files = list.files(path_selection_tables, 
                                   recursive = TRUE, full.names = TRUE)

# Function to proces file
process.file = function(file){
 
  # Get file name for storing
  file_short = file |> basename() |> str_remove('.WAV') |> str_remove('.wav')
  
  # Run amplitude threshold on one file
  wave = readWave(file)
  wave = ffilter(wave, from = 10000, output = 'Wave')
  pdf(sprintf('%s%s.pdf', path_detections_pdf, file_short), 30, 5)
  detections = call.detect.multiple(wave, threshold = 0.04, min_dur = 0,
                                    plot_it = TRUE,
                                    save_extra = 0.01, env_type = 'summed',
                                    bin_depth = 256, merge_overlap = TRUE)
  dev.off()
  
  # Plot small spectrograms of detections
  # plot.all.specs(detections, file, 
  # sprintf('%s%s.pdf', path_spectrograms, file_short))
  
  # Export detections
  if(nrow(detections) > 0) 
    export.detections(detections, wave@samp.rate,
                      sprintf('%s%s.txt', path_detections_txt, file_short))
  
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
  
  # Store performance
  sink(sprintf('%s%s.txt', path_performance, file_short))
  print(performance)
  sink()
  
} # end process.file

# Run and test time
time_1 = Sys.time()
mclapply(files, process.file, mc.cores = 4)
time_2 = Sys.time()
message(sprintf('Processed %s files in %s seconds.', 
                length(files), round(time_2 - time_1)))