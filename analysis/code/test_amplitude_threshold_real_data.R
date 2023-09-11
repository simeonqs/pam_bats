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

# Run for all files
for(file in files){
  
  # Get file name for storing
  file_short = file |> basename() |> str_remove('.WAV') |> str_remove('.wav')
  
  # Run amplitude threshold on one file
  # wave = readWave(file, from = 9.5, to = 10.5, units = 'seconds')
  wave = readWave(file)
  wave = ffilter(wave, from = 10000, output = 'Wave')
  pdf(sprintf('%s%s.pdf', path_detections_pdf, file_short), 30, 5)
  detections = call.detect.multiple(wave, threshold = 0.07, min_dur = 0, 
                                    plot_it = TRUE,
                                    save_extra = 0.01, env_type = 'summed',
                                    bin_depth = 512, merge_overlap = TRUE)
  dev.off()
  
  # Plot small spectrograms of detections
  # plot.all.specs(detections, file, 
  # sprintf('%s%s.pdf', path_spectrograms, file_short))
  
  # Export detections
  if(nrow(detections) > 0) 
    export.detections(detections, wave@samp.rate,
                      sprintf('%s%s.txt', path_detections_txt, file_short))
  
  # # Compute performance
  # gt = load.selection.table(
  #   selection_table_files[str_detect(selection_table_files, file_short)])
  # gt = gt[gt$View == 'Waveform 1',]
  # gt = data.frame(start = gt$Begin.Time..s.,
  #                 end = gt$End.Time..s.,
  #                 file = 'nr')
  # d = detections/wave@samp.rate
  # d$file = 'nr'
  # performance = calc.perf(d, gt)
  # 
  # # Store performance
  # sink(sprintf('%s%s.txt', path_performance, file_short))
  # print(performance)
  # sink()
  
} # end file loop



