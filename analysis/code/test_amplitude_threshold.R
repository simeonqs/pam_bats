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
path_files = 'analysis/data/eksempler'
path_detections_pdf = 'analysis/results/eksempler/detections_pdf/'
path_detections_txt = 'analysis/results/eksempler/detections_txt/'
path_performance = 'analysis/results/eksempler/performance/'
path_spectrograms = 'analysis/results/eksempler/spectrograms/'
path_selection_tables = 
  'analysis/results/eksempler/ground_truth_selection_tables'

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
  wave = readWave(file)
  orig_max = max(abs(wave@left))
  wave = ffilter(wave, from = 10000, to = 90000, 
                 output = 'Wave')
  wave = downsample(wave, 192000)
  pdf(sprintf('%s%s.pdf', path_detections_pdf, file_short), 30, 5)
  detections = call.detect.multiple(wave, threshold = 0.065, min_dur = 0,
                                    plot_it = TRUE,
                                    save_extra = 0.01, env_type = 'summed',
                                    bin_depth = 128, merge_overlap = TRUE)
  dev.off()
  wave@left = round(wave@left / max(abs(wave@left)) * orig_max)
  
  # Plot small spectrograms of detections
  # plot.all.specs(detections, file, 
                 # sprintf('%s%s.pdf', path_spectrograms, file_short))
  
  # Export detections
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
  
} # end file loop



