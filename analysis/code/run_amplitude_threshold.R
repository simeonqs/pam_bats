# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats
# Author: Simeon Q. Smeele
# Description: Running the amplitude threshold detector on real data. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Import libraries
library(callsync)
library(seewave)
library(tuneR)
library(scales)
library(stringr)
library(parallel)

# List paths
path_files = '../../../NA14_A_first_half'
path_detections_txt = 'analysis/results/NA14_A_first_half/detections_txt/'

# List files to detect on 
files = list.files(path_files, recursive = TRUE, full.names = TRUE)
# files = files[1:100]

# Function to proces file
process.file = function(file){
  
  # Get file name for storing
  file_short = file |> basename() |> str_remove('.WAV') |> str_remove('.wav')
  
  # Run amplitude threshold on one file
  wave = readWave(file)
  wave = ffilter(wave, from = 10000, output = 'Wave')
  detections = call.detect.multiple(wave, threshold = 0.045, min_dur = 0,
                                    plot_it = FALSE,
                                    save_extra = 0.01, env_type = 'summed',
                                    bin_depth = 256, merge_overlap = TRUE)
  
  # Export detections
  export.detections(detections, wave@samp.rate,
                    sprintf('%s%s.txt', path_detections_txt, file_short))
  
} # end process.file

# Run and test time
time_1 = Sys.time()
mclapply(files, process.file, mc.cores = 4)
time_2 = Sys.time()
message(sprintf('Processed %s files in %s.', 
                length(files), round(time_2 - time_1)))
