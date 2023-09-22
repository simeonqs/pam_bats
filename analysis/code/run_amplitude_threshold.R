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
path_wavs = 'analysis/results/NA14_A_first_half/wavs/'

# List files to detect on 
files = list.files(path_files, recursive = TRUE, full.names = TRUE)
files = files[1:100]

# Function to export selection to wav
export.detections.to.wav = function(wave, detections, base_path_out){
  if(nrow(detections) > 0){
    lapply(seq_len(nrow(detections)), function(i)
      writeWave(wave[detections$start[i]:detections$end[i]],
                sprintf('%s_%s_%s_%s.wav', base_path_out, i, 
                        detections$start[i], detections$end[i]),
                extensible = FALSE))
  }
}

# Function to process file
process.file = function(file){
  
  # Get file name for storing
  file_short = file |> basename() |> str_remove('.WAV') |> str_remove('.wav')
  
  # Run amplitude threshold on one file
  wave = readWave(file)
  wave = ffilter(wave, from = 10000, to = 90000, 
                 output = 'Wave')
  wave = downsample(wave, 192000)
  detections = call.detect.multiple(wave, threshold = 0.065, min_dur = 0,
                                    save_extra = 0.01, env_type = 'summed',
                                    bin_depth = 128, merge_overlap = TRUE)

  # Export detections
  export.detections(detections, wave@samp.rate,
                    sprintf('%s%s.txt', path_detections_txt, file_short))
  base_path_out = sprintf('%s%s', path_wavs, file_short)
  
} # end process.file

# Run and test time
time_1 = Sys.time()
mclapply(files, process.file, mc.cores = 4)
time_2 = Sys.time()
message(sprintf('Processed %s files in %s.', 
                length(files), round(time_2 - time_1)))
