![image](https://github.com/simeonqs/pam_bats/assets/48213863/50889284-d160-46c7-9664-fa60fc20baad)# Overview

Private repository for the North Sea Lot1 PAM of bats. If you are not Simeon, please don't share. 

# Meta data

The repository contains three main folders. `analysis` contains all the data, code and results for the analysis in `R`. `aspot` contains all the data, code and results for the analysis in `aspot`. `documents` contains all other relevant documents for the project. Only `analysis` is currently tracked by `git`. 

Meta data per file:

- `pam_bats.Rproj`: R project file, if you open this, all relative paths should work
- `README.md`: you are reading this file now

- `analysis/code/activity_overview_aspot.R`: script to summarise the detections found by animal spot, outputs a csv file for all detections per folder and a csv file with only verified detections of bats per folder
- `analysis/code/activity_overview_csv`: script to store all information per station in a single csv file; columns contain
  - `station`: name of the station
  - `n_files`: number of wav files recorded
  - `n_detections`: number of detections from animal spot
  - `n_bats`: number of detections that are varified as bat calls
  - `total_duration_h`: the total duration (of all wav files combined) in hours
  - `fp_rate_1000windows`: false positive rate - number of false positives per 1000 windows (animal spot looks at 20 ms windows with 10 ms overlap)
  - `fp_rate_h`: false positive rate - number of false positives per hour
- `analysis/code/activity_overview_durations.R`: script to calculate durations per wav file from the prediction output from animal spot; stores a csv file per folder with four columns:
  - `file`: name of the original wav file
  - `duration`: total duration in seconds
  - `n_windows`: total number of windows that were predicted upon (animal spot looks at 20 ms windows with 10 ms overlap)
  - `station`: name of the station
- `analysis/code/`:
- `analysis/code/`:
- `analysis/code/`:
- `analysis/code/`:
- `analysis/code/`:
- `analysis/code/`:
