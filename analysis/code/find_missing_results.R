# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: List all folders that have been run through Aspot. Compare to 
# list of directories. Output list that still has to be run. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(dplyr)
library(stringr)

paths_results = 
  list.files('/media/au472091/data/new_results_aspot/land')
paths_dirs = read.table('analysis/data/all_paths_ERDA.txt', sep = ',')$V1

paths_dirs = paths_dirs[str_detect(paths_dirs, 'LOT1_LAND_DATA')]
paths_dirs = paths_dirs[!basename(paths_dirs) %in% paths_results]
write.table(paths_dirs |> as.data.frame(), 
            '~/Desktop/directories_to_download.txt', 
            row.names = FALSE, col.names = FALSE, quote = FALSE)
