# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Loads selection tables from segmentation step and merges calls
# that are very close (these are often echoes or buzzes). Stores output as 
# seperate selection tables. 
# NOTE: not finished.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'callsync')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_selection_tables = 'aspot/models/m52/selection_tables'
path_output = 'aspot/models/m52/merged_calls_selection_tables'

# Run through selection tables
selection_table_files = list.files(path_selection_tables, full.names = TRUE)
for(file in selection_table_files){
  
  # Load selection table
  selection_table = load.selection.table(file)
  selection_table = selection_table[order(selection_table$Begin.time..s.),]
  rownames(selection_table) = seq_len(nrow(selection_table))
  
  # Run through detection and merge close calls
  for(i in seq_len(nrow(selection_table))[-1]){
    
    # Merge if close
    if((selection_table$Begin.time..s.[i] - 
        selection_table$Begin.time..s.[i-1]) <= 0.04) stop()
    
  } # end i loop
  
} # end file loop






