# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Takes Raven selection tables and audio files for the Novana
# data. Species abbreviations are in the file name. Outputs Aspot training 
# files for the species model. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'seewave', 'tuneR', 'callsync')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Settings
data_set = 32
samp_rate_lower_limit = 192000

# Paths 
path_results = sprintf('aspot/data_sets_s/data_%s', data_set)
path_data = 'analysis/data'
path_selections = sprintf(
  '%s/aspot_selections/buzz_and_social',
  path_data)
path_wavs = sprintf(
  '%s/audio',
  path_data)

# Create directories
if(!file.exists(path_results)) dir.create(path_results, recursive = TRUE)

# Find the selection tables
selection_tables = load.selection.tables(path_selections, recursive = TRUE)

# List files
audio_files = c(list.files(path_wavs,  '*wav', full.names = TRUE, 
                           recursive = TRUE), 
                list.files(path_wavs,  '*WAV', full.names = TRUE, 
                           recursive = TRUE))

# The function for each selection
export.selection = function(selection, selection_table, wave, file_name){
  
  # Get start and end
  start = round((selection_table$Begin.Time..s.[selection]) * wave@samp.rate)
  if(length(start) == 0) 
    start = round((selection_table$Begin.time..s.[selection]) * wave@samp.rate)
  end = round((selection_table$End.Time..s.[selection]) * wave@samp.rate)
  if(length(end) == 0) 
    end = round((selection_table$End.time..s.[selection]) * wave@samp.rate)
  
  # Create new name
  if(selection_table$Annotation[selection] == 's') type = 'S' else type = 'B'
  
  ID = round(runif(1) * 1e7)
  year = 2023
  tape_name = str_replace_all(file_name, '_', '-')
  start_name = round(start / wave@samp.rate * 1000)
  end_name = round(end / wave@samp.rate * 1000)
  new_name = paste0(type, '-bat', '_', ID, '_', year, '_', 
                    tape_name, '_', start_name, '_', end_name)
  
  # Create new wave and save
  new_wave = wave[start:end]
  if(length(new_wave@left)/new_wave@samp.rate > 0.005){
    if(wave@samp.rate > samp_rate_lower_limit){
      new_name = paste0(path_results, '/', new_name, '.wav')
      writeWave(new_wave, new_name, extensible = FALSE)
    }
  }
  
}

# The running per selection table
export.selections = function(file){
  
  # Read the table
  selection_table = selection_tables[selection_tables$file == file,]
  
  # Find wav file name
  file_name = file |>
    basename() |>
    str_remove('.Table.1.selections.txt') |>
    str_remove('.Table.2.selections.txt') |>
    str_remove('_predict_output.log.annotation.result.txt')
  file_name = sub('\\.\\d+\\.selections\\.txt$', '', file_name)
  
  print(file_name)
  
  # Read the wave
  wave = readWave(audio_files[ grepl(paste0(file_name, '.wav'), audio_files,
                                     ignore.case = TRUE) ][1])
  
  # Export for each piece
  sapply(seq_len(nrow(selection_table)), export.selection, 
         selection_table, wave, file_name)
}

# Calls the function
sapply(unique(selection_tables$file), export.selections)

# Message
files_stored = list.files(path_results, '*wav')
types = files_stored |> strsplit('_') |> sapply(`[`, 1)
message(sprintf('Exported %s selections with following types:',
                length(files_stored)))
print(table(types))

files_stored = list.files(path_results, '*wav', full.names = TRUE)
srs = sapply(files_stored, function(x){
  wave = readWave(x)
  return(wave@samp.rate)
})
message('Sample rates:')
print(table(srs))
