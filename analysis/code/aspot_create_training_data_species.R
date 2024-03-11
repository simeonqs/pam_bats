# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Takes Raven selection tables, annotations that are translated
# from Sonochiro (per file) and raw data to make clips for the multi species
# Animal Spot model. 
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
data_set = 2
types_include = c('m', 'p', 'v')

# Paths 
path_results = sprintf('aspot/data_sets_s/data_%s', data_set)
path_selections_1 = 'aspot/test_data_sets/boeje_test_data/txt'
path_selections_2 = 'analysis/data/aspot_selections/target/pam_guard'
path_selections_3 = 'analysis/data/data_sendttilKristianBerglertilNeuralNet'
path_sono = 'analysis/results/sonochiro/results.csv'
path_wavs = 'analysis/data'

# Create directories
if(!file.exists(path_results)) dir.create(path_results, recursive = TRUE)

# List files
audio_files = c(list.files(path_wavs,  '*wav', full.names = TRUE, 
                           recursive = TRUE), 
                list.files(path_wavs,  '*WAV', full.names = TRUE, 
                           recursive = TRUE))

# Find the selection tables
selection_tables_1 = load.selection.tables(path_selections_1)
selection_tables_2 = load.selection.tables(path_selections_2)
selection_tables_3 = load.selection.tables(path_selections_3, recursive = TRUE)

# Create translation table for sonochiro annotations
trans_sono = c(ENVsp = 'ENVsp',
               `ENVsp x two individuals` = 'ENVsp',
               `Myodau approach` = 'Myodau',
               `Nycnoc` = 'ENVsp',
               `Pipnat` = 'Pipnat',
               `Pipnat m feeding buzz` = 'Pipnat',
               `Pleaur` = 'Pleaur',
               `Pleaur m approach` = 'Pleaur',
               `Pleaur m feeding buzz` = 'Pleaur')

# Load sonochiro and subset for same files
sono = read.csv(path_sono)
sono = sono[sono$Sound.type %in% names(trans_sono),]
selection_tables = selection_tables[selection_tables$file %in% sono$file,]

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
  type = sono$Sound.type[sono$file == selection_table$file[1]][1]
  if(is.null(type)) stop('Type not found.')
  type = trans_sono[type]
  ID = round(runif(1) * 1e7)
  year = 2020
  tape_name = str_replace_all(file_name, '_', '-')
  start_name = round(start / wave@samp.rate * 1000)
  end_name = round(end / wave@samp.rate * 1000)
  new_name = paste0(type, '-bat', '_', ID, '_', year, '_', 
                    tape_name, '_', start_name, '_', end_name)
  
  # Create new wave and save
  new_wave = wave[start:end]
  if(length(new_wave@left)/new_wave@samp.rate > 0.005){
    new_name = paste0(path_results, '/', new_name, '.wav')
    writeWave(new_wave, new_name, extensible = FALSE)
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
                                     ignore.case = TRUE) ])
  
  # Export for each piece
  sapply(seq_len(nrow(selection_table)), export.selection, 
         selection_table, wave, file_name)
}

# Calls the function
sapply(unique(selection_tables$file), export.selections)

# Message
message('Exported all selections.')