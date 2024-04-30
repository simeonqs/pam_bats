# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Makes overview of all validation files and checks if none have
# been included as training data. 
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
path_validation_files = 
  'analysis/results/test_data/ground_truth_selection_tables_species'
path_training = 'analysis/data/aspot_selections'

# Read selection tables
validation = load.selection.tables(path_validation_files)

# Make overviews and print
message('Number of calls per class:')
print(table(validation$Annotation))

message('Number of files per class:')
print(table(unique(validation[c('file', 'Annotation')])$Annotation))

message('Number of files per location:')
files = list.files(path_validation_files)
location = ifelse(str_detect(files, 'NS'), 'BØJER',
                  ifelse(str_detect(files, 'ONBOARD'), 'TOGTER',
                         ifelse(str_detect(files, 'HR'), 'HRIII',
                                'LAND')))
print(table(location))

# Test if any validation files are included in training data
training_files = list.files(path_training, recursive = TRUE) |> 
  basename() |>
  str_remove('_predict_output.log.annotation.result.txt') |>
  str_remove('.Table.1.selections.txt') |>
  str_remove('.Table.2.selections.txt')
validation_files = list.files(path_validation_files) |> 
  basename() |>
  str_remove('_predict_output.log.annotation.result.txt') |>
  str_remove('.Table.1.selections.txt') |>
  str_remove('.Table.2.selections.txt')
cross_over = validation_files[validation_files %in% training_files]
if(length(cross_over) > 0) 
  warning('Found validation file(s) in training data!')


