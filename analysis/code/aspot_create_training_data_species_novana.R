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
data_set = 33
samp_rate_lower_limit = 192000

# Paths 
path_results = sprintf('aspot/data_sets_s/data_%s', data_set)
path_data = 'analysis/data'
path_selections = sprintf(
  '%s/aspot_selections/target_species/TrainingData_Classification_NOVANAbased_easy',
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
  if(str_detect(file_name, '_')) {
    type = file_name |> str_replace_all(' ', '_') |> strsplit('_') |> 
      sapply(`[`, 6)
  } else {
    type = file_name |> strsplit(' ') |> sapply(`[`, 1)
  }
  type = type |> str_remove('-')
  # if(type == 'MbraMmys') type = 'Mbramys'
  # if(type %in% c('Mdau', 'Mbramys', 'Mnat', 'Mdas')) type = 'M'
  if(type %in% c('Vmur', 'Eser')) type = 'noise'
  if(type %in% c('Nnoc')) type = 'target'
  
  ID = round(runif(1) * 1e7)
  year = 2023
  # year = ifelse(file_name %in%
  #                 c('BigumTjele_SB_db_id_2222_Eser_250721_2341',
  #                   'BigumTjele_SB_db_id_2222_Mdas_260721_0039',
  #                   'BigumTjele_SB_db_id_2222_Mdau_260721_0125',
  #                   'Loevenholm_ELM_db_id_7114_Mnat_100721_0054',
  #                   'BigumTjele_SB_db_id_2222_Nnoc_2500721_2310',
  #                   'Loevenholm_ELM_db_id_7114_Paur_100721_0121',
  #                   'BigumTjele_SB_db_id_2222_Pnat_250721_2305',
  #                   'BigumTjele_SB_db_id_2222_Ppip_250721_2326',
  #                   'BigumTjele_SB_db_id_2222_Ppyg_250721_2346'),
  #               2020,
  #               ifelse(file_name %in%
  #                        c('Grenaa_ELM_db_id_4085_Eser_20210713_2322',
  #                          'Grenaa_ELM_db_id_4085_Mdas_20210714_0036',
  #                          'Grenaa_ELM_db_id_4085_Mdau_20210713_2318',
  #                          'SASkov_SB_db_id_4089_Mnat_040821_2341',
  #                          'Grenaa_ELM_db_id_4085_Nnoc_20210714_0035',
  #                          'SASkov_SB_db_id_4089_Paur_050821_0027',
  #                          'Grenaa_ELM_db_id_4085_Pnat_20210714_0107',
  #                          'Moegeltoender_ELM_db_id_4043_Ppip_20210723_0015',
  #                          'Grenaa_ELM_db_id_4085_Ppyg_20210714_0147'),
  #                      2021,
  #                      2022))
  tape_name = str_replace_all(file_name, '_', '-')
  start_name = round(start / wave@samp.rate * 1000)
  end_name = round(end / wave@samp.rate * 1000)
  new_name = paste0(type, '-bat', '_', ID, '_', year, '_', 
                    tape_name, '_', start_name, '_', end_name)
  
  # Create new wave and save
  new_wave = wave[start:end]
  if(length(new_wave@left)/new_wave@samp.rate > 0.005){
    if(wave@samp.rate > samp_rate_lower_limit){
      if(type %in% c('target', 'noise')){
      new_name = paste0(path_results, '/', new_name, '.wav')
      writeWave(new_wave, new_name, extensible = FALSE)
      }
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
                                     ignore.case = TRUE) ])
  
  # Export for each piece
  sapply(seq_len(nrow(selection_table)), export.selection, 
         selection_table, wave, file_name)
}

# Calls the function
sapply(unique(selection_tables$file), export.selections)

# # Set train, val, test
# all_files = list.files(path_results, '*wav')
# train = all_files[str_detect(all_files, '_2022_')]
# val = all_files[str_detect(all_files, '_2021_')]
# test = all_files[str_detect(all_files, '_2020_')]
# write.table(train, sprintf('%s/train.csv', path_results),
#             row.names = FALSE, col.names = FALSE)
# write.table('train.csv', sprintf('%s/train', path_results),
#             row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table(val, sprintf('%s/val.csv', path_results),
#             row.names = FALSE, col.names = FALSE)
# write.table('val.csv', sprintf('%s/val', path_results),
#             row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table(test, sprintf('%s/test.csv', path_results),
#             row.names = FALSE, col.names = FALSE)
# write.table('test.csv', sprintf('%s/test', path_results),
#             row.names = FALSE, col.names = FALSE, quote = FALSE)

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
