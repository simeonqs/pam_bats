# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Combines selection tables from segmentation and classification.
# Also creates an overview with species per 3-5 seconds chunks. 
# source('analysis/code/aspot_combine_selection_tables.R')
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'callsync', 'parallel')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
station = 'Stadiloe'
path_segmentation = sprintf(
  '/home/au472091/Documents/results_aspot/%s/selection_tables', station)
path_logs_segmentation = sprintf(
  '/home/au472091/Documents/results_aspot/%s/predict', station)
path_classifiction = sprintf(
  '/home/au472091/Documents/results_aspot/%s/selection_tables_species', 
  station)
path_logs = sprintf(
  '/home/au472091/Documents/results_aspot/%s/predict_species', station)
path_combined_selection_tables = sprintf(
  '/home/au472091/Documents/results_aspot/%s/combined_selection_tables', 
  station)
path_species_overview = sprintf(
  'analysis/results/species_overview/%s.csv', station)

# List files
seg_files = list.files(path_segmentation, '*txt', full.names = TRUE)
log_seg_files = list.files(path_logs_segmentation, full.names = TRUE)
class_files = list.files(path_classifiction, '*txt', full.names = TRUE)
log_files = list.files(path_logs, full.names = TRUE)

# Create empty data frame for species overview
species_overview = data.frame()

# For each file, get all classification files and filled out annotations
if(!dir.exists(path_combined_selection_tables)) 
  dir.create(path_combined_selection_tables)
message('Running with ', length(seg_files), ' selection tables:')
for(seg_file in seg_files){
  
  # Print progress
  message(which(seg_file == seg_files), '/', length(seg_files))
  
  # Get subset 
  ext = 'predict_output.log.annotation.result.txt'
  finder_thingy = paste0('/', str_remove(basename(seg_file), ext)) |>
    str_replace('\\+', '\\\\+')
  class_files_sub = 
    class_files[str_detect(class_files, finder_thingy)]
  
  # If empty, copy empty table
  if(length(class_files_sub) == 0) 
    file.copy(seg_file, 
              paste(path_combined_selection_tables, 
                    basename(seg_file), sep = '/'))
  
  # If not empty, go through detections and store updated table
  if(!length(class_files_sub) == 0){
    
    ## load selection table
    selection_table = load.selection.table(seg_file)
    
    ## run through class_files_sub
    selection_table$Sound.type = 
      mclapply(seq_len(nrow(selection_table)), function(i){
        cfs = class_files_sub[str_detect(class_files_sub, 
                                         sprintf('_%s_predict', i))]
        if(length(cfs) == 0){type = 'NA'} else {
          class_table = load.selection.table(cfs)
          type = unique(class_table$Sound.type)
          sel = cfs |> str_extract('(\\d+)_predict') |> 
            str_remove('_predict') |> as.numeric()
          if(length(type) == 1){
          } else {
            if(length(type) > 1){
              lf = cfs |> basename() |> str_remove('.annotation.result.txt')
              lf = str_replace(lf, '\\+', '\\\\+') # brilliant fix
              lf = paste0('/', lf)
              pred = readLines(log_files[str_detect(log_files, lf)])
              ## loop through prediction file to extract classes and probs
              classes = probs = c()
              for(line in pred){
                if(str_detect(line, '\\|I\\|time=')){
                  classes = c(classes,
                              str_extract(line, 'class=([a-zA-Z]+)') |>
                                str_remove('class='))
                  probs = c(probs,
                            str_extract(line, 'prob=(\\d+\\.\\d+)') |>
                              str_remove('prob=')) |> as.numeric()
                } # end if str_detect loop
              } # end line loop
              type = classes[probs == max(probs)][1]
            } else {
              type = 'noise'
            } # end type greater 1
          } # end one type loop
        } # end if class file found
        return(type)
      }, mc.cores = 4) |> unlist() # end cfs mclapply
    
    ## fix format table
    selection_table$View = 'Spectrogram 1'
    names(selection_table) = c('Selection', 'View', 'Channel',
                               'Begin Time (s)', 'End Time (s)',
                               'Low Freq (Hz)', 'High Freq (Hz)', 
                               'Annotations', 'Comments')
    selection_table = selection_table[-9]
    
    ## write table
    write.table(selection_table, paste(path_combined_selection_tables, 
                                       basename(seg_file), sep = '/'),
                row.names = FALSE,
                sep = '\t', quote = FALSE)
    
    ## make summary per species
    lf = seg_file |> basename() |> str_remove('.annotation.result.txt')
    pred = readLines(log_seg_files[str_detect(log_seg_files, lf)])[9]
    duration = (pred |> strsplit('=') |> sapply(`[`, 2) |> as.numeric())/100
    if(duration < 3) warning('File shorter than 3 seconds: ', lf)
    chunk_starts = seq(0, duration, 5)
    chunk_starts = chunk_starts[duration-chunk_starts > 3]
    for(chunk_start in chunk_starts){
      
      ## subset
      sub = selection_table[selection_table$`Begin Time (s)` >= chunk_start & 
                              selection_table$`End Time (s)` < 
                              (chunk_start+5),]
      
      ## find species
      table_species = table(sub$Annotation)
      species_d = names(table_species)
      
      ## add species
      if(length(species_d) > 0){
        species_overview = rbind(species_overview,
                                 data.frame(
                                   species = species_d,
                                   file = lf |> 
                                     str_remove('_predict_output.log'),
                                   chunk_start = chunk_start,
                                   n_detections = as.numeric(table_species)
                                 ))
      }
      
    } # end chunk_start loop
    
  } # if loop length greater 0
  
} # end file loop

# Write species overview
write.csv(species_overview, path_species_overview)

# Message progress
message('Filled out all classifications and stored results.')