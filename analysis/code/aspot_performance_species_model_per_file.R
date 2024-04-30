# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Combines selection tables from segmentation and classification.
# Then uses ground truth to calculate performance. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'callsync', 'caret')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
model_1 = 52
model_2 = 45
path_segmentation = sprintf('aspot/models/m%s/selection_tables', model_1)
path_classifiction = sprintf('aspot/models_s/m%s/selection_tables', model_2)
path_logs = sprintf('aspot/models_s/m%s/predict', model_2)
path_combined_selection_tables = 
  sprintf('aspot/models_s/m%s/combined_selection_tables', model_2)
path_grount_truth = 
  'analysis/results/test_data/ground_truth_selection_tables_species'
path_pdf = sprintf(
  'analysis/results/confusion_matrix_genus_model_m%s+m%s_per_file.pdf',
  model_1, model_2)

# List files
seg_files = list.files(path_segmentation, '*txt', full.names = TRUE)
class_files = list.files(path_classifiction, '*txt', full.names = TRUE)
log_files = list.files(path_logs, full.names = TRUE)

# Function to calculate overlap
calc.iou = function(st_d, st_g, end_d, end_g){
  union = max(end_d, end_g) - min(st_d, st_g)
  intercept = min(end_d, end_g) - max(st_d, st_g)
  if(intercept > union) stop('Error in calc.overlap.')
  return(intercept/union)
}

# For each file, get all classification files and filled out annotations
for(seg_file in seg_files){
  
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
    selection_table = load.selection.table(seg_file)
    for(cfs in class_files_sub){
      class_table = load.selection.table(cfs)
      type = unique(class_table$Sound.type)
      sel = cfs |> str_extract('(\\d+)_predict') |> 
        str_remove('_predict') |> as.numeric()
      if(length(type) == 1){
        selection_table$Sound.type[sel] = type
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
          selection_table$Sound.type[sel] = type
        } else {
          type = 'noise'
          selection_table$Sound.type[sel] = type
        } # end type greater 1
      } # end one type loop
    } # end cfs loop
    write.table(selection_table, paste(path_combined_selection_tables, 
                                       basename(seg_file), sep = '/'),
                row.names = FALSE,
                sep = '\t', quote = FALSE)
  } # if loop length greater 0
  
} # end file loop

# Message progress
message('Filled out all classifications and stored results.')

# Load selection tables for annotated detections and ground truth
manual = load.selection.tables(path_grount_truth)
aspot = load.selection.tables(path_combined_selection_tables)

# Translate entries
manual$Annotation[manual$Annotation %in% c('Mdau', 'Mdas', 'Mnat', 'm')] = 'M'
manual$Annotation[manual$Annotation %in% c('EVN', 'Nnoc', 'Vmur', 'Eser')] = 
  'NVE'
manual$Annotation[manual$Annotation %in% c('b', 'a')] = 'B'
manual$Annotation[manual$Annotation %in% c('s')] = 'S'

# List unique files
files = list.files(path_grount_truth) |> str_remove('.Table.1.selections.txt')

# Subset for land files
message('Subsetting for LAND.')
files = files[!str_detect(files, 'NS')]
files = files[!str_detect(files, 'HR')]
files = files[!str_detect(files, 'ONBOARD')]

# Create place holders for output
class_results = data.frame()

# Run over all files
for(file in files){
  
  ## subset
  d = aspot[aspot$file == file,]
  g = manual[manual$file == file,]
  
  ## remove species with less than three occurrences
  table_species = table(d$Sound.type)
  table_species = table_species[names(table_species) != 'noise']
  table_species = table_species[table_species >= 5]
  species_d = names(table_species)
  
  ## run through present species
  species_g = unique(g$Annotation)
  species_g = species_g[!species_g %in% c('?', 'o')]
  for(sp in species_g){
    if(sp %in% species_d){
      class_results = rbind(class_results, data.frame(file = file,
                                                      d = sp,
                                                      g = sp)) 
    } else {
      if(sp == 'Ppippyg' & any(species_d %in% c('Ppip', 'Ppyg'))){
        if(any(species_d == 'Ppip')){
          class_results = rbind(class_results, data.frame(file = file,
                                                          d = 'Ppip',
                                                          g = sp))
        }
        if(any(species_d == 'Ppyg')){
          class_results = rbind(class_results, data.frame(file = file,
                                                          d = 'Ppyg',
                                                          g = sp))
        }
      } else {
      class_results = rbind(class_results, data.frame(file = file,
                                                      d = '-error-',
                                                      g = sp))
      } # end else loop
    } # end else
  } # end sp loop (species_g)
  
  ## run through false species
  for(sp in species_d[!species_d %in% species_g]){
    if(!(sp %in% c('Ppip', 'Ppyg') & any(species_g == 'Ppippyg')))
      class_results = rbind(class_results, data.frame(file = file,
                                                      d = sp,
                                                      g = '-error-'))
  } # end sp loop (species_d)
  
} # end file loop

# Plot confusion matrix
pdf(path_pdf)
levels = sort(unique(c(class_results$d, class_results$g)))
conf_matrix = table(factor(class_results$d, levels = levels), 
                    factor(class_results$g, levels = levels))
percentages = conf_matrix
for(i in seq_len(nrow(percentages))) 
  percentages[,i] = percentages[,i]/sum(percentages[,i]) * 100
color_gradient = colorRampPalette(c('lightblue', 'darkblue'))
plot(seq_along(levels), type = 'n', xlab = '', ylab = '',
     xlim = c(0.5, length(levels)+0.5), ylim = c(0.5, length(levels)+0.5),
     xaxt = 'n', yaxt = 'n')
mtext('aspot', 1, 2.5)
mtext('ground truth', 2, 3)
for(i in seq_along(levels)){
  for(j in seq_along(levels)){
    rect(i - 0.5, j - 0.5, i + 0.5, j + 0.5, 
         col = color_gradient(101)[as.numeric(percentages[i, j]+1)])
    text(i, j, labels = conf_matrix[i, j], col = 'white', cex = 1.5)
  }
}
mtext(rownames(conf_matrix), side = 2, at = seq_along(levels), las = 2, 
      line = 0.75)
mtext(colnames(conf_matrix), side = 1, at = seq_along(levels), line = 0.75)
dev.off()

# Message
message('Stored all results.')