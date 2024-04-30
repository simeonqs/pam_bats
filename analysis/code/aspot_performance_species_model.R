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
model_1 = 54
model_2 = 43
path_segmentation = sprintf('aspot/models/m%s/selection_tables', model_1)
path_classifiction = sprintf('aspot/models_s/m%s/selection_tables', model_2)
path_logs = sprintf('aspot/models_s/m%s/predict', model_2)
path_combined_selection_tables = 
  sprintf('aspot/models_s/m%s/combined_selection_tables', model_2)
path_grount_truth = 
  'analysis/results/test_data/ground_truth_selection_tables_species'
path_pdf = sprintf('analysis/results/confusion_matrix_genus_model_m%s+m%s.pdf',
                   model_1, model_2)
path_txt = sprintf('analysis/results/confusion_matrix_genus_model_m%s+m%s.txt',
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

# List unique files
files = list.files(path_grount_truth) |> str_remove('.Table.1.selections.txt')

# # Subset to LAND files only
# message('Subsetting for LAND.')
# files = files[!str_detect(files, 'NS') & !str_detect(files, 'ONBOARD') &
#                 !str_detect(files, 'HR')]
# aspot = aspot[aspot$file %in% files,]
# manual = manual[manual$file %in% files,]

# Create place holders for output
fps = fns = tps = c()
class_results = data.frame()

# Run through images
for(file in files){
  
  ## subset
  d = aspot[aspot$file == file,]
  g = manual[manual$file == file,]
  
  ## run through detections and ground truths
  links = data.frame(row_d = numeric(),
                     row_g = numeric())
  for(row_d in rownames(d)){
    for(row_g in rownames(g)){
      links = rbind(links,
                    data.frame(file = file,
                               row_d = row_d, 
                               row_g = row_g, 
                               g = manual[row_g,]$Annotation,
                               d = aspot[row_d,]$Sound.type,
                               iou = calc.iou(d[row_d,]$Begin.time..s.,
                                              g[row_g,]$Begin.Time..s.,
                                              d[row_d,]$End.time..s.,
                                              g[row_g,]$End.Time..s.)))
    }
  }
  
  ## only keep if some overlap
  links = links[links$iou > 0,]
  
  ## run through ground truths and remove all but one link
  ## first order so that the ones with highest overlap are at the top
  ## then remove duplications (which will not be the top entry)
  if(nrow(links) > 1){
    links = links[order(links$iou, decreasing = TRUE),]
    links = links[!duplicated(links$row_g),]
    links = links[!duplicated(links$row_d),]
  }
  
  ## check if there are any duplications left
  if(any(duplicated(links$row_d)) | any(duplicated(links$row_g)))
    stop('Found duplications in file ', image, '.')
  
  ## store remaining
  class_results = rbind(class_results,
                        links)
  
  ## get fps
  new_fps = rownames(d)[!rownames(d) %in% links$row_d]
  fps = c(fps, new_fps)
  
  ## get fns
  new_fns = rownames(g)[!rownames(g) %in% links$row_g]
  fns = c(fns, new_fns)
  
} # end image loop

# Add false positives as such
class_results = rbind(class_results,
                      data.frame(file = aspot[fps,]$file,
                                 row_d = fps,
                                 row_g = 'NA',
                                 d = aspot[fps,]$Sound.type,
                                 g = '-noise-',
                                 iou = NA))

# Add false negatives as such
class_results = rbind(class_results,
                      data.frame(file = manual[fns,]$file,
                                 row_d = 'NA',
                                 row_g = fns,
                                 d = '-missed-',
                                 g = manual[fns,]$Annotation,
                                 iou = NA))

# Run checks
if(any(duplicated(class_results$row_d[class_results$row_d != 'NA']))) 
  stop('Duplications in row d.')
if(any(duplicated(class_results$row_g[class_results$row_g != 'NA']))) 
  stop('Duplications in row g.')
if(any(!rownames(aspot) %in% class_results$row_d)) 
  stop('Missing rows from Animal Spot.')
if(any(!rownames(manual[manual$Multiple != 'empty',]) %in% 
       class_results$row_g)) 
  stop('Missing rows from ground truth.')

# Remove overlapping, social, etc. 
class_results = class_results[!class_results$g %in% 
                                c('a', 'b', '?', 'o', 's'),]

# Fix some names
class_results$d[class_results$d == 'noise'] = '-noise-'

# Plot confusion matrix
pdf(path_pdf, 8, 8)
par(mar = c(5, 6, 0.5, 0.5))
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
mtext('aspot', 1, 3)
mtext('ground truth', 2, 4)
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

# Store stats
conf_matrix = confusionMatrix(factor(class_results$d, levels = levels), 
                              factor(class_results$g, levels = levels))
sink(path_txt)
cat(sprintf('Found %s false negative(s). 
                These are the rownumbers in the manual labels for the 
                false negative(s): %s. \n', 
            length(fns),
            paste(fns, collapse = ', ')))
cat(sprintf('Found %s false positives(s). 
                These are the rownumbers in the aspot labels for the 
                false positive(s): %s. \n', 
            length(fps),
            paste(fps, collapse = ', ')))
cat(sprintf('Found %s true positives(s) and plotted the confusion matrix.\n\n',
            nrow(class_results)))
print(conf_matrix)
sink()

# Message
message('Stored all results.')