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
path_segmentation = 'aspot/models/m49/selection_tables'
path_classifiction = 'aspot/models_g/m12/selection_tables'
path_logs = 'aspot/models_g/m12/predict'
path_combined_selection_tables = 'aspot/models_g/m12/combined_selection_tables'
path_grount_truth = 'analysis/results/test_data/ground_truth_selection_tables'
path_pdf = 'analysis/results/confusion_matrix_genus_model.pdf'
path_txt = 'analysis/results/confusion_matrix_genus_model.txt'

# List files
seg_files = list.files(path_segmentation, '*txt', full.names = TRUE)
class_files = list.files(path_classifiction, '*txt', full.names = TRUE)
log_files = list.files(path_logs, full.names = TRUE)

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

# List unique files
files = list.files(path_grount_truth) |> str_remove('.Table.1.selections.txt')

# Create place holders for output
fps = fns = tps = c()
class_results = data.frame(row_d = character(),
                           row_g = character(),
                           d = character(),
                           g = character())

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
      keep_start = d[row_d,]$Begin.time..s. > g[row_g,]$Begin.Time..s. & 
        d[row_d,]$Begin.time..s. < g[row_g,]$End.Time..s.
      keep_end = d[row_d,]$End.time..s. > g[row_g,]$Begin.Time..s. & 
        d[row_d,]$End.time..s. < g[row_g,]$End.Time..s.
      keep_around = d[row_d,]$Begin.time..s. <= g[row_g,]$Begin.Time..s. & 
        d[row_d,]$End.time..s. >= g[row_g,]$End.Time..s.
      if(keep_start | keep_end | keep_around) 
        links = rbind(links,
                      data.frame(row_d = row_d, row_g = row_g))
    }
  }
  
  ## get fps
  new_fps = rownames(d)[!rownames(d) %in% links$row_d]
  fps = c(fps, new_fps)
  
  ## get fns
  new_fns = rownames(g)[!rownames(g) %in% links$row_g]
  fns = c(fns, new_fns)
  
  ## look at true positives and classification
  ### deal with multiple detections for single ground truth
  gs_with_multiple_ds = unique(links$row_g[duplicated(links$row_g)])
  for(x in gs_with_multiple_ds){
    sub = links[links$row_g == x,]
    temp = data.frame(g = manual[sub$row_g,]$Annotation,
                      d = aspot[sub$row_d,]$Sound.type)
    temp$match = temp$g == temp$d
    ### if any match, pick one to be correct classification, remove the rest
    if(any(temp$match)){
      row_name_match = sample(sub$row_d[temp$match], 1)
      row_name_others = sub$row_d[!sub$row_d == row_name_match]
      new_fps = row_name_others
      fps = c(fps, new_fps)
      links = links[-which(links$row_d %in% row_name_others),]
    } else {
      ### if non match, pick one to be wrong classification
      row_name_non_match = sample(sub$row_d, 1)
      row_name_others = sub$row_d[!sub$row_d == row_name_non_match]
      new_fps = row_name_others
      fps = c(fps, new_fps)
      links = links[-which(links$row_d %in% row_name_others),]
    }
    
  }
  ### store remaining
  class_results = rbind(class_results,
                        data.frame(row_d = links$row_d,
                                   row_g = links$row_g,
                                   g = manual[links$row_g,]$Annotation,
                                   d = aspot[links$row_d,]$Sound.type))
  
} # end image loop

# Add all false positives to the class_results as noise in the ground truth
for(fp in fps){
  d = aspot[fp,]
  g = manual[manual$file == d$file,]
  keep_start = d$Begin.time..s. > g$Begin.Time..s. & 
    d$Begin.time..s. < g$End.Time..s.
  keep_end = d$End.time..s. > g$Begin.Time..s. & 
    d$End.time..s. < g$End.Time..s.
  keep_around = d$Begin.time..s. <= g$Begin.Time..s. & 
    d$End.time..s. >= g$End.Time..s.
  keep = keep_start | keep_end | keep_around
  if(length(which(keep)) > 1) 
    warning(sprintf('Found multiple ground truths for fp %s.', fp))
  if(length(which(keep)) == 1) 
    class_results =  rbind(class_results,
                           data.frame(row_d = fp,
                                      row_g = rownames(g[keep,]),
                                      g = g[keep,]$Annotation,
                                      d = d$Sound.type))
  if(length(which(keep)) == 0) 
    class_results =  rbind(class_results,
                           data.frame(row_d = fp,
                                      row_g = 'NaN',
                                      g = 'noise',
                                      d = d$Sound.type))
}

# Plot confusion matrix
pdf(path_pdf)
class_results = class_results[class_results$g != '',]
levels = c('m', 'p', 'v', 'noise')
conf_matrix = table(factor(class_results$d, levels = levels), 
                    factor(class_results$g, levels = levels))
percentages = conf_matrix
for(i in seq_len(nrow(percentages))) 
  percentages[,i] = percentages[,i]/sum(percentages[,i]) * 100
color_gradient = colorRampPalette(c('lightblue', 'darkblue'))
plot(seq_along(levels), type = 'n', xlab = 'aspot', ylab = 'ground truth',
     xlim = c(0.5, length(levels)+0.5), ylim = c(0.5, length(levels)+0.5),
     xaxt = 'n', yaxt = 'n')
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