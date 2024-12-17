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
path_ccst = 'aspot/models_s/m46/combined_selection_tables/sts_nnoc.csv'
path_gt = 'analysis/results/test_data/ground_truth_selection_tables_species'
path_pdf = 'analysis/results/confusion_matrices/nnoc_model.pdf'

# Load files
aspot = read.csv(path_ccst)
manual = load.selection.tables(path_gt)

# Subset for NVE
# also keeping some levels of noise, will be removed after
aspot = aspot[aspot$Sound.type %in% c('Nnoc', 'VE'),]
manual = manual[manual$Annotation %in% c('Eser', 'EVN', 'Nnoc', 'Vmur',
                                         'a', 'b', '?', 'o', 's'),]

# Function to calculate overlap
calc.iou = function(st_d, st_g, end_d, end_g){
  union = max(end_d, end_g) - min(st_d, st_g)
  intercept = min(end_d, end_g) - max(st_d, st_g)
  if(intercept > union) stop('Error in calc.overlap.')
  return(intercept/union)
}

# Create place holders for output
fps = fns = tps = c()
class_results = data.frame()

# Run through images
for(file in unique(c(manual$file, aspot$file))){
  
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

# Message
message('Stored all results.')