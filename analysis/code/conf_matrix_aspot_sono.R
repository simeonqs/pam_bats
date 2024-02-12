# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Confusion matrix between sonochiro and animal spot. 
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
path_sono = 'analysis/data/sonochiro/results.csv'
path_aspot = 
  '~/Documents/large_data/results_aspot/defenitely_bats/NS30_B_Fall2023'
path_pdf = 'analysis/results/confusion_matrix_sonochiro_aspot.pdf'

# Load data
sono = read.csv(path_sono)
aspot = load.selection.tables(path_aspot)

# Subset sono to only files with bat detections
sono = sono[sono$id_final %in% c('ENVsp', 'Pipnat', 'Pipnat m feeding buzz'),]
rownames(sono) = seq_len(nrow(sono))
sono$id_final = 'target'

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

# List unique files
files = unique(aspot$file)

# Run through images
for(file in files){
  
  ## subset
  d = aspot[aspot$file == file,]
  g = sono[sono$file == file,]
  
  ## run through detections and sonochiros
  links = data.frame(row_d = numeric(),
                     row_g = numeric())
  for(row_d in rownames(d)){
    for(row_g in rownames(g)){
      links = rbind(links,
                    data.frame(file = file,
                               row_d = row_d, 
                               row_g = row_g, 
                               d = aspot[row_d,]$Sound.type,
                               g = sono[row_g,]$id_final,
                               iou = calc.iou(d[row_d,]$Begin.time..s.,
                                              g[row_g,]$Begin.time..s.,
                                              d[row_d,]$End.time..s.,
                                              g[row_g,]$End.time..s.)))
    }
  }
  
  ## only keep if some overlap
  links = links[links$iou > 0,]
  
  ## run through sonochiros and remove all but one link
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
                                 g = 'noise',
                                 iou = NA))

# Add false negatives as such
if(length(fns) > 0)
  class_results = rbind(class_results,
                        data.frame(file = sono[fns,]$file,
                                   row_d = 'NA',
                                   row_g = fns,
                                   d = 'noise',
                                   g = sono[fns,]$id_final,
                                   iou = NA))

# Run checks
if(any(duplicated(class_results$row_d[class_results$row_d != 'NA']))) 
  stop('Duplications in row d.')
if(any(duplicated(class_results$row_g[class_results$row_g != 'NA']))) 
  stop('Duplications in row g.')
if(any(!rownames(aspot) %in% class_results$row_d)) 
  stop('Missing rows from Animal Spot.')
if(any(!rownames(sono[sono$Multiple != 'empty',]) %in% 
       class_results$row_g)) 
  stop('Missing rows from sonochiro.')

# Plot confusion matrix
# pdf(path_pdf)
levels = c('target', 'noise')
conf_matrix = table(factor(class_results$d, levels = levels), 
                    factor(class_results$g, levels = levels))
percentages = conf_matrix
for(i in seq_len(nrow(percentages))) 
  percentages[,i] = percentages[,i]/sum(percentages[,i]) * 100
color_gradient = colorRampPalette(c('lightblue', 'darkblue'))
plot(seq_along(levels), type = 'n', xlab = 'aspot', ylab = 'sonochiro',
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
# dev.off()



