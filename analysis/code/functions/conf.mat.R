conf.mat = function(detections, manual){
  
  # Fix some column names
  names(manual)[names(manual) == 'Begin.Time..s.'] = 'Begin.time..s.'
  names(manual)[names(manual) == 'End.Time..s.'] = 'End.time..s.'

  # Add sound type if missing
  if(!'Sound.type' %in% rownames(manual)) manual$Sound.type = 'target'
  
  # Create place holders for output
  fps = fns = tps = c()
  class_results = data.frame()
  
  # List unique files
  files = unique(c(detections$file, manual$file))
  
  # Run through images
  for(file in files){
    
    ## subset
    d = detections[detections$file == file,]
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
                                 d = detections[row_d,]$Sound.type,
                                 g = manual[row_g,]$Sound.type,
                                 iou = calc.iou(d[row_d,]$Begin.time..s.,
                                                g[row_g,]$Begin.time..s.,
                                                d[row_d,]$End.time..s.,
                                                g[row_g,]$End.time..s.)))
      }
    }
    
    ## only keep if some overlap
    links = links[links$iou > 0,]
    
    ## run through manual and remove all but one link
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
                        data.frame(file = detections[fps,]$file,
                                   row_d = fps,
                                   row_g = 'NA',
                                   d = detections[fps,]$Sound.type,
                                   g = 'noise',
                                   iou = NA))
  
  # Add false negatives as such
  if(length(fns) > 0)
    class_results = rbind(class_results,
                          data.frame(file = manual[fns,]$file,
                                     row_d = 'NA',
                                     row_g = fns,
                                     d = 'noise',
                                     g = manual[fns,]$Sound.type,
                                     iou = NA))
  
  # Run checks
  if(any(duplicated(class_results$row_d[class_results$row_d != 'NA']))) 
    stop('Duplications in row d.')
  if(any(duplicated(class_results$row_g[class_results$row_g != 'NA']))) 
    stop('Duplications in row g.')
  if(any(!rownames(detections) %in% class_results$row_d)) 
    stop('Missing rows from detections.')
  if(any(!rownames(manual[manual$Multiple != 'empty',]) %in% 
         class_results$row_g)) 
    stop('Missing rows from ground truth.')
  
  # Return result
  return(class_results)
  
} # end conf.mat