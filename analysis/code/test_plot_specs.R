plot.all.specs = function(detections, file, path_pdf = NULL){
  
  if(!is.null(path_pdf)) pdf(path_pdf)
  for(i in seq_len(nrow(detections))){
    wave = readWave(file, from = detections$start[i], to = detections$end[i])
    better.spectro(wave)
  }
  if(!is.null(path_pdf)) dev.off()
}


