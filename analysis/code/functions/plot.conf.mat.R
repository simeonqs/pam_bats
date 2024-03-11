plot.conf.mat = function(levels, class_results, path_pdf = NULL,
                         labs = c('detections', 'manual'), main = ''){
  if(!is.null(path_pdf)) pdf(path_pdf)
  conf_matrix = table(factor(class_results$d, levels = levels), 
                      factor(class_results$g, levels = levels))
  percentages = conf_matrix
  for(i in seq_len(nrow(percentages))) 
    percentages[,i] = percentages[,i]/sum(percentages[,i]) * 100
  color_gradient = colorRampPalette(c('lightblue', 'darkblue'))
  plot(seq_along(levels), type = 'n', xlab = labs[1], ylab = labs[2],
       xlim = c(0.5, length(levels)+0.5), ylim = c(0.5, length(levels)+0.5),
       xaxt = 'n', yaxt = 'n', main = main)
  for(i in seq_along(levels)){
    for(j in seq_along(levels)){
      rect(i - 0.5, j - 0.5, i + 0.5, j + 0.5, 
           col = color_gradient(101)[as.numeric(percentages[i, j]+1)])
      text(i, j, labels = conf_matrix[i, j], col = 'white', cex = 1.5)
    }
  }
  mtext(rownames(conf_matrix), side = 2, at = seq_along(levels), las = 2, 
        line = 0.75, cex = 0.5)
  mtext(colnames(conf_matrix), side = 1, at = seq_along(levels), 
        line = 0.75, cex = 0.5)
  if(!is.null(path_pdf)) dev.off()
}