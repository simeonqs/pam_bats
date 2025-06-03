path = "/media/au472091/data/new_results_aspot/land"
files = list.files(path, '_predict_output.log.annotation.result.txt', 
                   recursive = TRUE, full.names = TRUE)
files = files[str_detect(files, 'selection_tables')]
sts = files |> lapply(function(x) {print(x); load.selection.table(x)})
n = sts |> lapply(nrow)
n_with_bats = length(which(n>0))
