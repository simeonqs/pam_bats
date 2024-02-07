library(stringr)

files = list.files('/home/au472091/Documents/results_aspot/defenitely_bats', 
                   full.names = TRUE, recursive = TRUE, pattern = '*wav')
files_fall = files[str_detect(files, 'STRANDING')]
write.csv(basename(files_fall), '~/Desktop/files_fall_bats.csv')
