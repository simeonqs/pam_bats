library(stringr)

path_results = '/media/au472091/data/new_results_aspot/defenitely_bats'
path_combined_data = 'analysis/results/combined_data.RData'
path_out = '~/Desktop/ENV_files'

audio_files = list.files(path_results, '*.wav', 
                         full.names = TRUE, recursive = TRUE)
load(path_combined_data)
sub = dat[which(dat$species == 'NVE'),]

for(row in seq_len(nrow(sub))){
  file = audio_files[str_detect(audio_files, sub$file_name[row])]
  if(length(file) != 1) stop('Problem finding file.')
  file.copy(file, 
            paste(path_out, basename(file), sep = '/'))
}

write.csv(data.frame(file = basename(sub$file_name),
                     art = '',
                     noter = ''),
          '~/Desktop/template.csv',
          row.names = FALSE)
