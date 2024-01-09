library(stringr)
rm(list=ls()) 

wav_directory = 
  '/media/au472091/T7 Shield/temp/NS30_B_Fall2023/Data'
prediction_directory = 
  '/home/au472091/Documents/results_aspot/NS30_B_Fall2023/predict'
output_directory = '/media/au472091/T7 Shield/efteraar/NS30_B_Fall2023/Data'

wav_files = list.files(wav_directory, full.names = TRUE)
pred_files = list.files(prediction_directory, full.names = TRUE)

missed_files = 
  wav_files[! str_remove(basename(wav_files), '.wav') %in%
              str_remove(basename(pred_files), '_predict_output.log')]

file.copy(missed_files,
          sprintf('%s/%s',
                  output_directory,
                  basename(missed_files)))
