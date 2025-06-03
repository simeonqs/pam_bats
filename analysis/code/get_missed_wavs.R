library(stringr)
rm(list=ls()) 

wav_directory = 
  '/media/au472091/T7 Shield/NS12_B_Spring23/NS12_B_Spring23'
prediction_directory = 
  '/media/au472091/Samsung_T5/results_aspot/NS12_B_Spring23/predict'
output_directory = 
  '/media/au472091/T7 Shield/missed_land'

wav_files = list.files(wav_directory, full.names = TRUE)
pred_files = list.files(prediction_directory, full.names = TRUE)

missed_files = 
  wav_files[! str_remove(basename(wav_files), '.wav') %in%
              str_remove(basename(pred_files), '_predict_output.log')]

file.copy(missed_files,
          sprintf('%s/%s',
                  output_directory,
                  basename(missed_files)))
