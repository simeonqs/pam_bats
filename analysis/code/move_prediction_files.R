library(stringr)
rm(list=ls()) 

wav_directory = 
  '/media/au472091/T7 Shield/LOT_1_BØJER_DATA/NS19_A_Spring23/Data'
prediction_directory = 
  '/home/au472091/Documents/results_aspot/NS19_A_Spring23/predict'
output_directory = 
  '/home/au472091/Documents/results_aspot/NS19_B_Spring23/predict'

wav_files = list.files(wav_directory, full.names = TRUE)
pred_files = list.files(prediction_directory, full.names = TRUE)

move = 
  pred_files[!str_remove(basename(pred_files), '_predict_output.log') %in%
              str_remove(basename(wav_files), '.wav')]

file.copy(move,
          sprintf('%s/%s',
                  output_directory,
                  basename(move)))
file.remove(move)
