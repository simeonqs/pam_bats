set.seed(5)
path_main = '/media/au472091/cd5d3443-6980-4c41-96c9-fdfa2b7c264e/'
path_audio = 'togter_all'
files = list.files(
  paste(path_main, path_audio, sep = '/'), 
  full.names = TRUE)
random_1000 = sample(files, 1000)
file.copy(random_1000, 
          paste(path_main, 'test_1000', basename(random_1000), sep = '/'))