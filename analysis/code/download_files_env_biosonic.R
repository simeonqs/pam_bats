library(glue)
library(dplyr)
library(stringr)

# Settings
sftp_host = 'io.erda.au.dk'
buffer_size = 258048
remote_base = '/Bat_wo_men/LOT1_DATA/LOT1_LAND_DATA/Mandoe_LAND7'
local_base = '/media/au472091/T7_green/files'
station = 'Mandoe'
path_combined_data = 'analysis/results/combined_data_land.RData'

# Load data
load(path_combined_data)
dat = dat[dat$station == station,]
dat = dat[which(str_detect(dat$species, 'ENV')),]
dat = dat[dat$date >= as.Date('2024-04-10'),]

# Add .wav extension to file_name
dat = dat %>%
  mutate(file_name = paste0(file_name, '.wav')) %>%
  distinct(folder_name, file_name)

# Function to list files in a remote SFTP directory
sftp_ls = function(path) {
  cmd_file = tempfile()
  writeLines(glue("ls \"{path}\""), cmd_file)
  full_cmd = glue("sftp -B {buffer_size} -b {cmd_file} {sftp_host}")
  out = suppressWarnings(system(full_cmd, intern = TRUE))
  unlink(cmd_file)
  return(str_trim(out))
}

# Step through each folder in dat
for (folder in unique(dat$folder_name)) {
  print(glue("Processing folder: {folder}"))
  remote_folder = file.path(remote_base, folder, 'Data')
  print(glue("Remote path: {remote_folder}"))
  
  # List files in this folder
  listing = sftp_ls(remote_folder)
  
  if (any(grepl('No such file', listing))) {
    stop(glue("Folder not found on remote: {remote_folder}"))
  }
  
  # Files expected for this folder
  files_needed = dat %>% filter(folder_name == folder) %>% pull(file_name)
  
  # Files actually present in listing
  files_found = intersect(files_needed, basename(listing))
  
  # Throw error if not all expected files are present
  if (length(files_found) < length(files_needed)) {
    missing = setdiff(files_needed, files_found)
    stop(glue("Missing files in {folder}: {paste(missing, collapse = ', ')}"))
  }
  
  # Download each file
  for (file in files_found) {
    remote_file = file.path(remote_folder, file)
    local_file = file.path(local_base, station, file)
    
    batch_file = tempfile()
    writeLines(glue("get \"{remote_file}\" \"{local_file}\""), batch_file)
    cmd = glue("sftp -B {buffer_size} -b {batch_file} {sftp_host}")
    system(cmd)
    unlink(batch_file)
  }
  
  message(glue("Downloaded {length(files_found)} file(s) from {folder}"))
}
