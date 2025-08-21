library(glue)
library(dplyr)
library(stringr)

# Settings
sftp_host = 'io.erda.au.dk'
buffer_size = 258048
remote_base = '/Bat_wo_men/LOT1_DATA/LOT1_LAND_DATA'
local_base = '/media/au472091/data/files_land_winter'
path_combined_data = 'analysis/results/combined_data_land.RData'

# Load data
load(path_combined_data)
dat = dat[dat$type_location == 'land' & !is.na(dat$species),]
month = as.numeric(format(dat$date, '%m'))
day = as.numeric(format(dat$date, '%d'))
dat = dat[month %in% c(11, 12, 1, 2), ]

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

# List folders land
land_folders = sftp_ls(remote_base)[-1]
dep_folders = sapply(land_folders, function(x) sftp_ls(x)[-1]) |> unlist()

# Step through each folder in dat
for (folder in dep_folders) {
  print(glue("Processing folder: {folder}"))
  remote_folder = file.path(folder, 'Data')
  print(glue("Remote path: {remote_folder}"))
  
  # List files in this folder
  listing = sftp_ls(remote_folder)
  
  if (any(grepl('No such file', listing))) {
    stop(glue("Folder not found on remote: {remote_folder}"))
  }
  
  # Files expected for this folder
  files_needed = dat %>% filter(folder_name == basename(folder)) %>% pull(file_name)
  if(length(files_needed) ==0) next
  
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
    local_file = file.path(local_base, file)
    
    batch_file = tempfile()
    writeLines(glue("get \"{remote_file}\" \"{local_file}\""), batch_file)
    cmd = glue("sftp -B {buffer_size} -b {batch_file} {sftp_host}")
    system(cmd)
    unlink(batch_file)
  }
  
  message(glue("Downloaded {length(files_found)} file(s) from {folder}"))
}
