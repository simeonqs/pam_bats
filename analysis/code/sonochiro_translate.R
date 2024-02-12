# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Translate sqlite3 file from Sonochiro to txt similar to output
# from Animal Spot. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'RSQLite')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_sqlite_file = 'analysis/data/sonochiro/Data.sqlite3'
path_output = 'analysis/data/sonochiro/results.csv'

# Connect to the SQLite database
con = dbConnect(SQLite(), dbname = path_sqlite_file)

# Print tables
tables = dbListTables(con)

# Fetch all rows from the selected table
signal = dbGetQuery(con, 'SELECT * FROM signal')
audiofile = dbGetQuery(con, 'SELECT * FROM audiofile')
sequence = dbGetQuery(con, 'SELECT * FROM sequence')
evaluation = dbGetQuery(con, 'SELECT * FROM evaluation')
audiofileprocessing = dbGetQuery(con, 'SELECT * FROM audiofileprocessing')

# Close connection
dbDisconnect(con)

# For NS30_A fall we have:
# 1786 file names
# 1786 processing
# 3846 sequences -> split of 5 seconds
# 39691 signals

# sequency key from signal to processing key from sequence to 
# audiofile key from processing to filename from audiofile

# Translate into single data.frame
trans_key = unique(audiofile[c('key', 'filename')])
colnames(trans_key)[colnames(trans_key) == 'key'] = 'audiofile' 
trans_key = merge(trans_key, 
                  audiofileprocessing[c('key', 'audiofile')], 
                  by = 'audiofile')
colnames(trans_key)[colnames(trans_key) == 'key'] = 'processing' 
trans_key = merge(trans_key, 
                  sequence[c('key', 'processing')], 
                  by = 'processing')
colnames(trans_key)[colnames(trans_key) == 'key'] = 'sequence' 
trans_key = merge(trans_key, 
                  signal[c('key', 'sequence')], 
                  by = 'sequence')
rownames(trans_key) = trans_key$key
out = data.frame(file = trans_key[signal$key,]$filename |> str_remove('.wav'),
                 sequence = trans_key[signal$key,]$sequence,
                 `Begin time (s)` = signal$start_time/1000,
                 `End time (s)` = signal$end_time/1000)
out = merge(out,
            evaluation[!duplicated(evaluation$sequence),
                       c('sequence', 'id_final')],
            by = 'sequence', all.x = TRUE, all.y = FALSE)

# Print head
message('Sample of the result:')
print(head(out))

# Store
write.csv(out, path_output, row.names = FALSE)