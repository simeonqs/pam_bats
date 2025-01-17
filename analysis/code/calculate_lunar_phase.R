# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Calculate lunar phase. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Load the necessary package
library(lunar)

# Paths
path_out = 'analysis/data/lunar_data_esbjerg.csv'

# Define the date range
start_date = as.Date('2023-04-10')
end_date = as.Date('2025-04-10')

# Generate the sequence of dates from start to end
dates = seq(start_date, end_date, by = 'days')

# Calculate the lunar phases for each date using Esbjerg's coordinates
lunar_phases = lunar.phase(dates, lat = 55.4762, lon = 8.4493)  # Esbjerg

# Create a data frame with the date and lunar phase
lunar_data = data.frame(
  date = dates,
  lunar_phase = lunar_phases
)

# Write csv
write.csv(lunar_data, path_out, row.names = FALSE)

