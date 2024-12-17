files = list.files('/media/au472091/T7 Shield', '*.txt', 
                   recursive = TRUE, full.names = TRUE)
files_st = files[str_detect(files, 'selection_tables')]
st_sizes = lapply(files_st, file.size)
dates = files_st |> basename() |> strsplit('_') |> sapply(`[`, 2) |> 
  as.Date(format = '%Y%m%d')
length(which(unlist(st_sizes[dates <= as.Date('2024-04-10')]) > 148))

stations = unique(summary$station)
stations_from_files = files_st |> basename() |> strsplit('_') |> 
  sapply(`[`, 1) |> str_remove('-LOT1')

stations_from_files = 
  ifelse(stations_from_files == 'KAMMER', 'KAMMERSLUSEN', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'LAND-BALLUM', 'BALLUM', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'LAND-MANDØ', 'MANDØ', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'LAND1', 'KAMMERSLUSEN', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'LAND10', 'REJSBY', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'LAND2', 'BLÅVAND', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'LAND3', 'SKJERN', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'LAND4', 'STADILØ', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'LAND5', 'HUSBY', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'LAND6', 'BALLUM', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'LAND7', 'MANDØ', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'LAND8', 'NYMINDE', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'LAND9', 'FANØ', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'MANDO', 'MANDØ', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'NYMND-PLTG', 'NYMINDE', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'ROEMOE', 'RØMØ', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'STADILOE', 'STADILØ', 
         stations_from_files) 
stations_from_files =
  ifelse(stations_from_files == 'BLAAVAND', 'BLÅVAND', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'FANO', 'FANØ', 
         stations_from_files) 
stations_from_files = 
  ifelse(stations_from_files == 'TWJ-08', 'MANDØ', 
         stations_from_files) 

table(stations_from_files[unlist(
  st_sizes[dates <= as.Date('2024-04-10')]) > 148]) |> as.data.frame()


stations = unique(summary$station)
n_dates = vapply(stations, function(st){
  sub_sum = summary[summary$station == st & summary$offshore,]
  sub_det = summary_detections[summary_detections$station == st,]
  dates = unique(c(sub_sum$date, sub_det$date))
  return(length(dates[dates < as.Date('2024-04-11')]))
}, numeric(1))
print(data.frame(n = n_dates))

stations = unique(summary$station)
n_dates = vapply(stations, function(st){
  sub_sum = summary[summary$station == st,]
  sub_det = summary_detections[summary_detections$station == st,]
  dates = unique(c(sub_sum$DATE |> as.Date(format = '%Y-%b-%d'), sub_det$DATE))
  return(length(dates[dates < as.Date('2024-04-11')]))
}, numeric(1))
print(data.frame(n = n_dates))

