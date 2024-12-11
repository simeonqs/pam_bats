# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam_bats  
# Author: Simeon Q. Smeele
# Description: Compares measured with remodelled weather data.  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'dplyr', 'lubridate')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_combined_data = 'analysis/results/combined_data_with_weather.RData'
path_vestas = 'analysis/data/weather/measured_weather_VESTAS'
path_pdf = 'analysis/results/spatial_model/weather_comparison.pdf'
path_pdf_compare_mills = 
  'analysis/results/spatial_model/weather_comparison_mills.pdf'

# Load data
load(path_combined_data)
files_vestas = list.files(path_vestas, full.names = TRUE)

# Open PDF
pdf(path_pdf, 7, 3)
par(mfrow = c(1, 3))

# Run through stations for which we have data
for(st in c('A05', 'A06', 'C03')){
  
  sub = dat[which(dat$station == st),]
  sub$date_time = sub$date_time |>
    as.POSIXct(format='%Y-%m-%d %H:%M:%S') |>
    floor_date(unit = 'minute')
  sub = sub[!duplicated(sub$date_time),]
  vestas = read.csv(files_vestas[str_detect(files_vestas,
                                            str_remove(st, '0'))])
  vestas$date_time = vestas$time |> 
    as.POSIXct(format='%Y-%m-%d %H:%M:%S') |>
    floor_date(unit = 'minute')
  merged = merge(sub, vestas, by = 'date_time', all.x = TRUE, all.y = FALSE)
  merged = na.omit(merged)
  merged = merged[merged[,colnames(merged)[
    str_detect(colnames(merged), '.WindDirection')]] > 0,]
  plot(merged[,colnames(merged)[str_detect(colnames(merged), '.WindSpeed')]],
       merged$wind_speed,
       xlab = 'Measured wind speed (ms/s)',
       ylab = 'Remodelled wind speed (ms/s)', 
       main = sprintf('Wind speed %s', st))
  abline(a = 0, b = 1)
  cor = cor(
    merged$wind_speed,
    merged[,colnames(merged)[str_detect(colnames(merged), '.WindSpeed')]]
  )
  message('Correlation wind speed ', st, ': ', cor)
  plot(merged[,colnames(merged)[
    str_detect(colnames(merged), '.WindDirection')]],
    merged$wind_direction,
    xlab = 'Measured wind direction (degrees)',
    ylab = 'Remodelled wind direction (degrees)', 
    main = sprintf('Wind direction %s', st))
  abline(a = 0, b = 1)
  cor = cor(
    merged$wind_direction,
    merged[,colnames(merged)[str_detect(colnames(merged), '.WindDirection')]]
  )
  message('Correlation wind direction ', st, ': ', cor)
  plot(merged[,colnames(merged)[str_detect(colnames(merged), '.EnvTmp')]],
       merged$mean_temp,
       xlab = 'Measured temperature (degree celsius)',
       ylab = 'Remodelled temperature (degree celsius)', 
       main = sprintf('Temperature %s', st))
  abline(a = 0, b = 1)
  cor = cor(
    merged$mean_temp,
    merged[,colnames(merged)[str_detect(colnames(merged), '.EnvTmp')]]
  )
  message('Correlation temperature ', st, ': ', cor)
  
} # end st loop

# Close PDF
dev.off()

# Compare weather from different mills
dat_comp_temp = dat_comp_ws = dat_comp_wd =
  data.frame(date_time = dat$date_time)
for(st in c('A05', 'A06', 'C03')){
  
  vestas = read.csv(files_vestas[str_detect(files_vestas,
                                            str_remove(st, '0'))])
  vestas$date_time = vestas$time |> 
    as.POSIXct(format ='%Y-%m-%d %H:%M:%S') |>
    floor_date(unit = 'minute')
  
  temp = vestas[,c(colnames(vestas)[str_detect(colnames(vestas), 
                                               '.EnvTmp')],
                   'date_time')]
  ws = vestas[,c(colnames(vestas)[str_detect(colnames(vestas), 
                                             '.WindSpeed')],
                 'date_time')]
  wd = vestas[,c(colnames(vestas)[str_detect(colnames(vestas), 
                                             '.WindDirection')],
                 'date_time')]
  
  dat_comp_temp = merge(dat_comp_temp, temp, by = 'date_time',
                        all.x = TRUE, all.y = FALSE)
  dat_comp_ws = merge(dat_comp_ws, ws, by = 'date_time',
                      all.x = TRUE, all.y = FALSE)
  dat_comp_wd = merge(dat_comp_wd, wd, by = 'date_time',
                      all.x = TRUE, all.y = FALSE)
  
}

pdf(path_pdf_compare_mills, 5, 5)
plot(na.omit(dat_comp_temp[,-1]))
plot(na.omit(dat_comp_ws[,-1]))
plot(na.omit(dat_comp_wd[,-1]))
dev.off()









