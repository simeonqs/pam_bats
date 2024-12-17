sub = dat[dat$type_location == 'boejer',]
for(station in unique(sub$station)){
  subsub = sub[sub$station == station,]
  plot(subsub$date, as.integer(as.factor(subsub$folder_name)),
       main = station)
}




station = 'NS6'
subsub = sub[sub$station == station,]
as.factor(subsub$folder_name) |> levels()
