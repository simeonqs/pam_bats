removed_dates = data.frame(
  station = 'HR3-4',
  date = as.Date(c('2023-06-03', '2023-09-17', '2023-09-18', '2023-12-12', 
                   '2023-12-13', '2023-12-14'))
)
removed_dates = rbind(removed_dates, data.frame(
  station = 'HR3-6',
  date = as.Date(c('2023-12-22'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS06',
  date = c(seq(as.Date('2023-09-17'), as.Date('2023-09-30'), by = 'day'),
           seq(as.Date('2023-12-17'), as.Date('2023-12-20'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS08',
  date = c(as.Date('2023-05-31'), as.Date('2023-06-01'),
           seq(as.Date('2023-09-30'), as.Date('2023-11-07'), by = 'day'),
           as.Date('2023-12-27'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS12',
  date = c(seq(as.Date('2023-09-21'), as.Date('2023-11-03'), by = 'day'),
           seq(as.Date('2023-06-04'), as.Date('2023-06-06 '), by = 'day'),
           as.Date('2023-12-29'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS13',
  date = c(seq(as.Date('2023-05-26'), as.Date('2023-05-29'), by = 'day'),
           seq(as.Date('2023-12-26'), as.Date('2024-01-08'), by = 'day'),
           as.Date('2024-03-27'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS14',
  date = c(seq(as.Date('2023-06-06'), as.Date('2023-06-08'), by = 'day'),
           seq(as.Date('2023-09-05'), as.Date('2023-09-06'), by = 'day'),
           seq(as.Date('2023-12-27'), as.Date('2024-01-07'), by = 'day'),
           as.Date('2024-04-05'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS16',
  date = c(seq(as.Date('2023-06-08'), as.Date('2023-06-11'), by = 'day'),
           seq(as.Date('2023-12-25'), as.Date('2024-01-23'), by = 'day'),
           as.Date('2024-04-01'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS19',
  date = c(seq(as.Date('2023-05-25'), as.Date('2023-05-26'), by = 'day'),
           seq(as.Date('2023-09-23'), as.Date('2023-10-28'), by = 'day'),
           seq(as.Date('2023-12-23'), as.Date('2023-12-30'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS20',
  date = c(seq(as.Date('2023-06-06'), as.Date('2023-06-08 '), by = 'day'),
           seq(as.Date('2023-09-26'), as.Date('2023-10-15'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS21',
  date = c(seq(as.Date('2023-06-18'), as.Date('2023-06-19'), by = 'day'),
           seq(as.Date('2023-12-26'), as.Date('2024-01-06'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS24',
  date = c(seq(as.Date('2023-06-03'), as.Date('2023-06-04'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS25',
  date = c(seq(as.Date('2023-06-08'), as.Date('2023-06-12'), by = 'day'),
           seq(as.Date('2023-12-27'), as.Date('2023-12-29'), by = 'day'),
           as.Date('2024-04-01'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS26',
  date = c(as.Date('2023-06-05'),
           seq(as.Date('2023-09-20'), as.Date('2023-09-21'), by = 'day'),
           as.Date('2023-12-10'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS27',
  date = c(as.Date('2023-06-03'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS28',
  date = c(seq(as.Date('2023-05-31'), as.Date('2023-06-01'), by = 'day'),
           as.Date('2024-04-01'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS29',
  date = c(seq(as.Date('2023-05-30'), as.Date('2023-06-05'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS30',
  date = c(seq(as.Date('2023-09-24'), as.Date('2023-10-23'), by = 'day'),
           seq(as.Date('2023-12-26'), as.Date('2023-12-28'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS31',
  date = c(seq(as.Date('2023-06-07'), as.Date('2023-06-10'), by = 'day'),
           seq(as.Date('2023-09-21'), as.Date('2023-11-01'), by = 'day'),
           seq(as.Date('2023-12-28'), as.Date('2023-12-29'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS32',
  date = c(as.Date('2023-05-09'),
           seq(as.Date('2023-11-30'), as.Date('2023-12-04'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS33',
  date = c(as.Date('2023-05-29'),
           seq(as.Date('2023-09-21'), as.Date('2023-09-24'), by = 'day'),
           seq(as.Date('2023-12-22'), as.Date('2023-12-26'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS34',
  date = c(seq(as.Date('2023-06-08'), as.Date('2023-06-11'), by = 'day'),
           seq(as.Date('2023-09-25'), as.Date('2023-11-17'), by = 'day'),
           seq(as.Date('2023-12-23'), as.Date('2023-12-25'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'NS35',
  date = c(seq(as.Date('2023-06-06'), as.Date('2023-06-09'), by = 'day'),
           seq(as.Date('2023-09-25'), as.Date('2023-11-05'), by = 'day'),
           seq(as.Date('2023-12-28'), as.Date('2024-01-04'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'A01',
  date = c(as.Date('2023-06-19'),
           seq(as.Date('2023-11-06'), as.Date('2023-11-12'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'A05',
  date = c(seq(as.Date('2023-06-28'), as.Date('2023-06-29'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'A06',
  date = c(seq(as.Date('2023-06-27'), as.Date('2023-06-29'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'F03',
  date = c(seq(as.Date('2023-11-15'), as.Date('2023-11-17'), by = 'day'))
))
removed_dates = rbind(removed_dates, data.frame(
  station = 'J01',
  date = c(seq(as.Date('2023-07-13'), as.Date('2023-07-14'), by = 'day'))
))