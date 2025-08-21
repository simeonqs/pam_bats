sorter = order(dat_model$julian_date)
nights = dat_model$julian_date[sorter]

# Identify nights with wind direction between 45 and 135 degrees
wind_shade = dat_model$wind_direction >= 45 &
  dat_model$wind_direction <= 135
water_shade = dat_model$precip > 1

# Find contiguous regions of TRUE values for shading
shade_ranges = split(nights[wind_shade], 
                     cumsum(c(TRUE, diff(wind_shade) != 0))[wind_shade])
shade_starts = sapply(shade_ranges, min)
shade_ends = sapply(shade_ranges, max)

# Plot data
plot(nights, 
     dat_model$detection[sorter] - rnorm(nrow(dat_model), 0.2, 0.05),
     xaxt = 'n', yaxt = 'n',
     pch = trans_subset[dat_model$subset[sorter]], 
     col = c('#c39bd3', # 2023 = purple
             '#f8c471', # 2024 = yellow
             '#d5dbdb'  # 2025 = grey (not included)
     )[dat_model$year[sorter]],
     cex = 0.8,
     xlab = 'Night of year', ylab = 'Probability presence per night')

# Add shaded rectangles for NE-SE wind nights
for (i in seq_len(nrow(dat_model))) {
  if(wind_shade[i]) lines(rep(dat_model$julian_date[i], 2), c(0, 0.8), col = 'grey', lwd = 2)
}

# Add shaded rectangles for watery nights
for (i in seq_len(nrow(dat_model))) {
  if(water_shade[i]) lines(rep(dat_model$julian_date[i], 2), 
                           c(0, 0.5), col = 'blue', lwd = 2)
}

# Axes and baseline
axis_dates = as.POSIXlt(c('2024-08-01', '2024-08-15', '2024-09-01', 
                          '2024-09-15', '2024-10-01', '2024-10-15'))$yday + 1
axis(1, axis_dates, c('Aug 1st', 'Aug 15th', 'Sep 1st', 'Sep 15th', 
                      'Oct 1st', 'Oct 15th'))
axis(2, c(0, 0.2, 0.4, 0.6, 0.8))
abline(h = 0, lty = 2, lwd = 2)

shade(PI_pred, nights , col = '#AED6F1')
lines(nights, mean_pred, lwd = 3, col = '#1B4F72')

