## Create wind speeds to predict for (1 m/s increments)
wind_speeds = seq(0, max(dat_model$wind_speed), by = 1)

## Take means of posterior parameters for plotting
b_sin = colMeans(post$b_sin)
b_cos = colMeans(post$b_cos)
b_sin_speed = colMeans(post$b_sin_speed)
b_cos_speed = colMeans(post$b_cos_speed)
a = colMeans(post$a)
b_dist_coast = colMeans(post$b_dist_coast)
b_wind_speed = colMeans(post$b_wind_speed)
w_date = apply(post$w_date, 2, mean)
w_temp = apply(post$w_temp, 2, mean)

## Use mean distance to coast
dist_coast = mean(dat_model$distance_to_coast)

## Setup plotting area
plot(NA, xlim = range(wind_speeds), ylim = c(0, 1), 
     xlab = 'Wind speed [m/s]', ylab = 'Presence probability',
     type = 'n')

## Define arrow scaling
arrow_scaling = 0.05  # adjust as needed

## Loop through wind speeds
for (i in seq_along(wind_speeds)) {
  ws = wind_speeds[i]
  
  # Compute wind-direction coefficients at this wind speed
  sin_eff = b_sin + b_sin_speed * ws
  cos_eff = b_cos + b_cos_speed * ws
  
  # Optimal direction (radians)
  theta = (atan2(sin_eff, cos_eff) - pi/2) %% (2 * pi)
  
  # Strength of direction effect
  magnitude = sqrt(sin_eff^2 + cos_eff^2)
  
  # Arrow components (scaled)
  dx = cos(theta) * magnitude * arrow_scaling
  dy = sin(theta) * magnitude * arrow_scaling
  
  # Smooth terms
  smooth_date = sum(B_date_plot * w_date)
  smooth_temp = sum(B_temp_plot * w_temp)
  
  # Linear predictor at optimal direction
  linpred = a + 
    sin_eff * sin(theta) + cos_eff * cos(theta) +
    smooth_date + smooth_temp -
    b_dist_coast * dist_coast -
    b_wind_speed * ws
  
  prob = 1 / (1 + exp(-linpred))
  
  # Plot arrow
  arrows(x0 = ws, y0 = prob, 
         x1 = ws + dx, y1 = prob + dy,
         length = 0.05, angle = 20, col = 'black')
}






## Setup
wind_speeds = seq(0, max(dat_model$wind_speed), by = 1)
N_speed = length(wind_speeds)
N_post = length(post$a)

## Allocate
prob_mat = numeric(N_speed)
theta_vec = numeric(N_speed)
mag_vec = numeric(N_speed)

## Fixed values
dist_coast = mean(dat_model$distance_to_coast)

## Begin plot
plot(NA, xlim = range(wind_speeds), ylim = c(0, 1), 
     xlab = 'Wind speed (m/s)', ylab = 'Presence probability',
     type = 'n')

arrow_scaling = 0.05

for (j in seq_along(wind_speeds)) {
  ws = wind_speeds[j]
  
  # Store per-posterior sample values
  probs = numeric(N_post)
  thetas = numeric(N_post)
  mags = numeric(N_post)
  
  for (i in seq_len(N_post)) {
    # Wind direction effect
    sin_eff = post$b_sin[i] + post$b_sin_speed[i] * ws
    cos_eff = post$b_cos[i] + post$b_cos_speed[i] * ws
    
    theta = (atan2(sin_eff, cos_eff) - pi/2) %% (2 * pi)
    magnitude = sqrt(sin_eff^2 + cos_eff^2)
    
    # Smooth terms
    smooth_date = sum(B_date_plot * post$w_date[i, ])
    smooth_temp = sum(B_temp_plot * post$w_temp[i, ])
    
    # Linear predictor at optimal direction
    linpred = post$a[i] +
      sin_eff * sin(theta) + cos_eff * cos(theta) +
      smooth_date + smooth_temp -
      post$b_dist_coast[i] * dist_coast -
      post$b_wind_speed[i] * ws
    
    probs[i] = 1 / (1 + exp(-linpred))
    thetas[i] = theta
    mags[i] = magnitude
  }
  
  print(magnitude)
  
  # Posterior means
  prob_mean = mean(probs)
  theta_mean = mean(thetas)
  mag_mean = mean(mags)
  
  # Save
  prob_mat[j] = prob_mean
  theta_vec[j] = theta_mean
  mag_vec[j] = mag_mean
  
  # Arrow components
  dx = cos(theta_mean) * mag_mean * arrow_scaling
  dy = sin(theta_mean) * mag_mean * arrow_scaling
  
  # Plot arrow
  arrows(x0 = ws, y0 = prob_mean,
         x1 = ws + dx, y1 = prob_mean + dy,
         length = 0.05, angle = 20, col = 'black')
}
