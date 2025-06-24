# Helper function to get prior density for a parameter
get_prior_density = function(name, values, xx) {
  if (name == 'a') return(dnorm(xx, -2, 2))
  if (name == 'b') return(dexp(xx, 10) * (xx > 0))
  if (name == 'sigma_type') return(dexp(xx, 1) * (xx > 0))
  if (name == 'sigma_station') return(dexp(xx, 1) * (xx > 0))
  return(dnorm(xx, 0, 1))  # default prior
}

# Flatten the post list into named samples like w[1], w[2], ...
flatten_post = function(post) {
  flat = list()
  for (name in names(post)) {
    param = post[[name]]
    if (is.vector(param)) {
      flat[[name]] = as.numeric(param)
    } else if (is.matrix(param)) {
      for (i in seq_len(ncol(param))) {
        flat[[sprintf('%s[%d]', name, i)]] = param[, i]
      }
    }
  }
  flat
}

# Flatten posterior samples
flat_post = flatten_post(post)
param_names = names(flat_post)

# Set plotting layout
par(mfrow = c(3, 3), mar = c(2, 2, 2, 1))

# Plot loop with pagination
for (i in seq_along(param_names)) {
  param = param_names[i]
  samples = flat_post[[param]]
  xlims = range(samples)
  xx = seq(xlims[1] - 1, xlims[2] + 1, length.out = 200)
  
  base_name = sub("\\[.*", "", param)
  prior_density = get_prior_density(base_name, samples, xx)
  
  # Plot prior
  plot(xx, prior_density, type = 'l', col = 'lightblue', lwd = 3,
       main = param, xlab = '', ylab = '',
       ylim = c(0, max(prior_density, density(samples)$y) * 1.2))
  
  # Overlay posterior
  lines(density(samples), col = 'black', lwd = 2)
  
  # Start new page every 9 plots
  if (i %% 9 == 0 && i < length(param_names)) {
    readline(prompt = 'Press [Enter] for next page...')
    par(mfrow = c(3, 3), mar = c(2, 2, 2, 1))
  }
}
