#
# Computes HDI.
#

hdi = function(samples , credibleMass = 0.95) {
  return (quantile(samples, c((1 - credibleMass) / 2, 1 - (1 - credibleMass) / 2), type = 8, na.rm = TRUE))
}
