#
# Logistic function.
#

inverseLogit = function (value) {
  return (exp(value) / (1 + exp(value)))
}
