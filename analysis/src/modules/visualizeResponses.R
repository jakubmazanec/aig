#
# Plots visualizations of responses.
#

visualizeResponses = function(responses) {
  probabilities = sapply(responses, function(x) { x$probability })

  hist(probabilities, breaks = 20)

  # hist(sapply(rawResponses, function(x) { x$probability }), breaks = 40)
  # hist(sapply(rawResponses, function(x) { x$domain1Probability }), breaks = 40)
  # hist(sapply(rawResponses, function(x) { x$domain2Probability }), breaks = 40)
  # hist(sapply(rawResponses, function(x) { x$domain3Probability }), breaks = 40)
  #
  # hist(sapply(rawResponses, function(x) { x$domain1Sum }), breaks = 40)
  # hist(sapply(rawResponses, function(x) { x$domain2Sum }), breaks = 40)
  # hist(sapply(rawResponses, function(x) { x$domain3Sum }), breaks = 40)
  #
  # plot(sapply(rawResponses, function(x) { x$domain1Sum }), sapply(rawResponses, function(x) { x$domain1Probability }))
  # plot(sapply(rawResponses, function(x) { x$domain2Sum }), sapply(rawResponses, function(x) { x$domain2Probability }))
  # plot(sapply(rawResponses, function(x) { x$domain3Sum }), sapply(rawResponses, function(x) { x$domain3Probability }))

  return (NULL)
}
