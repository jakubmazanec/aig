#
# Plots visualizations of items.
#

visualizeItems = function(items) {
  difficulties = sapply(items, function(x) { x$difficulty })
  domain1Difficulties = sapply(items, function(x) { x$domain1Difficulty })
  domain2Difficulties = sapply(items, function(x) { x$domain2Difficulty })
  domain3Difficulties = sapply(items, function(x) { x$domain3Difficulty })

  discriminations = sapply(items, function(x) { x$discrimination })
  domain1Discriminations = sapply(items, function(x) { x$domain1Discrimination })
  domain2Discriminations = sapply(items, function(x) { x$domain2Discrimination })
  domain3Discriminations = sapply(items, function(x) { x$domain3Discrimination })

  hist(difficulties, breaks = 20)
  hist(domain1Difficulties, breaks = 20)
  hist(domain2Difficulties, breaks = 20)
  hist(domain3Difficulties, breaks = 20)

  hist(discriminations, breaks = 20)
  hist(domain1Discriminations, breaks = 20)
  hist(domain2Discriminations, breaks = 20)
  hist(domain3Discriminations, breaks = 20)

  return (NULL)
}
