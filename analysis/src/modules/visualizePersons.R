#
# Plots visualizations of persons.
#

visualizePersons = function(persons) {
  abilities = sapply(persons, function(x) { x$ability })
  meanAbilities = sapply(persons, function(x) { x$meanAbility })
  lowestAbilities = sapply(persons, function(x) { x$lowestAbility })
  highestAbilities = sapply(persons, function(x) { x$highestAbility })
  domain1MeanAbilities = sapply(persons, function(x) { x$domain1MeanAbility })
  domain2MeanAbilities = sapply(persons, function(x) { x$domain2MeanAbility })
  domain3MeanAbilities = sapply(persons, function(x) { x$domain3MeanAbility })

  hist(abilities, breaks = 20)
  hist(meanAbilities, breaks = 20)
  hist(lowestAbilities, breaks = 20)
  hist(highestAbilities, breaks = 20)
  hist(domain1MeanAbilities, breaks = 20)
  hist(domain2MeanAbilities, breaks = 20)
  hist(domain3MeanAbilities, breaks = 20)

  return (NULL)
}
