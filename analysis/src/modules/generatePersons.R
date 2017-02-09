#
# Generates persons.
#

generatePersons = function (personsCount) {
  persons = list()

  # 3 domains with 3 processes
  for (i in 1:personsCount) {
    abilities = mvrnorm(1000, rep(0, 9), matrix(c(1,    0.5,  0.5,  0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
                                                  0.5,  1,    0.5,  0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
                                                  0.5,  0.5,  1,    0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
                                                  0.25, 0.25, 0.25, 1,    0.5,  0.5,  0.25, 0.25, 0.25,
                                                  0.25, 0.25, 0.25, 0.5,  1,    0.5,  0.25, 0.25, 0.25,
                                                  0.25, 0.25, 0.25, 0.5,  0.5,  1,    0.25, 0.25, 0.25,
                                                  0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 1,    0.5,  0.5,
                                                  0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.5,  1,    0.5,
                                                  0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.5,  0.5,  1), 9, 9, byrow = TRUE))
    domain1Abilities = abilities[1:3]
    domain2Abilities = abilities[4:6]
    domain3Abilities = abilities[7:9]
    domain1MeanAbility = mean(domain1Abilities)
    domain2MeanAbility = mean(domain2Abilities)
    domain3MeanAbility = mean(domain3Abilities)

    persons[[i]] = list("index" = i,
                        "ability" = domain3Abilities[1],
                        "domain1Abilities" = domain1Abilities,
                        "domain2Abilities" = domain2Abilities,
                        "domain3Abilities" = domain3Abilities,
                        "domain1MeanAbility" = domain1MeanAbility,
                        "domain2MeanAbility" = domain2MeanAbility,
                        "domain3MeanAbility" = domain3MeanAbility,
                        "meanAbility" = mean(c(domain1MeanAbility, domain2MeanAbility, domain3MeanAbility)),
                        "lowestAbility" = min(domain1MeanAbility, domain2MeanAbility, domain3MeanAbility),
                        "highestAbility" = max(domain1MeanAbility, domain2MeanAbility, domain3MeanAbility))
  }

  return (persons)
}
