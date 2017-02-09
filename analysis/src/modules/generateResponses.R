#
# Generates responses.
#

generateResponses = function (persons, items, model, errorSd = 0) {
  personsCount = length(persons)
  itemsCount = length(items)

  responses = list()

  if (tolower(model) == "1pl") {
    for (i in 1:personsCount) {
      for (j in 1:itemsCount) {
        sum = rnorm(1, 0, errorSd) + (persons[[i]]$ability - items[[j]]$difficulty)

        probability = exp(sum) / (1 + exp(sum))

        responses[[length(responses) + 1]] = list("index" = length(responses) + 1,
                                                  "probability" = probability,
                                                  "sum" = sum,
                                                  "response" = rbinom(1, 1, probability),
                                                  "personIndex" = i,
                                                  "itemIndex" = j)
      }
    }
  } else if (tolower(model) == "2pl") {
    for (i in 1:personsCount) {
      for (j in 1:itemsCount) {
        sum = rnorm(1, 0, errorSd) + items[[j]]$discrimination * (persons[[i]]$ability - items[[j]]$difficulty)

        probability = exp(sum) / (1 + exp(sum))

        responses[[length(responses) + 1]] = list("index" = length(responses) + 1,
                                                  "probability" = probability,
                                                  "sum" = sum,
                                                  "response" = rbinom(1, 1, probability),
                                                  "personIndex" = i,
                                                  "itemIndex" = j)
      }
    }
  } else if (tolower(model) == "3pl") {
    for (i in 1:personsCount) {
      for (j in 1:itemsCount) {
        sum = rnorm(1, 0, errorSd) + items[[j]]$discrimination * (persons[[i]]$ability - items[[j]]$difficulty)

        probability = items[[j]]$guessing + (1 - items[[j]]$guessing) * (exp(sum) / (1 + exp(sum)))

        responses[[length(responses) + 1]] = list("index" = length(responses) + 1,
                                                  "probability" = probability,
                                                  "sum" = sum,
                                                  "response" = rbinom(1, 1, probability),
                                                  "personIndex" = i,
                                                  "itemIndex" = j)
      }
    }
  } else if (tolower(model) == "1pl-pot") {
    for (i in 1:personsCount) {
      for (j in 1:itemsCount) {
        domain1Sum = rnorm(1, 0, errorSd) + (persons[[i]]$domain1Abilities[1] - items[[j]]$domain1Difficulty) + (persons[[i]]$domain1Abilities[2] - items[[j]]$domain1Difficulty) + (persons[[i]]$domain1Abilities[3] - items[[j]]$domain1Difficulty)
        domain2Sum = rnorm(1, 0, errorSd) + (persons[[i]]$domain2Abilities[1] - items[[j]]$domain2Difficulty) + (persons[[i]]$domain2Abilities[2] - items[[j]]$domain2Difficulty) + (persons[[i]]$domain2Abilities[3] - items[[j]]$domain2Difficulty)
        domain3Sum = rnorm(1, 0, errorSd) + (persons[[i]]$domain3Abilities[1] - items[[j]]$domain3Difficulty) + (persons[[i]]$domain3Abilities[2] - items[[j]]$domain3Difficulty) + (persons[[i]]$domain3Abilities[3] - items[[j]]$domain3Difficulty)

        domain1Probability = exp(domain1Sum) / (1 + exp(domain1Sum))
        domain2Probability = exp(domain2Sum) / (1 + exp(domain2Sum))
        domain3Probability = exp(domain3Sum) / (1 + exp(domain3Sum))

        probability = domain1Probability * domain2Probability * domain3Probability

        responses[[length(responses) + 1]] = list("index" = length(responses) + 1,
                                                  "probability" = probability,
                                                  "domain1Sum" = domain1Sum,
                                                  "domain2Sum" = domain2Sum,
                                                  "domain3Sum" = domain3Sum,
                                                  "domain1Probability" = domain1Probability,
                                                  "domain2Probability" = domain2Probability,
                                                  "domain3Probability" = domain3Probability,
                                                  "response" = rbinom(1, 1, probability),
                                                  "personIndex" = i,
                                                  "itemIndex" = j)
      }
    }
  } else if (tolower(model) == "2pl-pot") {
    for (i in 1:personsCount) {
      for (j in 1:itemsCount) {
        domain1Sum = rnorm(1, 0, errorSd) + items[[j]]$domain1Discrimination * (persons[[i]]$domain1Abilities[1] - items[[j]]$domain1Difficulty) + items[[j]]$domain1Discrimination * (persons[[i]]$domain1Abilities[2] - items[[j]]$domain1Difficulty) + items[[j]]$domain1Discrimination * (persons[[i]]$domain1Abilities[3] - items[[j]]$domain1Difficulty)
        domain2Sum = rnorm(1, 0, errorSd) + items[[j]]$domain2Discrimination * (persons[[i]]$domain2Abilities[1] - items[[j]]$domain2Difficulty) + items[[j]]$domain2Discrimination * (persons[[i]]$domain2Abilities[2] - items[[j]]$domain2Difficulty) + items[[j]]$domain2Discrimination * (persons[[i]]$domain2Abilities[3] - items[[j]]$domain2Difficulty)
        domain3Sum = rnorm(1, 0, errorSd) + items[[j]]$domain3Discrimination * (persons[[i]]$domain3Abilities[1] - items[[j]]$domain3Difficulty) + items[[j]]$domain3Discrimination * (persons[[i]]$domain3Abilities[2] - items[[j]]$domain3Difficulty) + items[[j]]$domain3Discrimination * (persons[[i]]$domain3Abilities[3] - items[[j]]$domain3Difficulty)

        domain1Probability = exp(domain1Sum) / (1 + exp(domain1Sum))
        domain2Probability = exp(domain2Sum) / (1 + exp(domain2Sum))
        domain3Probability = exp(domain3Sum) / (1 + exp(domain3Sum))

        probability = domain1Probability * domain2Probability * domain3Probability

        responses[[length(responses) + 1]] = list("index" = length(responses) + 1,
                                                  "probability" = probability,
                                                  "domain1Sum" = domain1Sum,
                                                  "domain2Sum" = domain2Sum,
                                                  "domain3Sum" = domain3Sum,
                                                  "domain1Probability" = domain1Probability,
                                                  "domain2Probability" = domain2Probability,
                                                  "domain3Probability" = domain3Probability,
                                                  "response" = rbinom(1, 1, probability),
                                                  "personIndex" = i,
                                                  "itemIndex" = j)
      }
    }
  }

  return (responses)
}
