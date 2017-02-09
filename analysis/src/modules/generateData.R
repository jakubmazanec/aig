source("src/modules/createScoresTable.R")
source("src/modules/generateResponses.R")


#
# Generates dataset from persons and items.
#

generateData = function (personsList, itemsList, model, name, errorSd = 0) {
  personsCount = length(personsList)
  itemsCount = length(itemsList)

  responsesList = generateResponses(personsList, itemsList, model, errorSd)

  responsesCount = length(responsesList)
  responses = sapply(responsesList, function(x) { x$response })
  itemIndices = sapply(responsesList, function(x) { x$itemIndex })
  personIndices = sapply(responsesList, function(x) { x$personIndex })

  responsesTable = array(0, c(personsCount, itemsCount))

  for (i in 1:responsesCount) {
    responsesTable[responsesList[[i]]$personIndex, responsesList[[i]]$itemIndex] = responsesList[[i]]$response
  }

  scoresTablesList = createScoresTable(responsesTable)
  correctScoresTable = scoresTablesList$correctScores
  totalScoresTable = scoresTablesList$totalScores

  personScores = tapply(responses, personIndices, sum)
  itemScores = tapply(responses, itemIndices, sum)

  personAbilities = NULL
  itemDifficulties = NULL
  itemDiscriminations = NULL
  itemGuessings = NULL
  responseProbabilities = sapply(responsesList, function(x) { x$probability })

  if (tolower(model) == "1pl") {
    personAbilities = sapply(personsList, function(x) { x$ability })
    itemDifficulties = sapply(itemsList, function(x) { x$difficulty })
    itemDiscriminations = rep(1, itemsCount)
    itemGuessings = rep(0, itemsCount)
  } else if (tolower(model) == "2pl") {
    personAbilities = sapply(personsList, function(x) { x$ability })
    itemDifficulties = sapply(itemsList, function(x) { x$difficulty })
    itemDiscriminations = sapply(itemsList, function(x) { x$discrimination })
    itemGuessings = rep(0, itemsCount)
  } else if (tolower(model) == "3pl") {
    personAbilities = sapply(personsList, function(x) { x$ability })
    itemDifficulties = sapply(itemsList, function(x) { x$difficulty })
    itemDiscriminations = sapply(itemsList, function(x) { x$discrimination })
    itemGuessings = sapply(itemsList, function(x) { x$guessing })
  } else if (tolower(model) == "1pl-pot") {
    personAbilities = sapply(personsList, function(x) {
      a = sum(x$domain1Abilities)
      b = sum(x$domain2Abilities)
      c = sum(x$domain3Abilities)

      result = log(exp(a + b + c) / (1 + exp(a + b) + exp(a + c) + exp(b + c) + exp(a) + exp(b) + exp(c)))

      return (result)
    })
    itemDifficulties = sapply(itemsList, function(x) {
      a = x$domain1Difficulty
      b = x$domain1Difficulty
      c = x$domain1Difficulty

      result = log(exp(a + b + c) / (1 + exp(a + b) + exp(a + c) + exp(b + c) + exp(a) + exp(b) + exp(c)))

      return (result)
    })
    itemDiscriminations = rep(1, itemsCount)
    itemGuessings = rep(0, itemsCount)
  } else  if (tolower(model) == "2pl-pot") {
    personAbilities = sapply(personsList, function(x) {
      a = sum(x$domain1Abilities)
      b = sum(x$domain2Abilities)
      c = sum(x$domain3Abilities)

      result = -0.5 * log(exp(-a - b - c) * (1 + exp(a + b) + exp(a + c) + exp(b + c) + exp(a) + exp(b) + exp(c))) + 0.4 * pi

      return (result)
    })
    itemDifficulties = sapply(itemsList, function(x) {
      a = x$domain1Difficulty
      b = x$domain2Difficulty
      c = x$domain3Difficulty

      result = log(exp(a + b + c) / (1 + exp(a + b) + exp(a + c) + exp(b + c) + exp(a) + exp(b) + exp(c)))

      return (result)
    })
    itemDiscriminations = sapply(itemsList, function(x) {
      a = x$domain1Discrimination
      b = x$domain2Discrimination
      c = x$domain3Discrimination

      result = log(exp(a + b + c) / (1 + exp(a + b) + exp(a + c) + exp(b + c) + exp(a) + exp(b) + exp(c)))

      return (result)
    })
    itemGuessings = rep(0, itemsCount)
  }

  return (list(name = name,
               personsCount = personsCount,
               itemsCount = itemsCount,
               responsesCount = responsesCount,
               personsList = personsList,
               itemsList = itemsList,
               responsesList = responsesList,
               responsesTable = responsesTable,
               responses = responses,
               itemIndices = itemIndices,
               personIndices = personIndices,
               personScores = personScores,
               itemScores = itemScores,
               scoresTablesList = scoresTablesList,
               correctScoresTable = correctScoresTable,
               totalScoresTable = totalScoresTable,
               personAbilities = personAbilities,
               itemDifficulties = itemDifficulties,
               itemDiscriminations = itemDiscriminations,
               itemGuessings = itemGuessings,
               responseProbabilities = responseProbabilities))
}
