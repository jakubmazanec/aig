#
# Calculates discrepancies between parameter estimates and true values.
#

getModelDiscrepancies = function (data, model) {
  startTime = proc.time()

  cat("Calculating parameter discrepancies for model:", toupper(model$type), "\n")
  cat("Start time:", format(Sys.time(), "%X"), "\n")

  personAbilitiesSummary = NULL
  itemDifficultiesSummary = NULL
  itemDiscriminationsSummary = NULL
  itemGuessingsSummary = NULL
  responseProbabilitiesSummary = NULL

  personAbilitiesSummary = data.frame(personIndex = 1:data$personsCount,
                                      mean = model$parameterEstimatesList$personAbilities[, 1] - data$personAbilities,
                                      hdiLowerBound = model$parameterEstimatesList$personAbilities[, 2] - data$personAbilities,
                                      hdiUpperBound = model$parameterEstimatesList$personAbilities[, 3] - data$personAbilities)

  itemDifficultiesSummary = data.frame(itemIndex = 1:data$itemsCount,
                                       mean = model$parameterEstimatesList$itemDifficulties[, 1] - data$itemDifficulties,
                                       hdiLowerBound = model$parameterEstimatesList$itemDifficulties[, 2] - data$itemDifficulties,
                                       hdiUpperBound = model$parameterEstimatesList$itemDifficulties[, 3] - data$itemDifficulties)

  if (!is.null(model$parameterEstimatesList$itemDiscriminations)) {
    itemDiscriminationsSummary = data.frame(itemIndex = 1:data$itemsCount,
                                            mean = model$parameterEstimatesList$itemDiscriminations[, 1] - data$itemDiscriminations,
                                            hdiLowerBound = model$parameterEstimatesList$itemDiscriminations[, 2] - data$itemDiscriminations,
                                            hdiUpperBound = model$parameterEstimatesList$itemDiscriminations[, 3] - data$itemDiscriminations)
  }

  if (!is.null(model$parameterEstimatesList$itemGuessings)) {
    itemGuessingsSummary = data.frame(itemIndex = 1:data$itemsCount,
                                      mean = model$parameterEstimatesList$itemGuessings[, 1] - data$itemGuessings,
                                      hdiLowerBound = model$parameterEstimatesList$itemGuessings[, 2] - data$itemGuessings,
                                      hdiUpperBound = model$parameterEstimatesList$itemGuessings[, 3] - data$itemGuessings)
  }

  responseProbabilitiesSummary = data.frame(itemIndex = 1:data$responsesCount,
                                       mean = model$parameterEstimatesList$responseProbabilities[, 1] - data$responseProbabilities,
                                       hdiLowerBound = model$parameterEstimatesList$responseProbabilities[, 2] - data$responseProbabilities,
                                       hdiUpperBound = model$parameterEstimatesList$responseProbabilities[, 3] - data$responseProbabilities)

  # calculate duration
  endTime = proc.time()
  duration = (endTime - startTime)[3]

  cat("Done!\n")
  cat("Elapsed time:", duration, "s\n")

  return (list(personAbilitiesSummary = personAbilitiesSummary,
               itemDifficultiesSummary = itemDifficultiesSummary,
               itemDiscriminationsSummary = itemDiscriminationsSummary,
               itemGuessingsSummary = itemGuessingsSummary,
               responseProbabilitiesSummary = responseProbabilitiesSummary))
}
