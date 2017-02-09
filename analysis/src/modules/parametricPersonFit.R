#
# Calculates parametric person fit statistics.
#

parametricPersonFit = function (data, model) {
  startTime = proc.time()

  cat("Calculating parametric person fit statistics for model:", toupper(model$type), "\n")
  cat("Start time:", format(Sys.time(), "%X"), "\n")

  W = function (responses, probabilities) {
    return (sum((responses - probabilities) ^ 2) / sum(probabilities * (1 - probabilities)))
  }

  # person fit summary table
  personFitSummary = data.frame(personIndices = 1:data$personsCount,
                                observedMean = rep(0, data$personsCount),
                                observedHdiLowerBound = rep(0, data$personsCount),
                                observedHdiUpperBound = rep(0, data$personsCount),
                                replicatedMean = rep(0, data$personsCount),
                                replicatedHdiLowerBound = rep(0, data$personsCount),
                                replicatedHdiUpperBound = rep(0, data$personsCount),
                                ppp = rep(0, data$personsCount))

  cat("Calculating observed statistics...\n")

  # observed responses per person
  observedPersonResponses = do.call(rbind, tapply(data$responses, data$personIndices, function (x) { return (x) }))

  # create samples of observed person fit
  observedPersonFit = array(0, c(model$samplesCount, data$personsCount))

  for (i in 1:model$samplesCount) {
    for (j in 1:data$personsCount) {
      if (tolower(model$type) == "1pl") {
        observedPersonFit[i, j] = W(observedPersonResponses[j, ], inverseLogit(model$personAbilities[i, j] - model$itemDifficulties[i,]))
      } else if (tolower(model$type) == "2pl") {
        observedPersonFit[i, j] = W(observedPersonResponses[j, ], inverseLogit(model$itemDiscriminations[i,] * (model$personAbilities[i, j] - model$itemDifficulties[i,])))
      } else if (tolower(model$type) == "3pl") {
        observedPersonFit[i, j] = W(observedPersonResponses[j, ], model$itemGuessings[i,] + (1 - model$itemGuessings[i,]) * inverseLogit(model$itemDiscriminations[i,] * (model$personAbilities[i, j] - model$itemDifficulties[i,])))
      }
    }
  }

  personFitSummary[, 2:4] = t(apply(observedPersonFit, 2, summarizeSamples))

  cat("Calculating replicated statistics...\n")

  # create samples of replicated person fit
  replicatedPersonFit = array(0, c(model$samplesCount, data$personsCount))

  for (i in 1:model$samplesCount) {
    replicatedPersonResponses = do.call(rbind, tapply(model$replicatedResponses[i, ], data$personIndices, function (x) { return (x) }))

    for (j in 1:data$personsCount) {
      if (tolower(model$type) == "1pl") {
        replicatedPersonFit[i, j] = W(replicatedPersonResponses[j, ], inverseLogit(model$personAbilities[i, j] - model$itemDifficulties[i,]))
      } else if (tolower(model$type) == "2pl") {
        replicatedPersonFit[i, j] = W(replicatedPersonResponses[j, ], inverseLogit(model$itemDiscriminations[i,] * (model$personAbilities[i, j] - model$itemDifficulties[i,])))
      } else if (tolower(model$type) == "3pl") {
        replicatedPersonFit[i, j] = W(replicatedPersonResponses[j, ], model$itemGuessings[i,] + (1 - model$itemGuessings[i,]) * inverseLogit(model$itemDiscriminations[i,] * (model$personAbilities[i, j] - model$itemDifficulties[i,])))
      }
    }
  }

  personFitSummary[, 5:7] = t(apply(replicatedPersonFit, 2, summarizeSamples))

  # calculate ppp-values
  for (i in 1:data$personsCount) {
    personFitSummary[i, 8] = mean(replicatedPersonFit[, i] >= observedPersonFit[, i])
  }

  # calculate duration
  endTime = proc.time()
  duration = (endTime - startTime)[3]

  cat("Done!\n")
  cat("Elapsed time:", duration, "s\n")

  return (personFitSummary)
}
