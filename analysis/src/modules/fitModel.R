#
# Fits bayesian IRT model.
#

fitModel = function (data, modelType, warmupIterationsCount = 250, iterationsCount = 1000, chainsCount = 1) {
  startTime = proc.time()

  cat("Fitting bayesian IRT model:", toupper(modelType), "\n")
  cat("Start time:", format(Sys.time(), "%X"), "\n")

  fit = stan(paste("src/", tolower(modelType), ".stan", sep = ""),
             data = list(personsCount = data$personsCount,
                         itemsCount = data$itemsCount,
                         responsesCount = data$responsesCount,
                         responses = data$responses,
                         itemIndices = data$itemIndices,
                         personIndices = data$personIndices),
             warmup = warmupIterationsCount,
             iter = iterationsCount,
             chains = chainsCount)

  # diagnose model
  # modelShiny = as.shinystan(fit)

  # launch_shinystan(modelShiny)

  # extract all samples
  cat("Extracting parameter samples...\n")

  parametersList = rstan::extract(fit)
  samplesCount = as.integer(dim(parametersList$lp__)[1])

  # parameters
  personAbilities = parametersList$personAbilities
  itemDifficulties = parametersList$itemDifficulties
  itemDiscriminations = parametersList$itemDiscriminations
  itemGuessings = parametersList$itemGuessings
  responseProbabilities = parametersList$responseProbabilities

  # replicated parameters
  cat("Extracting replicated samples...\n")

  replicatedResponses = parametersList$replicatedResponses
  replicatedResponsesTable = array(0, c(samplesCount, data$personsCount, data$itemsCount))

  for (sampleIndex in 1:samplesCount) {
    for (i in 1:data$responsesCount) {
      replicatedResponsesTable[sampleIndex, data$personIndices[i], data$itemIndices[i]] = replicatedResponses[sampleIndex, i]
    }
  }

  # WAIC & LOO
  cat("Computing prediction accuracy...\n")

  # waic = waic(parametersList$logLikelihood)
  loo = loo(parametersList$logLikelihood)

  # parameter summaries (mean, 2.5 %, 97.5 %)
  cat("Computing parameter estimates...\n")

  parameterEstimatesList = list()

  parameterEstimatesList$personAbilities = t(apply(parametersList$personAbilities, 2, summarizeSamples))
  parameterEstimatesList$itemDifficulties = t(apply(parametersList$itemDifficulties, 2, summarizeSamples))
  parameterEstimatesList$itemDiscriminations = NULL
  parameterEstimatesList$itemGuessings = NULL
  parameterEstimatesList$responseProbabilities = t(apply(parametersList$responseProbabilities, 2, summarizeSamples))

  if (!is.null(itemDiscriminations)) {
    parameterEstimatesList$itemDiscriminations = t(apply(parametersList$itemDiscriminations, 2, summarizeSamples))
  } else {
    if (tolower(modelType) == "1pl") {
      parameterEstimatesList$itemDiscriminations = matrix(1, data$itemsCount, 3)
    }
  }

  if (!is.null(itemGuessings)) {
    parameterEstimatesList$itemGuessings = t(apply(parametersList$itemGuessings, 2, summarizeSamples))
  } else {
    if (tolower(modelType) == "1pl" | tolower(modelType) == "2pl") {
      parameterEstimatesList$itemGuessings = matrix(0, data$itemsCount, 3)
    }
  }

  # calculate duration
  endTime = proc.time()
  duration = (endTime - startTime)[3]

  cat("Done!\n")
  cat("Elapsed time:", duration, "s\n")

  return (list(type = toupper(modelType),
               fit = fit,
               samplesCount = samplesCount,
               parametersList = parametersList,
               personAbilities = personAbilities,
               itemDifficulties = itemDifficulties,
               itemDiscriminations = itemDiscriminations,
               itemGuessings = itemGuessings,
               responseProbabilities = responseProbabilities,
               replicatedResponses = replicatedResponses,
               replicatedResponsesTable = replicatedResponsesTable,
               parameterEstimatesList = parameterEstimatesList,
               # waic = waic,
               loo = loo,
               duration = duration))
}
