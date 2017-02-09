#
# Calculates pairwise item odds ratios.
#

pairwiseItemOddsRatios = function (data, model) {
  startTime = proc.time()

  cat("Calculating pairwise item odds ratios for model:", toupper(model$type), "\n")
  cat("Start time:", format(Sys.time(), "%X"), "\n")

  responsesToOddsRatios = function(responses) {
    itemIndices = t(combn(ncol(responses), 2))
    itemIndicesCount = nrow(itemIndices)
    oddsRatios = numeric(itemIndicesCount)

    for (i in 1:itemIndicesCount) {
      crossTable = table(factor(responses[, itemIndices[i, 1]], levels = c(0, 1)), factor(responses[, itemIndices[i, 2]], levels = c(0, 1)))

      n00 = crossTable[1, 1]
      n01 = crossTable[1, 2]
      n10 = crossTable[2, 1]
      n11 = crossTable[2, 2]

      oddsRatios[i] = (n00 * n11) / (n01 * n10)
    }

    result = data.frame(itemIndices, oddsRatios)
    names(result) = c("item1Index", "item2Index", "observed")

    return(result)
  }

  # observed odds ratios
  oddsRatios = responsesToOddsRatios(data$responsesTable)

  # replicated odds ratios
  replicatedOddsRatios = matrix(NA, model$samplesCount, nrow(oddsRatios))

  progressBar = txtProgressBar(min = 1, max = model$samplesCount, style = 3)

  for (i in 1:model$samplesCount) {
    replicatedResponsesTable = array(0, c(data$personsCount, data$itemsCount))

    for (j in 1:data$responsesCount) {
      replicatedResponsesTable[data$responsesList[[j]]$personIndex, data$responsesList[[j]]$itemIndex] = model$replicatedResponses[i, j]
    }

    replicatedOddsRatios[i,] = responsesToOddsRatios(replicatedResponsesTable)$observed

    setTxtProgressBar(progressBar, i)
  }

  oddsRatios = cbind(oddsRatios, t(apply(replicatedOddsRatios, 2, summarizeSamples)))

  # ppp values
  oddsRatios$ppp = NA

  for (i in 1:nrow(oddsRatios)) {
    oddsRatios$ppp[i] = mean(oddsRatios$observed[i] <= replicatedOddsRatios[i,])
  }

  # calculate duration
  endTime = proc.time()
  duration = (endTime - startTime)[3]

  cat("\n")
  cat("Done!\n")
  cat("Elapsed time:", duration, "s\n")

  return (oddsRatios)
}
