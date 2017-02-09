#
# Calculates person biserial correlations.
#

personBiserialCorrelations = function (data, model) {
  startTime = proc.time()

  cat("Calculating person biserial correlations for model:", toupper(model$type), "\n")
  cat("Start time:", format(Sys.time(), "%X"), "\n")

  personBiserialCorrelation = function (responsesTable) {
    personsCount = nrow(responsesTable)
    itemCorrectResponseProportions = colMeans(responsesTable)
    result = rep(0, personsCount)

    for (personIndex in 1:personsCount) {
      if (sd(responsesTable[personIndex,]) == 0) {
        result[personIndex] = NA
      } else {
        result[personIndex] = cor(responsesTable[personIndex,], itemCorrectResponseProportions)
      }
    }

    return (result)
  }


  result = data.frame(personIndex = 1:data$personsCount,
                                          observed = personBiserialCorrelation(data$responsesTable),
                                          replicatedMean = rep(0, data$personsCount),
                                          replicatedHdiLowerBound = rep(0, data$personsCount),
                                          replicatedHdiUpperBound = rep(0, data$personsCount))

  replicatedCorrelations = array(0, c(model$samplesCount, data$personsCount))



  for (sampleIndex in 1:model$samplesCount) {
    replicatedCorrelations[sampleIndex, ] = personBiserialCorrelation(model$replicatedResponsesTable[sampleIndex,,])
  }

  result[, 3:5] = t(apply(replicatedCorrelations, 2, summarizeSamples))

  result$ppp = NA

  for (i in 1:nrow(result)) {
    if (is.finite(result$observed[i])) {
      result$ppp[i] = mean(replicatedCorrelations[, i] >= result$observed[i], na.rm = TRUE)
    } else {
      result$ppp[i] = NA
    }
  }

  # calculate duration
  endTime = proc.time()
  duration = (endTime - startTime)[3]

  cat("Done!\n")
  cat("Elapsed time:", duration, "s\n")

  return (result)
}
