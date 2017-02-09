#
# Calculates distribution of person scores.
#

scoresDistribution = function (data, model) {
  startTime = proc.time()

  cat("Calculating scores distribution for model:", toupper(model$type), "\n")
  cat("Start time:", format(Sys.time(), "%X"), "\n")

  # person scores count
  personScoresCount = data.frame(table(factor(data$personScores, levels = 0:data$itemsCount)))
  personScoresCount[, 1] = as.numeric(personScoresCount[, 1])

  # replicated scores count samples
  replicatedPersonScores = t(apply(model$replicatedResponses, 1, function(x) tapply(x, data$personIndices, sum)))
  replicatedPersonScoresCount = t(apply(replicatedPersonScores, 1, function(x) table(factor(x, levels = 0:data$itemsCount))))

  personScoresCount = cbind(personScoresCount, t(apply(replicatedPersonScoresCount, 2, summarizeSamples)))
  personScoresCount = cbind(personScoresCount, p = rep(0, data$itemsCount + 1))

  colnames(personScoresCount) = c("score", "observed", "replicatedMean", "replicatedHdiLowerBound", "replicatedHdiUpperBound", "ppp")

  # calculate ppp-values
  for (i in 1:(data$itemsCount + 1)) {
    personScoresCount[i, 6] = mean(replicatedPersonScoresCount[, i] >= personScoresCount[i, 2])
  }

  # calculate duration
  endTime = proc.time()
  duration = (endTime - startTime)[3]

  cat("Done!\n")
  cat("Elapsed time:", duration, "s\n")

  return (personScoresCount)
}
