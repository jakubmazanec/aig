#
# Calculates fit of person scores distribution.
#

scoresDistributionFit = function (data, model) {
  startTime = proc.time()

  cat("Computing scores distribution fit statistic for model:", toupper(model$type), "\n")
  cat("Start time:", format(Sys.time(), "%X"), "\n")

  # person scores count
  personScoresCount = data.frame(table(factor(data$personScores, levels = 0:data$itemsCount)))
  personScoresCount[, 1] = as.numeric(personScoresCount[, 1])

  # replicated scores count samples
  replicatedPersonScores = t(apply(model$replicatedResponses, 1, function(x) tapply(x, data$personIndices, sum)))
  replicatedPersonScoresCount = t(apply(replicatedPersonScores, 1, function(x) table(factor(x, levels = 0:data$itemsCount))))

  personScoresCount = cbind(personScoresCount, t(apply(replicatedPersonScoresCount, 2, summarizeSamples)))

  colnames(personScoresCount) = c("score", "observed", "replicatedMean", "replicatedHdiLowerBound", "replicatedHdiUpperBound")
  colnames(personScoresCount) = c("score", "observed", "replicatedMean", "replicatedHdiLowerBound", "replicatedHdiUpperBound")

  # calculate overall fit
  chi2 = function(scores, expected) {
    result = (scores - expected) ^ 2 / expected

    result[!is.finite(result)] = 0

    return (sum(result))
  }

  observedPersonScoresCountChi2 = chi2(personScoresCount$observed, personScoresCount$replicatedMean)
  replicatedPersonScoresCountChi2 = apply(replicatedPersonScoresCount, 1, chi2, expected = personScoresCount$replicatedMean)
  personScoresCountChi2Ppp = mean(replicatedPersonScoresCountChi2 >= observedPersonScoresCountChi2)

  temp = summarizeSamples(replicatedPersonScoresCountChi2)

  # calculate duration
  endTime = proc.time()
  duration = (endTime - startTime)[3]

  cat("Done!\n")
  cat("Elapsed time:", duration, "s\n")

  result = data.frame(observed = observedPersonScoresCountChi2,
                      replicatedMean = temp[1],
                      replicatedHdiLowerBound = temp[2],
                      replicatedHdiUpperBound = temp[3],
                      ppp = personScoresCountChi2Ppp)

  rownames(result) = c()

  return (result)
}
