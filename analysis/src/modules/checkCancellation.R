source("src/modules/checkMatrix.R")


#
# Checks single and/or double cancellation axioms on random sample of matrices.
#

checkCancellation = function (correctScoresTable, totalScoresTable, test, checksCount = 10) {
  startTime = proc.time()

  if (tolower(test) == "single") {
    cat("Checking ACM axioms: single cancellation\n")
  } else if (tolower(test) == "single-double") {
    cat("Checking ACM axioms: single & double cancellation\n")
  } else if (tolower(test) == "double") {
    cat("Checking ACM axioms: double cancellation\n")
  } else {
    cat("Checking cancellation axioms...\n")
  }

  cat("Start time:", format(Sys.time(), "%X"), "\n")

  rowsCount = dim(correctScoresTable)[1]
  columnsCount = dim(correctScoresTable)[2]
  rowIndices = c(1:dim(correctScoresTable)[1])
  columnIndices = c(1:dim(correctScoresTable)[2])

  matricesList = list()
  totalChecksTable = matrix(0, dim(correctScoresTable)[1], dim(correctScoresTable)[2])
  failedChecksTable = matrix(0, dim(correctScoresTable)[1], dim(correctScoresTable)[2])

  progressBar = txtProgressBar(min = 0, max = checksCount, style = 3)

  checkIndex = 0
  failedAttemptsCount = 0

  while (checkIndex < checksCount) {
    sampleRowIndices = sort(sample(rowIndices, 3))
    sampleColumnIndices = sort(sample(columnIndices, 3))

    isNew = TRUE

    if (length(matricesList) >= 1) {
      for (j in 1:length(matricesList)) {
        if (identical(sampleRowIndices, matricesList[[j]]$rowIndices) & identical(sampleColumnIndices, matricesList[[j]]$columnIndices)) {
          isNew = FALSE
          failedAttemptsCount = failedAttemptsCount + 1

          break
        }
      }
    }

    if (isNew) {
      sampleCorrectScoresTable = correctScoresTable[sampleRowIndices, sampleColumnIndices]
      sampleTotalScoresTable = totalScoresTable[sampleRowIndices, sampleColumnIndices]

      sampleRatioCorrect = sampleCorrectScoresTable / sampleTotalScoresTable

      if (sum(sampleRatioCorrect == 1 | sampleRatioCorrect == 0) == 0) {
        sampleResult = checkMatrix(sampleCorrectScoresTable, sampleTotalScoresTable, test)

        for (j in 1:3) {
          for (k in 1:3) {
            totalChecksTable[sampleRowIndices[j], sampleColumnIndices[k]] = totalChecksTable[sampleRowIndices[j], sampleColumnIndices[k]] + 1

            if (sampleResult$fails[j, k] == TRUE) {
              failedChecksTable[sampleRowIndices[j], sampleColumnIndices[k]] = failedChecksTable[sampleRowIndices[j], sampleColumnIndices[k]] + 1
            }
          }
        }

        checkIndex = checkIndex + 1

        matricesList[[length(matricesList) + 1]] = list(rowIndices = sampleRowIndices,
                                                        columnIndices = sampleColumnIndices,
                                                        correctScoresTable = sampleCorrectScoresTable,
                                                        totalScoresTable = sampleTotalScoresTable,
                                                        means = sampleResult$means,
                                                        hdiLowerBounds = sampleResult$hdiLowerBounds,
                                                        hdiUpperBounds = sampleResult$hdiUpperBounds,
                                                        failsCounts = sampleResult$fails)
      }
    }

    setTxtProgressBar(progressBar, checkIndex)
  }

  checkFailureRates = failedChecksTable / totalChecksTable
  notNaIndices = !is.na(checkFailureRates)

  weightedMeanFailureRate = sum(checkFailureRates[notNaIndices] * totalScoresTable[notNaIndices]) / sum(totalScoresTable[notNaIndices])
  meanFailureRate = mean(checkFailureRates, na.rm = TRUE)

  weightedMeanPersonFailureRates = rep(0, rowsCount)
  meanPersonFailureRates = rep(0, rowsCount)

  for (i in 1:rowsCount) {
    notNaRowIndices = !is.na(checkFailureRates[i,])

    if (sum(notNaRowIndices) == 0) {
      weightedMeanPersonFailureRates[i] = NA
      meanPersonFailureRates[i] = NA
    } else {
      weightedMeanPersonFailureRates[i] = sum(checkFailureRates[i, notNaRowIndices] * totalScoresTable[i, notNaRowIndices]) / sum(totalScoresTable[i, notNaRowIndices])
      meanPersonFailureRates[i] = mean(checkFailureRates[i,], na.rm = TRUE)
    }
  }

  weightedMeanItemFailureRates = rep(0, columnsCount)
  meanItemFailureRates = rep(0, columnsCount)

  for (i in 1:columnsCount) {
    notNaRowIndices = !is.na(checkFailureRates[, i])

    if (sum(notNaRowIndices) == 0) {
      weightedMeanItemFailureRates[i] = NA
      meanItemFailureRates[i] = NA
    } else {
      weightedMeanItemFailureRates[i] = sum(checkFailureRates[notNaRowIndices, i] * totalScoresTable[notNaRowIndices, i]) / sum(totalScoresTable[notNaRowIndices, i])
      meanItemFailureRates[i] = mean(checkFailureRates[, i], na.rm = TRUE)
    }
  }

  # remove bad values
  checkFailureRates[!is.finite(checkFailureRates)] = NA

  # calculate duration
  endTime = proc.time()
  duration = (endTime - startTime)[3]

  cat("\n")
  cat("Done!\n")
  cat("Elapsed time:", duration, "s (", round(duration / checksCount, 2), "s per matrix checked)\n")

  return (list(matricesList = matricesList,
               checksCount = checksCount,
               failedChecksTable = failedChecksTable,
               totalChecksTable = totalChecksTable,
               checkFailureRates = checkFailureRates,
               meanFailureRate = meanFailureRate,
               weightedMeanFailureRate = weightedMeanFailureRate,
               weightedMeanPersonFailureRates = weightedMeanPersonFailureRates,
               meanPersonFailureRates = meanPersonFailureRates,
               weightedMeanItemFailureRates = weightedMeanItemFailureRates,
               meanItemFailureRates = meanItemFailureRates,
               duration = duration))
}
