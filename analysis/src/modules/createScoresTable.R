#
# Creates scores tables - table of sums of correct and total responses per items a persons grouped by their score.
#

createScoresTable = function (responsesTable) {
  itemsCount = ncol(responsesTable)
  columnSums = colSums(responsesTable, na.rm = TRUE)
  reorderedResponses = responsesTable[, order(columnSums)]
  rowSums = rowSums(reorderedResponses, na.rm = TRUE)
  columnNames = colSums(reorderedResponses)
  rowSumsTable = table(rowSums)
  personScores = as.numeric(names(rowSumsTable))[rowSumsTable >= 0] # minimal sum value

  correctScores = list()
  totalScores = list()

  for (personScore in personScores) {
    filteredRows = reorderedResponses[rowSums == personScore,]

    if (!is.matrix(filteredRows)) {
      filteredRows = t(as.matrix(filteredRows))
    }

    tmpRowsCount = nrow(filteredRows)

    totalScores[[as.character(personScore)]] = rep(nrow(filteredRows), itemsCount) - colSums(is.na(filteredRows))
    correctScores[[as.character(personScore)]] = colSums(filteredRows, na.rm = TRUE)
  }

  correctScores = do.call("rbind", correctScores)
  totalScores = do.call("rbind", totalScores)

  colnames(correctScores) = columnNames
  colnames(totalScores) = columnNames

  return (list(correctScores = correctScores,
               totalScores = totalScores))
}
