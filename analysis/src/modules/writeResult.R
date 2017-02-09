#
# Writes dataframe with results to a csv file.
#

writeResult = function(result, filename) {
  dir.create("output", showWarnings = FALSE)
  write.table(result,
              file = paste("output/", filename, ".csv", sep =""),
              quote = FALSE,
              sep = ";",
              eol = "\n",
              dec = ",",
              row.names = TRUE,
              col.names = TRUE)
}
