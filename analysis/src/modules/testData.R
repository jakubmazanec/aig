#
# Performs tests on a dataset.
#

testData = function (dataset) {
  startTime = proc.time()

  cat("Testing datasetset:", dataset$name, "\n\n")
  cat("Start time:", format(Sys.time(), "%X"), "\n")

  mean2 = function (x) {
    return (mean(x[is.finite(x)]))
  }

  sd2 = function (x) {
    return (sd(x[is.finite(x)]))
  }

  median2 = function (x) {
    return (median(x[is.finite(x)]))
  }

  mad2 = function (x) {
    return (mad(x[is.finite(x)]))
  }

  model.1PL = fitModel(dataset, "1PL")
  model.2PL = fitModel(dataset, "2PL")
  model.3PL = fitModel(dataset, "3PL")

  result = list(
    datasetName = dataset$name,
    model.1PL = model.1PL,
    model.2PL = model.2PL,
    model.3PL = model.3PL,
    discrepanciesList.1PL = getModelDiscrepancies(dataset, model.1PL),
    discrepanciesList.2PL = getModelDiscrepancies(dataset, model.2PL),
    discrepanciesList.3PL = getModelDiscrepancies(dataset, model.3PL),
    parametricPersonFit.1PL = parametricPersonFit(dataset, model.1PL),
    parametricPersonFit.2PL = parametricPersonFit(dataset, model.2PL),
    parametricPersonFit.3PL = parametricPersonFit(dataset, model.3PL),
    scoresDistribution.1PL = scoresDistribution(dataset, model.1PL),
    scoresDistribution.2PL = scoresDistribution(dataset, model.2PL),
    scoresDistribution.3PL = scoresDistribution(dataset, model.3PL),
    scoresDistributionFit.1PL = scoresDistributionFit(dataset, model.1PL),
    scoresDistributionFit.2PL = scoresDistributionFit(dataset, model.2PL),
    scoresDistributionFit.3PL = scoresDistributionFit(dataset, model.3PL),
    pairwiseItemOddsRatios.1PL = pairwiseItemOddsRatios(dataset, model.1PL),
    pairwiseItemOddsRatios.2PL = pairwiseItemOddsRatios(dataset, model.2PL),
    pairwiseItemOddsRatios.3PL = pairwiseItemOddsRatios(dataset, model.3PL),
    personBiserialCorrelations.1PL = personBiserialCorrelations(dataset, model.1PL),
    personBiserialCorrelations.2PL = personBiserialCorrelations(dataset, model.2PL),
    personBiserialCorrelations.3PL = personBiserialCorrelations(dataset, model.3PL),
    singleCancellation = checkCancellation(dataset$correctScoresTable, dataset$totalScoresTable, test = "single", checksCount = 200),
    doubleCancellation = checkCancellation(dataset$correctScoresTable, dataset$totalScoresTable, test = "single-double", checksCount = 200)
  )

  newColumn = c(
    "N of persons" = dataset$personsCount,
    "N of items" = dataset$itemsCount,
    "N of responses" = dataset$responsesCount,
    "Proportion of correct responses" = mean(dataset$responses),


    # "WAIC (1PL model)" = result$model.1PL$waic[[1]],
    # "WAIC SE (1PL model)" = result$model.1PL$waic[[4]],
    #
    # "WAIC (2PL model)" = result$model.2PL$waic[[1]],
    # "WAIC SE (2PL model)" = result$model.2PL$waic[[4]],
    #
    # "WAIC (3PL model)" = result$model.3PL$waic[[1]],
    # "WAIC SE (3PL model)" = result$model.3PL$waic[[4]],


    "PSIS-LOO (1PL model)" = result$model.1PL$loo[[1]],
    "PSIS-LOO SE (1PL model)" = result$model.1PL$loo[[4]],

    "PSIS-LOO (2PL model)" = result$model.2PL$loo[[1]],
    "PSIS-LOO SE (2PL model)" = result$model.2PL$loo[[4]],

    "PSIS-LOO (3PL model)" = result$model.3PL$loo[[1]],
    "PSIS-LOO SE (3PL model)" = result$model.3PL$loo[[4]],

    "Mean of ability discrepancies (1PL model)" = mean2(abs(result$discrepanciesList.1PL$personAbilitiesSummary[, "mean"])),
    "SD of ability discrepancies (1PL model)" = sd2(abs(result$discrepanciesList.1PL$personAbilitiesSummary[, "mean"])),
    "Median of ability discrepancies (1PL model)" = median2(abs(result$discrepanciesList.1PL$personAbilitiesSummary[, "mean"])),
    "MAD of ability discrepancies (1PL model)" = mad2(abs(result$discrepanciesList.1PL$personAbilitiesSummary[, "mean"])),
    "Number of abilities outside 95 % posterior interval (1PL model)" = sum(!(result$discrepanciesList.1PL$personAbilitiesSummary[, "hdiLowerBound"] <= 0 & result$discrepanciesList.1PL$personAbilitiesSummary[, "hdiUpperBound"] >=0)),

    "Mean of ability discrepancies (2PL model)" = mean2(abs(result$discrepanciesList.2PL$personAbilitiesSummary[, "mean"])),
    "SD of ability discrepancies (2PL model)" = sd2(abs(result$discrepanciesList.2PL$personAbilitiesSummary[, "mean"])),
    "Median of ability discrepancies (2PL model)" = median2(abs(result$discrepanciesList.2PL$personAbilitiesSummary[, "mean"])),
    "MAD of ability discrepancies (2PL model)" = mad2(abs(result$discrepanciesList.2PL$personAbilitiesSummary[, "mean"])),
    "Number of abilities outside 95 % posterior interval (2PL model)" = sum(!(result$discrepanciesList.2PL$personAbilitiesSummary[, "hdiLowerBound"] <= 0 & result$discrepanciesList.2PL$personAbilitiesSummary[, "hdiUpperBound"] >=0)),

    "Mean of ability discrepancies (3PL model)" = mean2(abs(result$discrepanciesList.3PL$personAbilitiesSummary[, "mean"])),
    "SD of ability discrepancies (3PL model)" = sd2(abs(result$discrepanciesList.3PL$personAbilitiesSummary[, "mean"])),
    "Median of ability discrepancies (3PL model)" = median2(abs(result$discrepanciesList.3PL$personAbilitiesSummary[, "mean"])),
    "MAD of ability discrepancies (3PL model)" = mad2(abs(result$discrepanciesList.3PL$personAbilitiesSummary[, "mean"])),
    "Number of abilities outside 95 % posterior interval (3PL model)" = sum(!(result$discrepanciesList.3PL$personAbilitiesSummary[, "hdiLowerBound"] <= 0 & result$discrepanciesList.3PL$personAbilitiesSummary[, "hdiUpperBound"] >=0)),


    "Mean of difficulty discrepancies (1PL model)" = mean2(abs(result$discrepanciesList.1PL$itemDifficultiesSummary[, "mean"])),
    "SD of difficulty discrepancies (1PL model)" = sd2(abs(result$discrepanciesList.1PL$itemDifficultiesSummary[, "mean"])),
    "Median of difficulty discrepancies (1PL model)" = median2(abs(result$discrepanciesList.1PL$itemDifficultiesSummary[, "mean"])),
    "MAD of difficulty discrepancies (1PL model)" = mad2(abs(result$discrepanciesList.1PL$itemDifficultiesSummary[, "mean"])),
    "Number of difficulties outside 95 % posterior interval (1PL model)" = sum(!(result$discrepanciesList.1PL$itemDifficultiesSummary[, "hdiLowerBound"] <= 0 & result$discrepanciesList.1PL$itemDifficultiesSummary[, "hdiUpperBound"] >=0)),

    "Mean of difficulty discrepancies (2PL model)" = mean2(abs(result$discrepanciesList.2PL$itemDifficultiesSummary[, "mean"])),
    "SD of difficulty discrepancies (2PL model)" = sd2(abs(result$discrepanciesList.2PL$itemDifficultiesSummary[, "mean"])),
    "Median of difficulty discrepancies (2PL model)" = median2(abs(result$discrepanciesList.2PL$itemDifficultiesSummary[, "mean"])),
    "MAD of difficulty discrepancies (2PL model)" = mad2(abs(result$discrepanciesList.2PL$itemDifficultiesSummary[, "mean"])),
    "Number of difficulties outside 95 % posterior interval (2PL model)" = sum(!(result$discrepanciesList.2PL$itemDifficultiesSummary[, "hdiLowerBound"] <= 0 & result$discrepanciesList.2PL$itemDifficultiesSummary[, "hdiUpperBound"] >=0)),

    "Mean of difficulty discrepancies (3PL model)" = mean2(abs(result$discrepanciesList.3PL$itemDifficultiesSummary[, "mean"])),
    "SD of difficulty discrepancies (3PL model)" = sd2(abs(result$discrepanciesList.3PL$itemDifficultiesSummary[, "mean"])),
    "Median of difficulty discrepancies (3PL model)" = median2(abs(result$discrepanciesList.3PL$itemDifficultiesSummary[, "mean"])),
    "MAD of difficulty discrepancies (3PL model)" = mad2(abs(result$discrepanciesList.3PL$itemDifficultiesSummary[, "mean"])),
    "Number of difficulties outside 95 % posterior interval (3PL model)" = sum(!(result$discrepanciesList.3PL$itemDifficultiesSummary[, "hdiLowerBound"] <= 0 & result$discrepanciesList.3PL$itemDifficultiesSummary[, "hdiUpperBound"] >=0)),


    "Mean of discrimination discrepancies (1PL model)" = mean2(abs(result$discrepanciesList.1PL$itemDiscriminationsSummary[, "mean"])),
    "SD of discrimination discrepancies (1PL model)" = sd2(abs(result$discrepanciesList.1PL$itemDiscriminationsSummary[, "mean"])),
    "Median of discrimination discrepancies (1PL model)" = median2(abs(result$discrepanciesList.1PL$itemDiscriminationsSummary[, "mean"])),
    "MAD of discrimination discrepancies (1PL model)" = mad2(abs(result$discrepanciesList.1PL$itemDiscriminationsSummary[, "mean"])),
    "Number of discriminations outside 95 % posterior interval (1PL model)" = sum(!(result$discrepanciesList.1PL$itemDiscriminationsSummary[, "hdiLowerBound"] <= 0 & result$discrepanciesList.1PL$itemDiscriminationsSummary[, "hdiUpperBound"] >=0)),

    "Mean of discrimination discrepancies (2PL model)" = mean2(abs(result$discrepanciesList.2PL$itemDiscriminationsSummary[, "mean"])),
    "SD of discrimination discrepancies (2PL model)" = sd2(abs(result$discrepanciesList.2PL$itemDiscriminationsSummary[, "mean"])),
    "Median of discrimination discrepancies (2PL model)" = median2(abs(result$discrepanciesList.2PL$itemDiscriminationsSummary[, "mean"])),
    "MAD of discrimination discrepancies (2PL model)" = mad2(abs(result$discrepanciesList.2PL$itemDiscriminationsSummary[, "mean"])),
    "Number of discriminations outside 95 % posterior interval (2PL model)" = sum(!(result$discrepanciesList.2PL$itemDiscriminationsSummary[, "hdiLowerBound"] <= 0 & result$discrepanciesList.2PL$itemDiscriminationsSummary[, "hdiUpperBound"] >=0)),

    "Mean of discrimination discrepancies (3PL model)" = mean2(abs(result$discrepanciesList.3PL$itemDiscriminationsSummary[, "mean"])),
    "SD of discrimination discrepancies (3PL model)" = sd2(abs(result$discrepanciesList.3PL$itemDiscriminationsSummary[, "mean"])),
    "Median of discrimination discrepancies (3PL model)" = median2(abs(result$discrepanciesList.3PL$itemDiscriminationsSummary[, "mean"])),
    "MAD of discrimination discrepancies (3PL model)" = mad2(abs(result$discrepanciesList.3PL$itemDiscriminationsSummary[, "mean"])),
    "Number of discriminations outside 95 % posterior interval (3PL model)" = sum(!(result$discrepanciesList.3PL$itemDiscriminationsSummary[, "hdiLowerBound"] <= 0 & result$discrepanciesList.3PL$itemDiscriminationsSummary[, "hdiUpperBound"] >=0)),


    "Mean of guessing discrepancies (1PL model)" = mean2(abs(result$discrepanciesList.1PL$itemGuessingsSummary[, "mean"])),
    "SD of guessing discrepancies (1PL model)" = sd2(abs(result$discrepanciesList.1PL$itemGuessingsSummary[, "mean"])),
    "Median of guessing discrepancies (1PL model)" = median2(abs(result$discrepanciesList.1PL$itemGuessingsSummary[, "mean"])),
    "MAD of guessing discrepancies (1PL model)" = mad2(abs(result$discrepanciesList.1PL$itemGuessingsSummary[, "mean"])),
    "Number of guessings outside 95 % posterior interval (1PL model)" = sum(!(result$discrepanciesList.1PL$itemGuessingsSummary[, "hdiLowerBound"] <= 0 & result$discrepanciesList.1PL$itemGuessingsSummary[, "hdiUpperBound"] >=0)),

    "Mean of guessing discrepancies (2PL model)" = mean2(abs(result$discrepanciesList.2PL$itemGuessingsSummary[, "mean"])),
    "SD of guessing discrepancies (2PL model)" = sd2(abs(result$discrepanciesList.2PL$itemGuessingsSummary[, "mean"])),
    "Median of guessing discrepancies (2PL model)" = median2(abs(result$discrepanciesList.2PL$itemGuessingsSummary[, "mean"])),
    "MAD of guessing discrepancies (2PL model)" = mad2(abs(result$discrepanciesList.2PL$itemGuessingsSummary[, "mean"])),
    "Number of guessings outside 95 % posterior interval (2PL model)" = sum(!(result$discrepanciesList.2PL$itemGuessingsSummary[, "hdiLowerBound"] <= 0 & result$discrepanciesList.2PL$itemGuessingsSummary[, "hdiUpperBound"] >=0)),

    "Mean of guessing discrepancies (3PL model)" = mean2(abs(result$discrepanciesList.3PL$itemGuessingsSummary[, "mean"])),
    "SD of guessing discrepancies (3PL model)" = sd2(abs(result$discrepanciesList.3PL$itemGuessingsSummary[, "mean"])),
    "Median of guessing discrepancies (3PL model)" = median2(abs(result$discrepanciesList.3PL$itemGuessingsSummary[, "mean"])),
    "MAD of guessing discrepancies (3PL model)" = mad2(abs(result$discrepanciesList.3PL$itemGuessingsSummary[, "mean"])),
    "Number of guessings outside 95 % posterior interval (3PL model)" = sum(!(result$discrepanciesList.3PL$itemGuessingsSummary[, "hdiLowerBound"] <= 0 & result$discrepanciesList.3PL$itemGuessingsSummary[, "hdiUpperBound"] >=0)),


    "Mean of response probability discrepancies (1PL model)" = mean2(abs(result$discrepanciesList.1PL$responseProbabilitiesSummary[, "mean"])),
    "SD of response probability discrepancies (1PL model)" = sd2(abs(result$discrepanciesList.1PL$responseProbabilitiesSummary[, "mean"])),
    "Median of response probability discrepancies (1PL model)" = median2(abs(result$discrepanciesList.1PL$responseProbabilitiesSummary[, "mean"])),
    "MAD of response probability discrepancies (1PL model)" = mad2(abs(result$discrepanciesList.1PL$responseProbabilitiesSummary[, "mean"])),
    "Number of response probabilities outside 95 % posterior interval (1PL model)" = sum(!(result$discrepanciesList.1PL$responseProbabilitiesSummary[, "hdiLowerBound"] <= 0 & result$discrepanciesList.1PL$responseProbabilitiesSummary[, "hdiUpperBound"] >=0)),

    "Mean of response probability discrepancies (2PL model)" = mean2(abs(result$discrepanciesList.2PL$responseProbabilitiesSummary[, "mean"])),
    "SD of response probability discrepancies (2PL model)" = sd2(abs(result$discrepanciesList.2PL$responseProbabilitiesSummary[, "mean"])),
    "Median of response probability discrepancies (2PL model)" = median2(abs(result$discrepanciesList.2PL$responseProbabilitiesSummary[, "mean"])),
    "MAD of response probability discrepancies (2PL model)" = mad2(abs(result$discrepanciesList.2PL$responseProbabilitiesSummary[, "mean"])),
    "Number of response probabilities outside 95 % posterior interval (2PL model)" = sum(!(result$discrepanciesList.2PL$responseProbabilitiesSummary[, "hdiLowerBound"] <= 0 & result$discrepanciesList.2PL$responseProbabilitiesSummary[, "hdiUpperBound"] >=0)),

    "Mean of response probability discrepancies (3PL model)" = mean2(abs(result$discrepanciesList.3PL$responseProbabilitiesSummary[, "mean"])),
    "SD of response probability discrepancies (3PL model)" = sd2(abs(result$discrepanciesList.3PL$responseProbabilitiesSummary[, "mean"])),
    "Median of response probability discrepancies (3PL model)" = median2(abs(result$discrepanciesList.3PL$responseProbabilitiesSummary[, "mean"])),
    "MAD of response probability discrepancies (3PL model)" = mad2(abs(result$discrepanciesList.3PL$responseProbabilitiesSummary[, "mean"])),
    "Number of response probabilities outside 95 % posterior interval (3PL model)" = sum(!(result$discrepanciesList.3PL$responseProbabilitiesSummary[, "hdiLowerBound"] <= 0 & result$discrepanciesList.3PL$responseProbabilitiesSummary[, "hdiUpperBound"] >=0)),


    "Mean of W person fit statistics (1PL model)" = mean2(result$parametricPersonFit.1PL$observedMean),
    "SD of W person fit statistics (1PL model)" = sd2(result$parametricPersonFit.1PL$observedMean),
    "Median of W person fit statistics (1PL model)" = median2(result$parametricPersonFit.1PL$observedMean),
    "MAD of W person fit statistics (1PL model)" = mad2(result$parametricPersonFit.1PL$observedMean),
    "Number of Ws outside 95 % posterior predictive interval (1PL model)" = length(result$parametricPersonFit.1PL$ppp[result$parametricPersonFit.1PL$ppp <= 0.05]),

    "Mean of W person fit statistics (2PL model)" = mean2(result$parametricPersonFit.2PL$observedMean),
    "SD of W person fit statistics (2PL model)" = sd2(result$parametricPersonFit.2PL$observedMean),
    "Median of W person fit statistics (2PL model)" = median2(result$parametricPersonFit.2PL$observedMean),
    "MAD of W person fit statistics (2PL model)" = mad2(result$parametricPersonFit.2PL$observedMean),
    "Number of Ws outside 95 % posterior predictive interval (2PL model)" = length(result$parametricPersonFit.2PL$ppp[result$parametricPersonFit.2PL$ppp <= 0.05]),

    "Mean of W person fit statistics (3PL model)" = mean2(result$parametricPersonFit.3PL$observedMean),
    "SD of W person fit statistics (3PL model)" = sd2(result$parametricPersonFit.3PL$observedMean),
    "Median of W person fit statistics (3PL model)" = median2(result$parametricPersonFit.3PL$observedMean),
    "MAD of W person fit statistics (3PL model)" = mad2(result$parametricPersonFit.3PL$observedMean),
    "Number of Ws outside 95 % posterior predictive interval (3PL model)" = length(result$parametricPersonFit.3PL$ppp[result$parametricPersonFit.3PL$ppp <= 0.05]),


    "Total number of person scores" = nrow(dataset$totalScoresTable),
    "Number of person scores outside 95 % posterior predictive interval (1PL model)" = length(result$scoresDistribution.1PL$ppp[result$scoresDistribution.1PL$ppp <= 0.025 | result$scoresDistribution.1PL$ppp >= 0.975]),
    "Person scores distribution fit statistic (1PL model)" = result$scoresDistributionFit.1PL$observed,
    "Person scores distribution fit statistic PPP-value (1PL model)" = result$scoresDistributionFit.1PL$ppp,

    "Number of person scores outside 95 % posterior predictive interval (2PL model)" = length(result$scoresDistribution.2PL$ppp[result$scoresDistribution.2PL$ppp <= 0.025 | result$scoresDistribution.2PL$ppp >= 0.975]),
    "Person scores distribution fit statistic (2PL model)" = result$scoresDistributionFit.2PL$observed,
    "Person scores distribution fit statistic PPP-value (2PL model)" = result$scoresDistributionFit.2PL$ppp,

    "Number of person scores outside 95 % posterior predictive interval (3PL model)" = length(result$scoresDistribution.3PL$ppp[result$scoresDistribution.3PL$ppp <= 0.025 | result$scoresDistribution.3PL$ppp >= 0.975]),
    "Person scores distribution fit statistic (3PL model)" = result$scoresDistributionFit.3PL$observed,
    "Person scores distribution fit statistic PPP-value (3PL model)" = result$scoresDistributionFit.3PL$ppp,


    "Total number of ratios" = choose(dataset$itemsCount, 2),
    "Mean of pairwise item odds ratios (1PL model)" = mean2(result$pairwiseItemOddsRatios.1PL$observed),
    "SD of pairwise item odds ratios (1PL model)" = sd2(result$pairwiseItemOddsRatios.1PL$observed),
    "Median of pairwise item odds ratios (1PL model)" = median2(result$pairwiseItemOddsRatios.1PL$observed),
    "MAD of pairwise item odds ratios (1PL model)" = mad2(result$pairwiseItemOddsRatios.1PL$observed),
    "Number of ratios outside 95 % posterior predictive interval (1PL model)" = length(result$pairwiseItemOddsRatios.1PL$ppp[result$pairwiseItemOddsRatios.1PL$ppp <= 0.025 | result$pairwiseItemOddsRatios.1PL$ppp >= 0.975]),

    "Mean of pairwise item odds ratios (2PL model)" = mean2(result$pairwiseItemOddsRatios.2PL$observed),
    "SD of pairwise item odds ratios (2PL model)" = sd2(result$pairwiseItemOddsRatios.2PL$observed),
    "Median of pairwise item odds ratios (2PL model)" = median2(result$pairwiseItemOddsRatios.2PL$observed),
    "MAD of pairwise item odds ratios (2PL model)" = mad2(result$pairwiseItemOddsRatios.2PL$observed),
    "Number of ratios outside 95 % posterior predictive interval (2PL model)" = length(result$pairwiseItemOddsRatios.2PL$ppp[result$pairwiseItemOddsRatios.2PL$ppp <= 0.025 | result$pairwiseItemOddsRatios.2PL$ppp >= 0.975]),

    "Mean of pairwise item odds ratios (3PL model)" = mean2(result$pairwiseItemOddsRatios.3PL$observed),
    "SD of pairwise item odds ratios (3PL model)" = sd2(result$pairwiseItemOddsRatios.3PL$observed),
    "Median of pairwise item odds ratios (3PL model)" = median2(result$pairwiseItemOddsRatios.3PL$observed),
    "MAD of pairwise item odds ratios (3PL model)" = mad2(result$pairwiseItemOddsRatios.3PL$observed),
    "Number of ratios outside 95 % posterior predictive interval (3PL model)" = length(result$pairwiseItemOddsRatios.3PL$ppp[result$pairwiseItemOddsRatios.3PL$ppp <= 0.025 | result$pairwiseItemOddsRatios.3PL$ppp >= 0.975]),


    "Mean of person biserial correlations (1PL model)" = mean2(result$personBiserialCorrelations.1PL$observed),
    "SD of person biserial correlations (1PL model)" = sd2(result$personBiserialCorrelations.1PL$observed),
    "Median of person biserial correlations (1PL model)" = median2(result$personBiserialCorrelations.1PL$observed),
    "MAD of person biserial correlations (1PL model)" = mad2(result$personBiserialCorrelations.1PL$observed),
    "Number of correlations outside 95 % posterior predictive interval (1PL model)" = length(result$personBiserialCorrelations.1PL$ppp[(result$personBiserialCorrelations.1PL$ppp <= 0.025 | result$personBiserialCorrelations.1PL$ppp >= 0.975) & !is.na(result$personBiserialCorrelations.1PL$ppp)]),

    "Mean of person biserial correlations (2PL model)" = mean2(result$personBiserialCorrelations.2PL$observed),
    "SD of person biserial correlations (2PL model)" = sd2(result$personBiserialCorrelations.2PL$observed),
    "Median of person biserial correlations (2PL model)" = median2(result$personBiserialCorrelations.2PL$observed),
    "MAD of person biserial correlations (2PL model)" = mad2(result$personBiserialCorrelations.2PL$observed),
    "Number of correlations outside 95 % posterior predictive interval (2PL model)" = length(result$personBiserialCorrelations.2PL$ppp[(result$personBiserialCorrelations.2PL$ppp <= 0.025 | result$personBiserialCorrelations.2PL$ppp >= 0.975) & !is.na(result$personBiserialCorrelations.2PL$ppp)]),

    "Mean of person biserial correlations (3PL model)" = mean2(result$personBiserialCorrelations.3PL$observed),
    "SD of person biserial correlations (3PL model)" = sd2(result$personBiserialCorrelations.3PL$observed),
    "Median of person biserial correlations (3PL model)" = median2(result$personBiserialCorrelations.3PL$observed),
    "MAD of person biserial correlations (3PL model)" = mad2(result$personBiserialCorrelations.3PL$observed),
    "Number of correlations outside 95 % posterior predictive interval (3PL model)" = length(result$personBiserialCorrelations.3PL$ppp[(result$personBiserialCorrelations.3PL$ppp <= 0.025 | result$personBiserialCorrelations.3PL$ppp >= 0.975) & !is.na(result$personBiserialCorrelations.3PL$ppp)]),


    "Mean of failed axiom checks (Single cancellation)" = result$singleCancellation$meanFailureRate,
    "Weighted mean of failed axiom checks (Single cancellation)" = result$singleCancellation$weightedMeanFailureRate,
    "Number of matrices checked (Single cancellation)" = length(result$singleCancellation$matricesList),
    "Total number of matrices (Single cancellation)" = choose(nrow(result$singleCancellation$totalChecksTable), 3) * choose(ncol(result$singleCancellation$totalChecksTable), 3),
    "Mean of failed axiom checks (Single & double cancellation)" = result$doubleCancellation$meanFailureRate,
    "Weighted mean of failed axiom checks (Single & double cancellation)" = result$doubleCancellation$weightedMeanFailureRate,
    "Number of matrices checked (Single & double cancellation)" = length(result$doubleCancellation$matricesList),
    "Total number of matrices (Single & double cancellation)" = choose(nrow(result$doubleCancellation$totalChecksTable), 3) * choose(ncol(result$doubleCancellation$totalChecksTable), 3)
  )

  # create results table
  overallResults = data.frame(result = newColumn)
  colnames(overallResults) = c(dataset$name)
  rownames(overallResults) = names(newColumn)

  # write to file
  writeResult(overallResults, dataset$name)

  # calculate duration
  endTime = proc.time()
  duration = (endTime - startTime)[3]

  cat("\n")
  cat("Dataset testing done!\n")
  cat("Elapsed time:", duration, "s\n")

  return (result)
}
