source("src/modules/summarizeSamples.R")


getMatrixCheckInitialValues = function(data) {
  orderBy = c(matrix(c(1, 2, 4, 3, 5, 7, 6, 8, 9), 3, 3))

  return (matrix(sort(data)[orderBy], 3, 3))
}

getMatrixCheckFitStatistic = function(n, N, p) {
  return (sum((n - N * p) ^ 2 / (N * p)))
}


#
# Checks single and/or double cancellation axioms on a single matrix.
#

checkMatrix = function(n, N, test = "single", seed = NULL) {
  if (is.integer(seed)) {
    set.seed(seed)
  }

  warmupCount = 500
  iterationsCount = 4500
  thinning = 4
  credibleMass = c(0.025, 0.975)

  # initialize
  p = n / N

  parametersChain = array(0, dim = c(trunc((iterationsCount - warmupCount) / thinning), 3, 3))
  replicatedCorrectScoresTableChain = array(0, dim = c(trunc((iterationsCount - warmupCount) / thinning), 3, 3))
  logLikelihoodChain = array(0, dim = c(trunc((iterationsCount - warmupCount) / thinning), 9))
  observedFitChain = rep(0, trunc((iterationsCount - warmupCount) / thinning))
  replicatedFitChain = rep(0, trunc((iterationsCount - warmupCount) / thinning))

  initialValues = getMatrixCheckInitialValues(p)
  lastSamples = initialValues
  lastLikelihoods = loglikelihood(n, N, initialValues)

  # iterate
  for (iterationIndex in 1:iterationsCount) {
    for (i in 1:3) {
      for (j in 1:3) {
        proposal.lowerBound.1 = 0
        proposal.lowerBound.2 = 0
        proposal.lowerBound.3 = 0
        proposal.upperBound.1 = 1
        proposal.upperBound.2 = 1
        proposal.upperBound.3 = 1

        # single cancellation
        if (tolower(test) != "double") {
          if (j == 1) {
            proposal.lowerBound.1 = 0
          } else {
            proposal.lowerBound.1 = lastSamples[i, j - 1]
          }

          if (i == 1) {
            proposal.lowerBound.2 = 0
          } else {
            proposal.lowerBound.2 = lastSamples[i - 1, j]
          }

          if (j == 3) {
            proposal.upperBound.1 = 1
          } else {
            proposal.upperBound.1 = lastSamples[i, j + 1]
          }

          if (i == 3) {
            proposal.upperBound.2 = 1
          } else {
            proposal.upperBound.2 = lastSamples[i + 1, j]
          }
        }

        # double cancellation
        if (tolower(test) == "double" | tolower(test) == "single-double") {
          inequalityTest.1 = as.logical(lastSamples[2, 1] < lastSamples[1, 2])
          inequalityTest.2 = as.logical(lastSamples[3, 2] < lastSamples[2, 3])

          if (inequalityTest.1 == inequalityTest.2) {
            if (inequalityTest.1 & inequalityTest.2) {
              if (i == 1 & j == 3) {
                proposal.lowerBound.3 = lastSamples[3, 1]
              }

              if (i == 3 & j == 1) {
                proposal.upperBound.3 = lastSamples[1, 3]
              }
            }

            if (!inequalityTest.1 & !inequalityTest.2) {
              if (i == 3 & j == 1) {
                proposal.lowerBound.3 = lastSamples[1, 3]
              }

              if (i == 1 & j == 3) {
                proposal.upperBound.3 = lastSamples[3, 1]
              }
            }
          }
        }

        proposal.lowerBound = max(proposal.lowerBound.1, proposal.lowerBound.2, proposal.lowerBound.3)

        if (proposal.upperBound.3 > proposal.lowerBound) {
          proposal.upperBound = min(proposal.upperBound.1, proposal.upperBound.2, proposal.upperBound.3)
        } else {
          proposal.upperBound = min(proposal.upperBound.1, proposal.upperBound.2)
        }

        if (proposal.upperBound < proposal.lowerBound) {
          proposal.upperBound = 1
        }

        # sample new point
        proposal = runif(1, proposal.lowerBound, proposal.upperBound)
        proposalLikelihood = dbinom(n[i, j], N[i, j], proposal, log = TRUE)

        #acceptance ratio
        acceptanceProbability = min(1, exp(proposalLikelihood - lastLikelihoods[i, j]), na.rm = TRUE)

        if (runif(1) < acceptanceProbability) {
          lastSamples[i, j] = proposal
          lastLikelihoods[i, j] = proposalLikelihood
        }
      }
    }

    if (iterationIndex %% thinning == 0) {
      if (iterationIndex > warmupCount) {
        replicatedCorrectResponses = matrix(rbinom(9, c(N), c(lastSamples)), 3, 3)

        parametersChain[trunc((iterationIndex - warmupCount) / thinning),,] = lastSamples
        replicatedCorrectScoresTableChain[trunc((iterationIndex - warmupCount) / thinning),,] = replicatedCorrectResponses
        logLikelihoodChain[trunc((iterationIndex - warmupCount) / thinning),] = c(lastLikelihoods)
        observedFitChain[trunc((iterationIndex - warmupCount) / thinning)] = getMatrixCheckFitStatistic(n, N, lastSamples)
        replicatedFitChain[trunc((iterationIndex - warmupCount) / thinning)] = getMatrixCheckFitStatistic(replicatedCorrectResponses, N, lastSamples)
      }
    }
  }

  # summarize samples
  means = zeros(parametersChain[1,,])
  hdiLowerBounds = zeros(parametersChain[1,,])
  hdiUpperBounds = zeros(parametersChain[1,,])

  for (i in 1:3) {
    for (j in 1:3) {
      samplesSummary = summarizeSamples(parametersChain[, i, j])

      means[i, j] = samplesSummary["mean"]
      hdiLowerBounds[i, j] = samplesSummary["hdiLowerBound"]
      hdiUpperBounds[i, j] = samplesSummary["hdiUpperBound"]
    }
  }

  return (list(iterationsCount = iterationsCount,
               parameters = parametersChain,
               logLikelihoods = logLikelihoodChain,
               replicatedCorrectScoresTable = replicatedCorrectScoresTableChain,
               observedFits = observedFitChain,
               replicatedFits = replicatedFitChain,
               correctScoresTable = n,
               totalScoresTable = N,
               ppp = mean(replicatedFitChain >= observedFitChain),
               means = means,
               hdiLowerBounds = hdiLowerBounds,
               hdiUpperBounds = hdiUpperBounds,
               fails = p <= hdiLowerBounds | p >= hdiUpperBounds))
}
