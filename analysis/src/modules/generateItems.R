#
# Generates items.
#

generateItems = function (itemsCount) {
  items = list()

  for (j in 1:itemsCount) {
    domain1Difficulty = rnorm(1, -1, 1)
    domain2Difficulty = rnorm(1, -1, 1)
    domain3Difficulty = rnorm(1, -1, 1)
    domain1Discrimination = runif(1, 0.5, 2)
    domain2Discrimination = runif(1, 0.5, 2)
    domain3Discrimination = runif(1, 0.5, 2)
    domain1Guessing = runif(1, 0.15, 0.25)
    domain2Guessing = runif(1, 0.15, 0.25)
    domain3Guessing = runif(1, 0.15, 0.25)

    items[[j]] = list(index = j,
                      difficulty = domain3Difficulty + 1,
                      domain1Difficulty = domain1Difficulty,
                      domain2Difficulty = domain2Difficulty,
                      domain3Difficulty = domain3Difficulty,
                      meanDifficulty = mean(c(domain1Difficulty, domain2Difficulty, domain3Difficulty)),
                      lowestDifficulty = min(domain1Difficulty, domain2Difficulty, domain3Difficulty),
                      highestDifficulty = max(domain1Difficulty, domain2Difficulty, domain3Difficulty),
                      discrimination = domain3Discrimination,
                      domain1Discrimination = domain1Discrimination,
                      domain2Discrimination = domain2Discrimination,
                      domain3Discrimination = domain3Discrimination,
                      meanDiscrimination = mean(c(domain1Discrimination, domain2Discrimination, domain3Discrimination)),
                      lowestDiscrimination = min(domain1Discrimination, domain2Discrimination, domain3Discrimination),
                      highestDiscrimination = max(domain1Discrimination, domain2Discrimination, domain3Discrimination),
                      guessing = domain3Guessing,
                      domain1Guessing = domain1Guessing,
                      domain2Guessing = domain2Guessing,
                      domain3Guessing = domain3Guessing,
                      meanGuessing = mean(c(domain1Guessing, domain3Guessing, domain3Guessing)),
                      lowestGuessing = min(domain1Guessing, domain3Guessing, domain3Guessing),
                      highestGuessing = max(domain1Guessing, domain3Guessing, domain3Guessing))
  }

  return (items)
}
