data {
	int<lower = 1> itemsCount;
	int<lower = 1> personsCount;
	int<lower = 1> responsesCount;
	int<lower = 0, upper = 1> responses[responsesCount];
	int<lower = 1> itemIndices[responsesCount];
	int<lower = 1> personIndices[responsesCount];
}

parameters {
	real personAbilities[personsCount];
	real itemDifficulties[itemsCount];
	real<lower = 0> itemDiscriminations[itemsCount];
	real<lower = 0, upper = 1> itemGuessings[itemsCount];
	real<lower = 0> itemDifficultyScale;
	real<lower = 0> itemDiscriminationScale;
	real<lower = 0> itemGuessingBeta;
}

model {
	// hyperpriors
	itemDifficultyScale ~ cauchy(0, 5);
	itemDiscriminationScale ~ cauchy(0, 5);
	itemGuessingBeta ~ cauchy(0, 5);

	// priors
	personAbilities ~ normal(0, 1);
	itemDifficulties ~ normal(0, itemDifficultyScale);
	itemDiscriminations ~ lognormal(0, itemDiscriminationScale);
	itemGuessings ~ beta(1, itemGuessingBeta);

	for (responseIndex in 1:responsesCount) {
	  responses[responseIndex] ~ bernoulli(itemGuessings[itemIndices[responseIndex]] + (1 - itemGuessings[itemIndices[responseIndex]]) * inv_logit( itemDiscriminations[itemIndices[responseIndex]] * (personAbilities[personIndices[responseIndex]] - itemDifficulties[itemIndices[responseIndex]])));
	}
}

generated quantities{
  int replicatedResponses[responsesCount];
  real logLikelihood[responsesCount];
  real responseProbabilities[responsesCount];

  for (responseIndex in 1:responsesCount) {
	  replicatedResponses[responseIndex] = bernoulli_rng(itemGuessings[itemIndices[responseIndex]] + (1 - itemGuessings[itemIndices[responseIndex]]) * inv_logit( itemDiscriminations[itemIndices[responseIndex]] * (personAbilities[personIndices[responseIndex]] - itemDifficulties[itemIndices[responseIndex]])));
	  logLikelihood[responseIndex] = bernoulli_lpmf(responses[responseIndex] | itemGuessings[itemIndices[responseIndex]] + (1 - itemGuessings[itemIndices[responseIndex]]) * inv_logit( itemDiscriminations[itemIndices[responseIndex]] * (personAbilities[personIndices[responseIndex]] - itemDifficulties[itemIndices[responseIndex]])));
	  responseProbabilities[responseIndex] = itemGuessings[itemIndices[responseIndex]] + (1 - itemGuessings[itemIndices[responseIndex]]) * inv_logit( itemDiscriminations[itemIndices[responseIndex]] * (personAbilities[personIndices[responseIndex]] - itemDifficulties[itemIndices[responseIndex]]));
	}
}
