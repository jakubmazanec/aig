# load libraries and functions
source("src/modules/index.R")

# create datasets
set.seed(42)

data.1PL.noise0 = generateData(generatePersons(500), generateItems(20), model = "1PL", name = "1PL noise 0", errorSd = 0)
data.1PL.noise2 = generateData(generatePersons(500), generateItems(20), model = "1PL", name = "1PL noise 2", errorSd = 2)
data.2PL.noise0 = generateData(generatePersons(500), generateItems(20), model = "2PL", name = "2PL noise 0", errorSd = 0)
data.2PL.noise2 = generateData(generatePersons(500), generateItems(20), model = "2PL", name = "2PL noise 2", errorSd = 2)
data.3PL.noise0 = generateData(generatePersons(500), generateItems(20), model = "3PL", name = "3PL noise 0", errorSd = 0)
data.3PL.noise2 = generateData(generatePersons(500), generateItems(20), model = "3PL", name = "3PL noise 2", errorSd = 2)
data.1PLPOT.noise0 = generateData(generatePersons(500), generateItems(20), model = "1PL-POT", name = "1PL-POT noise 0", errorSd = 0)
data.1PLPOT.noise2 = generateData(generatePersons(500), generateItems(20), model = "1PL-POT", name = "1PL-POT noise 2", errorSd = 2)
data.2PLPOT.noise0 = generateData(generatePersons(500), generateItems(20), model = "2PL-POT", name = "2PL-POT noise 0", errorSd = 0)
data.2PLPOT.noise2 = generateData(generatePersons(500), generateItems(20), model = "2PL-POT", name = "2PL-POT noise 2", errorSd = 2)

# calculate results
result = testData(data.1PL.noise0)
result = testData(data.1PL.noise2)
result = testData(data.2PL.noise0)
result = testData(data.2PL.noise2)
result = testData(data.3PL.noise0)
result = testData(data.3PL.noise2)
result = testData(data.1PLPOT.noise0)
result = testData(data.1PLPOT.noise2)
result = testData(data.2PLPOT.noise0)
result = testData(data.2PLPOT.noise2)
