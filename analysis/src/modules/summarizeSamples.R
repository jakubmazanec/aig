source("src/modules/hdi.R")

#
# Summarizes vector of MCMC samples.
#

# summarizePost = function (samples, compVal = NULL, rope = NULL , credibleMass = 0.95) {
# summarizeSamples = function (samples, rope = NULL , credibleMass = 0.95) {
summarizeSamples = function (samples, credibleMass = 0.95) {
  samplesMean = mean(samples, na.rm = TRUE)
  # samplesMedian = median(samples)
  # samplesDensity = density(samples)
  # samplesMode = samplesDensity$x[which.max(samplesDensity$y)]
  # mcmcEffSz = round( effectiveSize( samples ) , 1 )
  # names(mcmcEffSz) = NULL

  hdiLim = hdi(samples, credibleMass = credibleMass)

  # if (!is.null(rope)) {
  #   pcltRope = 100 * sum(samples < rope[1]) / length(samples)
  #   pcgtRope = 100 * sum(samples > rope[2]) / length(samples)
  #   pcinRope = 100 - (pcltRope + pcgtRope)
  # } else {
  #   rope = c(NA, NA)
  #   pcltRope = NA
  #   pcgtRope = NA
  #   pcinRope = NA
  # }

  return (c(mean = samplesMean,
            # median = samplesMedian,
            # mode = samplesMode,
             #ESS=mcmcEffSz ,
            # hdiMass = credibleMass,
            hdiLowerBound = hdiLim[[1]],
            hdiUpperBound = hdiLim[[2]]
            #CompVal=compVal , PcntGtCompVal=pcgtCompVal ,
            # ropeLower = rope[1],
            # ropeUpper = rope[2],
            # belowROPE = pcltRope,
            # inROPE = pcinRope,
            # aboveROPE = pcgtRope))
  ))
}
