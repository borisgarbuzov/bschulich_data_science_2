rm(list = ls())

# ========== 1. Difference kills continuity ==========
n = 100
whiteNoise = rnorm(n=n)
randomWalk = cumsum(whiteNoise)
randomWalkDiff = diff(randomWalk)
par(mfrow=c(2, 1))
plot(randomWalk)
plot(randomWalkDiff)

# ========== 2. Logarithm and power < 1. ==========
# Symmetricizes positive distribution
n = 1000
expSample = rexp(n=n)
expSampleLog = log(expSample)

hist(expSample)
hist(expSampleLog)

plot(expSample)
plot(expSampleLog)

# too much, try power transformation
lambda = 1/2
expSamplePower = (expSample^lambda - 1) / lambda

hist(expSample)
hist(expSamplePower)

# show as time series
plot(expSample)
plot(expSamplePower)

# log stabilizes variance? Does not work.
n = 1000
myMean = 100000
mySd = 1
whiteNoise = ts(rnorm(n=n, sd = mySd))
whiteNoiseTimesT = whiteNoise * time(whiteNoise)
whiteNoiseTimesTShifted = whiteNoiseTimesT + myMean
whiteNoiseTimesTShiftedLog = log(whiteNoiseTimesTShifted)

plot(whiteNoiseTimesTShifted)
plot(whiteNoiseTimesTShiftedLog)

# 2a. 
# Logarithm did not help. 
# We may try directly the inverse transformation
# Frist, we need to demean or center it
meanHat = mean(whiteNoiseTimesTShifted)
meanHat
myMean
whiteNoiseTimesTShiftedUnshifted = whiteNoiseTimesTShifted - meanHat
plot(whiteNoiseTimesTShiftedUnshifted)
whiteNoiseTimesTShiftedUnshiftedDivided = whiteNoiseTimesTShiftedUnshifted / time(whiteNoiseTimesTShiftedUnshifted)

plot(whiteNoiseTimesTShifted)
plot(whiteNoiseTimesTShiftedUnshiftedDivided)

# A bit more transparent way to produce
# a sample with growing variance
s <- c()
for (i in 1:1000) {
  s <- c(s, rnorm(1, mean = 100, sd = 0.1 * i))
}

par(mfrow = c(1, 1))
plot(as.ts(s))

# ========== 3. How to extract trend ========== 
# Successful
n = 100
whiteNoise = ts(rnorm(n=n, sd = 100))
timeSequence = time(whiteNoise)
beta0 = 1
beta1 = 2
trend = beta0 + beta1 * timeSequence
trendedNoise = trend + whiteNoise
fit = lm(trendedNoise ~ timeSequence)
coef(fit)
estimatedTrend = fitted(fit)
plot(trend)
lines(estimatedTrend, col = "blue")
# detrend
detrended = trendedNoise - estimatedTrend
plot(trendedNoise)
lines(detrended, col = "blue")

# ========== 4. Choose a better regressor ==========
a <- seq(from = 1, to = 10, length.out = 100)
b <- rnorm(100)
c <- a^3 + a^2 + rnorm(100, sd = 100)
par(mfrow = c(1, 2))

plot(b, a, main = 'A vs B')
lines(lowess(b, a), col = 2, lwd = 2)

plot(c, a, main = 'A vs C')
lines(lowess(c, a), col = 2, lwd = 2)
