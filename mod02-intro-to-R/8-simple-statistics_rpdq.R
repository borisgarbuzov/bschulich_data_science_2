# A family of 4 functions: pdqr

# Set parameters
trueMean = 5
trueSd = 6
sampleSize = 5000

# Generate
mySample = rnorm(n = sampleSize, mean = trueMean, sd = trueSd)

# Compare true parameters to estimated
sampleMean = mean(mySample)
sampleMean
sum(mySample)/length(mySample)
sampleSd = sd(mySample)
sampleSd
par(mfrow = c(1, 1))

# Visualize sample
fakeY = rep(x = 0, times = sampleSize)
plot(x = mySample, y = fakeY)
hist(mySample)

# True cdf vs empiric cdf
par(mfrow = c(2, 1))
# True
truePAtSample = pnorm(q = mySample, mean = trueMean, sd = trueSd)
plot(x = mySample, y = truePAtSample)
# Empiric
myEcdf = ecdf(x = mySample)
plot(myEcdf)
par(mfrow = c(1, 1))
plot(myEcdf)
# If we used points instead of lines, 
# we would not need to use sort
lines(x = sort(mySample), y = sort(truePAtSample), col = "red", lwd = 3)

# Quantile
pointCount = 100
pPoints = seq(0, 1, length.out = pointCount)
# First the sample quantiles
sampleQuantiles = quantile(probs = pPoints, x = mySample)
trueQuantiles = qnorm(pPoints, mean = trueMean, sd = trueSd)
plot(x = pPoints, y = sampleQuantiles, main = "True and true quantile functions")
lines(x = pPoints, y = trueQuantiles, col = "red")

# Now the qq plot
qqnorm(mySample)

# How to use generic qqplot?
# First I want to use something even more generic
# The simple plot function
plot(x = trueQuantiles, y = sampleQuantiles, main = "sample quantiles vs true quantiles (self-made QQ-plot)")

# Now qqplot
qqplot(x = trueQuantiles, y = sampleQuantiles, main = "Using generic qqplot")
n = 10000
# Function sample
DiceSamples <- sample(1:6, size = n, replace = TRUE)

barplot(table(sample(1:3, size=1000, replace=TRUE, prob=c(.30,.60,.10))))

barplot(table(sample(1:3, size=1000, replace=TRUE, prob=c(10, 20, 30))))

