rm(list=ls())

# Basic manipulations.
# 1. Pointwise.
# a. Unary.
x = ts(c(10, 20, 30))
x
time(x)

# i. Multiplication by constant.
multipliedX = 2 * x

# ii. Logarithm.
logarithmedX = log(x)

# iii. Subtract average (demean or center)
centeredX = x - mean(x)
sum(centeredX)
mean(centeredX)


# b. binary.
# i. Plus
y = ts(c(100, 200, 300))
myAddition = x + y
myAddition

# ii. Minus, ...
mySubtraction = x - y
time(mySubtraction)
names(mySubtraction)

# 2. Non-local
# a. Backshift
y = lag(x, k=-1)
time(y)
y[1]

# b. Forward shift
forwardShiftedX = lag(x, k=1)
time (forwardShiftedX)

# c. Higher order back shift B^d
d = 5
higherOrderBackShiftedX = lag(x = x, k = -d)
time(higherOrderBackShiftedX)

# b. Difference (1 - B),
difX = diff(x)
difX
time(difX)
# stopped here

# Explicit lag
# We do not need to put a minus here
# It will be a normal backward difference
diff(x = x, lag = 1)

# In fact, it can not even accept the negative lag for some reason
diff(x = x, lag = -1)

# Zero lag is also unacceptable
diff(x = x, lag = 0)

constructedDifX = x - lag(x = x, k = -1)
constructedDifX
# In both cases it became shorter
# And the time is as of the one of x, with first element cut off

# Higher lag difference (1 - B^d)
k = 2
myDiff = diff(x, lag = k, differences = 1)
myDiff
time(myDiff)

# Constructed higher lag difference
higherLagDifX = x - lag(x, -k)
higherLagDifX
time(higherLagDifX)

# and higher order difference (1 - B)^d
k = 1
d = 2
diff(x = x, lag = 1, differences = d)

# i. Det. constant
# We do not necessarily need to use ts here.
# Just vector is enough
constantSeries = rep(x = 10, times = 3)
constantSeries
constantSeries = rep(x = 10, each = 3)
constantSeries
constantSeries = rep(x = 10, length.out = 3)
constantSeries
constantSeries = c(10, 10, 10)
constantSeries
diff(constantSeries)
# Converted to a zero vector shorter than the original by 1

# ii. Det. linear trend
# Keep using vectors without ts
b0 = 1
b1 = 2
n = 5
equidistantValues = 1:n
linearTrend = b0 + b1*equidistantValues
linearTrend
par(mfrow = c(2,1))
plot(linearTrend)
constantSeries = diff(linearTrend)
# Converted to a constant series
constantSeries
plot(constantSeries)

# iii. Det. quadratic trend
b0 = 1
b1 = 2
b2 = 3
n = 50
equidistantValues = 1:n
quadraticTrend = b0 + b1 * equidistantValues + b2 * equidistantValues^2
quadraticTrend
par(mfrow = c(2,1))
plot(quadraticTrend, type = "b") # both - dots and lines
linearOutput = diff(quadraticTrend)
plot(linearOutput, type = "b")

# iv. Det. Sinusoid with lag=period difference maps into a constant
# sin(x) has period = 2*pi
# sin(x*pi) has period = 2
# First let us apply the regular lag=1 distance
# And see that the sinosoid changes phase
n = 50
periodCount = 5
equidistantValues = (1:n) * 2/n * 5

sinusoidPeriod2 = sin(equidistantValues*pi)
sinusoidPeriod2
par(mfrow = c(2,1))
plot(sinusoidPeriod2, type = "b") # both - dots and lines
anotherSinusoidPeriod2 = diff(sinusoidPeriod2)
plot(anotherSinusoidPeriod2, type = "b")

# to make it constant, we need a difference of lag a period
# Now the period is 10 elementary intervals
par(mfrow = c(2,1))
plot(sinusoidPeriod2, type = "b") # both - dots and lines
shiftedByAPeriod = lag(sinusoidPeriod2, k=10)
plot(shiftedByAPeriod, type = "b")
# same as it should be!

par(mfrow = c(2,1))
plot(sinusoidPeriod2, type = "b") # both - dots and lines
constantOutput = diff(sinusoidPeriod2, lag=10)
constantOutput
plot(constantOutput, type = "b", ylim = c(-0.1, 0.1))

# v. Det. cumulative sum, several times
n = 100
x = 1:n
x
par(mfrow = c(3,1))
plot(x, type = "b")
cumsumX = cumsum(x)
cumsumX
plot(cumsumX, type = "b")
difCumsumX = diff(cumsumX)
difCumsumX
# It should return it back to x with a loss 1 point
plot(difCumsumX, type = "b")
length(x)
length(cumsumX)
length(difCumsumX)


# vi. iid to MA(1)
# What happens to iid after differencing?
n = 1000
iid = rnorm(n)
iid
difIid = diff(iid)
difIid
par(mfrow = c(2,1))
plot(iid, type = "l")
plot(difIid, type = "l")
# Visually, they are hardly distinguishable,
# but difIid is no longer iid

# vii. random walk, several times
n = 1000
iid = rnorm(n)
randomWalk = cumsum(iid)
par(mfrow = c(3,1))
plot(iid, type = "l")
plot(randomWalk, type = "l")
# Back to iid
difRandomWalk = diff(randomWalk)
plot(difRandomWalk, type = "l")

# Linear filter

set.seed(1)
w = rnorm(5, 0, 1) # 500 N(0,1) variates
v = filter(w, sides=2, filter=rep(1/3, 3)) # moving average
length(v)
par(mfrow=c(2,1))
plot.ts(w, main="white noise")
plot.ts(v, ylim=c(-3,3), main="moving average")

# Stopped here

dfMatchedTime = ts.intersect(lag(w, k= -1), as.ts(w), lag(w, k = 1),
                             dframe = T)
dim(dfMatchedTime)
# First and last are dropped by intersect

vConstructed = ts(data = 1/3 * rowSums(dfMatchedTime), start = 2)
tsp(vConstructed)
par(mfrow=c(2,1))
plot.ts(w, main="white noise")
plot.ts(vConstructed, ylim=c(-3,3), main="moving average")

v = na.omit(v) # we need to remove NAs at the start and end of v
tsp(v)
par(mfrow=c(1, 1))
plot(v, type='l')
lines(vConstructed, col='red')

myDif = v - vConstructed
myDif
