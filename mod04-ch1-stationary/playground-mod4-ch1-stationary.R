rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Example 1.12
# Sinusoidal base signal
cs = 2*cos(2*pi*(1:500)/50 + .6*pi)
# Generate some noise
w = rnorm(500,0,1)
# in mar we have the sequence bottom, left, upper, right
# cex.main=1.5 is something about title
par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)   # help(par) for info
# next we plot 3 out of 3 specified by the layout matrix
# some interesting encoduing system in expression
tsplot(cs, ylab="", main = expression(x[t]==2*cos(2*pi*t/50+.6*pi)))
tsplot(cs + w, ylab="", main = expression(x[t]==2*cos(2*pi*t/50+.6*pi)+N(0,1)))
tsplot(cs + 5*w, ylab="", main = expression(x[t]==2*cos(2*pi*t/50+.6*pi)+N(0,25)))

# Example 1.24
set.seed(2)
x = rnorm(100)
y = lag(x, -5) + rnorm(100)
par(mfrow=c(1,1))
ccf(y, x, ylab='CCovF', type='covariance')
abline(v=0, lty=2)
text(11, .9, 'x leads')
text(-9, .9, 'y leads')

# an experiment again
myVector = c(1, 2, 3)
myTimeSeries = ts(myVector)
tsp(myTimeSeries)


# -----acf experiment------------------ 
x = rnorm(100, sd = 5)
acf(x, type = "covariance", ylab = "")

# Example 1.25
head(soi)
length(soi)
12*(1950.000- 1987.667)
tsp(soi)
dim(soi)
is.ts(soi)

# stopped here

myAcf = acf(soi, 6, plot=FALSE)
names(myAcf)
myAcf$acf
# print the first 3 significant values (without 1), 
# rounding them to 3 digits
(r = round(myAcf$acf[-1], 3)) # first 6 sample acf values
par(mfrow=c(1,2))
plot(lag(soi,-1), soi); legend('topleft', legend=r[1])
plot(lag(soi,-6), soi); legend('topleft', legend=r[6])
# Most likely, if the lagged version has less points,
# then we drop the unmatched pair

# Example 1.26
set.seed(101010)
x1 = 2*rbinom(11, 1, .5) - 1 # simulated sequence of coin tosses
x2 = 2*rbinom(101, 1, .5) - 1
x1
# We converted from {0, 1} to {-1, 1}
y1 = 5 + filter(x1, sides=1, filter=c(1,-.7))[-1]
y1
unique(y1) # Added by Boris. Curiosity about the values
y2 = 5 + filter(x2, sides=1, filter=c(1,-.7))[-1]
y2
unique(y2) # Added by Boris. Curiosity about the values
tsplot(y1, type='s')  # plot series
tsplot(y2, type='s')
tsplot(y1, type='l')  # plot series
tsplot(y2, type='l')
c(mean(y1), mean(y2))  # the sample means
acf(y1, lag.max=4, plot=TRUE)
acf(y2, lag.max=4, plot=TRUE)

# Example 1.27
par(mfrow=c(1,1))
acf1(speech, 250)
plot.ts(speech)

# Example 1.28
# acf1 and ccf2 are astsa v1.7.7+ scripts
# you can use acf and ccf instead
par(mfrow=c(3,1))
acf1(soi, 48, main="Southern Oscillation Index")
acf1(rec, 48, main="Recruitment")
ccf2(soi, rec, 48, main="SOI vs Recruitment")

# Example 1.29
set.seed(1492)
num=120; t=1:num
X = ts(2*cos(2*pi*t/12) + rnorm(num), freq=12)
Y = ts(2*cos(2*pi*(t+5)/12) + rnorm(num), freq=12)
Y
# pre-whitening
# It is artificially given on what to regress
Yw = residuals( lm(Y~ cos(2*pi*t/12) + sin(2*pi*t/12), na.action=NULL) )
# another way to obtain residuals
lm(Y~ cos(2*pi*t/12) + sin(2*pi*t/12), na.action=NULL)$residuals 
par(mfrow=c(3,2), mgp=c(1.6,.6,0), mar=c(3,3,1,1) )
tsplot(X)
tsplot(Y)
acf1(X, 48, ylab='ACF(X)')
acf1(Y, 48, ylab='ACF(Y)')
ccf2(X, Y, 24)
ccf2(X, Yw, 24, ylim=c(-.6,.6))
