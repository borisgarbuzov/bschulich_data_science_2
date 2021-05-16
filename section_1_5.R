rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Section 1.5 Estimation of Correlation

# Example 1.25
(r = round(acf(soi, 6, plot=FALSE)$acf[-1], 3)) # first 6 sample acf values
par(mfrow=c(1,2))
plot(lag(soi,-1), soi); legend('topleft', legend=r[1])
plot(lag(soi,-6), soi); legend('topleft', legend=r[6])

# Example 1.26
set.seed(101010)
x1 = 2*rbinom(11, 1, .5) - 1 # simulated sequence of coin tosses
x2 = 2*rbinom(101, 1, .5) - 1
y1 = 5 + filter(x1, sides=1, filter=c(1,-.7))[-1]
y2 = 5 + filter(x2, sides=1, filter=c(1,-.7))[-1]
tsplot(y1, type='s')  # plot series
tsplot(y2, type='s')
c(mean(y1), mean(y2))  # the sample means
acf(y1, lag.max=4, plot=FALSE)
acf(y2, lag.max=4, plot=FALSE)

# Example 1.27
par(mfrow=c(1,1))
acf1(speech, 250)

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
Yw = resid( lm(Y~ cos(2*pi*t/12) + sin(2*pi*t/12), na.action=NULL) )
par(mfrow=c(3,2), mgp=c(1.6,.6,0), mar=c(3,3,1,1) )
tsplot(X)
tsplot(Y)
acf1(X, 48, ylab='ACF(X)')
acf1(Y, 48, ylab='ACF(Y)')
ccf2(X, Y, 24)
ccf2(X, Yw, 24, ylim=c(-.6,.6))
