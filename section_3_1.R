rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Section 3.1 Autoregressive Moving Average Models

# Example 3.2
par(mfrow=c(2,1))
# in the expressions below, ~ is a space and == is equal
tsplot(arima.sim(list(order=c(1,0,0), ar=.9), n=100), ylab="x", main=(expression(AR(1)~~~phi==+.9)))
tsplot(arima.sim(list(order=c(1,0,0), ar=-.9), n=100), ylab="x", main=(expression(AR(1)~~~phi==-.9)))

# Example 3.5
par(mfrow=c(2,1))
tsplot(arima.sim(list(order=c(0,0,1), ma=.9), n=100), ylab="x", main=(expression(MA(1)~~~theta==+.9)))
tsplot(arima.sim(list(order=c(0,0,1), ma=-.9), n=100), ylab="x", main=(expression(MA(1)~~~theta==-.9)))

# Example 3.7
set.seed(8675309)         # Jenny, I got your number
x = rnorm(150, mean=5)    # Jenerate iid N(5,1)s
arima(x, order=c(1,0,1))  # Jenstimation

# Example 3.8
ARMAtoMA(ar = .9,  ma = .5,  10)   # first 10 psi-weights
ARMAtoMA(ar = -.5, ma = -.9, 10)   # first 10 pi-weights
