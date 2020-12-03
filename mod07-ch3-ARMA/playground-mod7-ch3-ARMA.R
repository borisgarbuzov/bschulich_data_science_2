rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
    install.packages("astsa")
  library(astsa)
}


maseries = arima.sim(list(order=c(0,0,1), ma=.9), n=100000)
acf(maseries)



# Example 3.2
par(mfrow=c(2,1))
# in the expressions below, ~ is a space and == is equal
tsplot(arima.sim(list(order=c(1,0,0), ar=.9), n=100), ylab="x", main=(expression(AR(1)~~~phi==+.9)))
tsplot(arima.sim(list(order=c(1,0,0), ar=-.9), n=100), ylab="x", main=(expression(AR(1)~~~phi==-.9)))

# arima.sim function will reject coefficients if they don't satisfy condition -1 < phi < 1 for AR(1)
arima.sim(list(order = c(1, 0, 0), ar = 1), n = 100)
arima.sim(list(order = c(1, 0, 0), ar = -1), n = 100)

# Example 3.5
par(mfrow=c(2,1))
tsplot(arima.sim(list(order=c(0,0,1), ma=.9), n=100), ylab="x", main=(expression(MA(1)~~~theta==+.9)))
tsplot(arima.sim(list(order=c(0,0,1), ma=-.9), n=100), ylab="x", main=(expression(MA(1)~~~theta==-.9)))

# Example 3.7
# Parameter redundancy.
# White noise is fit to ARMA(1,1)
set.seed(8675309)         # Jenny, I got your number
x = rnorm(150, mean=5)    # Jenerate iid N(5,1)s
arima(x, order=c(1,0,1))  # Jenstimation

# Example 3.8
ARMAtoMA(ar = .9,  ma = .5,  10)   # first 10 psi-weights
ARMAtoMA(ar = -.5, ma = -.9, 10)   # first 10 pi-weights
# It alsways gives psi
# If we want pi, we swap ar and ma parts. 
# So pi coefficients for the first line 
# are psi coefficients for the second line. 
# The authors' package gives this command
ARMAtoAR(ar=.9, ma=.5, 10)

# B: Try the easy AR(1) with phi = 1/2
ARMAtoMA(ar = 1/2,  lag.max = 10) 
# psi = (1/2, 1/4, 1/8, ...)

# Example 3.11
z = c(1,-1.5,.75)    # coefficients of the polynomial
(a = polyroot(z)[1]) # = 1+0.57735i, print one root which is 1 + i 1/sqrt(3)
arg = Arg(a)/(2*pi)  # arg in cycles/pt
1/arg                # = 12, the period

set.seed(8675309)    # Jenny, it's me again
ar2 = arima.sim(list(order=c(2,0,0), ar=c(1.5,-.75)), n = 144)
plot(ar2, axes=FALSE, xlab="Time")
axis(2); axis(1, at=seq(0,144,by=12)); box()  # work the plot machine
abline(v=seq(0,144,by=12), lty=2)
acf(ar2)

ACF = ARMAacf(ar=c(1.5,-.75), ma=0, 50)
plot(ACF, type="h", xlab="lag")
abline(h=0)

# B: Simple AR(1), phi = 1/2
ar1ACF = ARMAacf(ar=c(.5), ma=0, 5)
ar1ACF

# R does not like non-causal AR
arima.sim(n = 3, model = list(ar = c(2)))
# Above we saw that R refuses to simulate non-causal ARMA, 
# But this time, when we do not specify the order, 
# it is OK for some reason. 
# And as always, it is fine to produce the ACF:
ARMAacf(ar=c(2), lag.max = 5)

# Example 3.12
par(mfrow=c(1, 1))
ARMAtoMA(ar=.9, ma=.5, 50)       #  for a list
plot(ARMAtoMA(ar=.9, ma=.5, 50)) #  for a graph
# These are psi coefficients

ARMAtoMA(ar = 2, ma = 3, lag.max=1)
