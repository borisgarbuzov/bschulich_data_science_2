rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Section 3.2 Difference Equations

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

ACF = ARMAacf(ar=c(1.5,-.75), ma=0, 50)
plot(ACF, type="h", xlab="lag")
abline(h=0)

# Example 3.12
par(mfrow=c(1, 1))
ARMAtoMA(ar=.9, ma=.5, 50)       #  for a list
plot(ARMAtoMA(ar=.9, ma=.5, 50)) #  for a graph
