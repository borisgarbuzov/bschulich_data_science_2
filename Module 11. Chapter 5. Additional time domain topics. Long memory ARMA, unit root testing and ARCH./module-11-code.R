rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Example 5.1
# # NOTE: I think 'fracdiff' is a dinosaur and I should have changed
# this in the new edition... so just below, I'll do it using 'arfima',
# which seems to work well
library(fracdiff)
lvarve = log(varve) - mean(log(varve))
varve.fd = fracdiff(lvarve, nar=0, nma=0, M=30)
varve.fd$d  # = 0.3841688
varve.fd$stderror.dpq  # = 4.589514e-06 (If you believe this, I have a bridge for sale.)

p = rep(1,31)
for (k in 1:30){ p[k+1] = (k-varve.fd$d)*p[k]/(k+1) }
plot(1:30, p[-1], ylab=expression(pi(d)), xlab="Index", type="h", lwd=2)
res.fd = diffseries(log(varve), varve.fd$d)       # frac diff resids
res.arima = resid(arima(log(varve), order=c(1,1,1))) # arima resids

par(mfrow=c(2,1))
acf(res.arima, 100, xlim=c(4,97), ylim=c(-.2,.2), main="arima resids")
acf(res.fd, 100, xlim=c(4,97), ylim=c(-.2,.2), main="frac diff resids")

# Example 5.1 redux
if (suppressWarnings(!require("arfima"))) {
  install.packages("arfima")
  library(arfima)
}

summary(varve.fd <- arfima(log(varve)))  # d.hat = 0.3728, se(d,hat) = 0.0273
# residual stuff
innov = resid(varve.fd)
plot.ts(innov[[1]])
acf(innov[[1]])

## ... much better ...  sorry I didn't ...
## ... get it in for the newest edition ..
## ... once in awhile, they slip on by ...

# Example 5.2
series = log(varve)  # specify series to be analyzed
d0 = .1              # initial value of d
n.per = nextn(length(series))
m = (n.per)/2  - 1
per = abs(fft(series-mean(series))[-1])^2  # remove 0 freq
per = per/n.per      # R doesn't scale fft by sqrt(n)
g = 4*(sin(pi*((1:m)/n.per))^2)

# Function to calculate -log.likelihood
whit.like = function(d){
  g.d=g^d
  sig2 = (sum(g.d*per[1:m])/m)
  log.like = m*log(sig2) - d*sum(log(g)) + m
  return(log.like)
}

# Estimation (?optim for details - output not shown)
(est = optim(d0, whit.like, gr=NULL, method="L-BFGS-B", hessian=TRUE, lower=-.5, upper=.5,
             control=list(trace=1,REPORT=1)))

# Results  [d.hat = .380, se(dhat) = .028]
cat("d.hat =", est$par, "se(dhat) = ",1/sqrt(est$hessian),"\n")
g.dhat = g^est$par
sig2 = sum(g.dhat*per[1:m])/m
cat("sig2hat =",sig2,"\n")  # sig2hat = .229

u = spec.ar(log(varve), plot=FALSE)  # produces AR(8)
g = 4*(sin(pi*((1:500)/2000))^2)
fhat = sig2*g^{-est$par}             # long memory spectral estimate
par(mfrow = c(1, 1))
plot(1:500/2000, log(fhat), type="l", ylab="log(spectrum)", xlab="frequency")
lines(u$freq[1:250], log(u$spec[1:250]), lty="dashed")
ar.mle(log(varve))                   # to get AR(8) estimates

# GPH estimate with big bandwidth or else estimate sucks
fdGPH(log(varve), bandw=.9)   # m = n^bandw

# Example 5.3
library(tseries)
adf.test(log(varve), k=0)  # DF test
adf.test(log(varve))       # ADF test
pp.test(log(varve))        # PP test

# Example 5.4
gnpgr = diff(log(gnp))          # get the returns
u     = sarima(gnpgr, 1, 0, 0)  # fit an AR(1)
acf2(resid(u$fit), 20)          # get (p)acf of the squared residuals

library(fGarch)
summary(garchFit(~arma(1,0)+garch(1,0), gnpgr))

# Example 5.5 and 5.6
library(xts)   # needed to handle djia
djiar = diff(log(djia$Close))[-1]
acf2(djiar)    # exhibits some autocorrelation (not shown)
acf2(djiar^2)  # oozes autocorrelation (not shown)
library(fGarch)
# GARCH fit
summary(djia.g <- garchFit(~arma(1,0)+garch(1,1), data=djiar, cond.dist='std'))
plot(djia.g)    # to see all plot options
# APARCH fit
summary(djia.ap <- garchFit(~arma(1,0)+aparch(1,1), data=djiar, cond.dist='std'))
plot(djia.ap)
