rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Example 3.16
ar2.acf = ARMAacf(ar=c(1.5,-.75), ma=0, 24)[-1]
ar2.pacf = ARMAacf(ar=c(1.5,-.75), ma=0, 24, pacf=TRUE)
par(mfrow=c(1,2))
plot(ar2.acf, type="h", xlab="lag")
abline(h=0)
plot(ar2.pacf, type="h", xlab="lag")
abline(h=0)

tsp(rec)
# Example 3.18
acf2(rec, 48)     # will produce values and a graphic
(regr = ar.ols(rec, order=2, demean=F, intercept=TRUE))  # regression
regr$asy.se.coef  # standard errors
# 0.04178901 0.04187942
regr
# Coefficients:
#   1        2  
# 1.3541  -0.4632 
# Intercept: 6.737 (1.111)

# Why do we use a new command?
# Let us compare it with the old one
regrOld = arima(rec, order = c(2, 0, 0))
#          ar1      ar2  intercept
#          1.3512  -0.4612    61.8585
# Coefficients are the same in old and new, 
# but the intercepts are different. 
# And it does not help to change the demean parameter. 

# Example 3.25
par(mfrow=c(1, 1))
regr = ar.ols(rec, order=2, demean=FALSE, intercept=TRUE)
fore = predict(regr, n.ahead=24)
# Plot 2 arrays in 2 colors
ts.plot(rec, fore$pred, col=1:2, xlim=c(1980,1990), ylab="Recruitment")
# Add red circles over red line for some reason. 
lines(fore$pred, type="p", col=2)
lines(fore$pred+fore$se, lty="dashed", col=4)
lines(fore$pred-fore$se, lty="dashed", col=4)
# There is no true future to compare. 
# We could also use forecast::forecast, 
# but this is simpler. 
# It is sensible to use forecast::forecast, when using forecast::auto.arima.

# Example 3.26
set.seed(90210)
x = arima.sim(list(order = c(1,0,1), ar =.9, ma=.5), n = 100)
xr = rev(x) # xr is the reversed data
pxr = predict(arima(xr, order=c(1,0,1)), 10) # predict previous 10 before reversed data
pxrp = rev(pxr$pred) # reorder the predictors (for plotting)
pxrse = rev(pxr$se) # reorder the SEs
nx = ts(c(pxrp, x), start=-9) # attach the backcasts to the data
tsplot(nx, ylab=expression(X[~t]), main='Backcasting')
U = nx[1:10] + pxrse; L = nx[1:10] - pxrse
xx = c(-9:0, 0:-9); yy = c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(0.6, alpha = 0.2))
lines(-9:0, nx[1:10], col=2, type='o')

# This finishes the forecasting section 3.4
