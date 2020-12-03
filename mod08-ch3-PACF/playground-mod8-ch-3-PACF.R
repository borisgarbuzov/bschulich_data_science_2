rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Example 3.16
ar2.acf = ARMAacf(ar=c(1.5,-.75), ma=0, 24)[-1]
length(ar2.acf)
# There was 25 values initially, before we dropped the zero lag
ar2.pacf = ARMAacf(ar=c(1.5,-.75), ma=0, 24, pacf=TRUE)
# PACF does not have zero lag from the very start. 
# So nothing to drop
par(mfrow=c(1,2))
plot(ar2.acf, type="h", xlab="lag")
abline(h=0)
plot(ar2.pacf, type="h", xlab="lag")
abline(h=0)
# Moral is that PACF of AR(2) has 2 significant lags


tsp(rec)
# -----------Example 3.18------------------- 
# acf2 uses the whole series and estimates the acf and pacf
# We can not give it coefficients. 
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
# 1.3541  -0.4632
# s = 1.3541  -0.4632 = 0.8909 
# 1 - 0.8909 = 0.1091 
# 61.8585 * 0.1091 = 6.7488 
# the other way to ask mean of rec
mean(rec)
# 62.26278
62.26278 * 0.1091
# And if we substitute this, 



#---------------------- 
# Try bigger order p
p = 3
(regr3 = ar.ols(rec, order=p, demean=F, intercept=TRUE))  # regression
# Anyway it prints 2 coefficients,
# even though I gave p=3.
p=4
(regr4 = ar.ols(rec, order=p, demean=F, intercept=TRUE))  # regression

library(forecast)
autoregr = auto.arima(rec)
autoregr
# Now it detected ARIMA(1,1,0)(0,0,2)
# which is much more. 
  

#=============Book section 3.4. Forecasting ===================== 
# --------------- Example 3.25 ---------------------- 
# Add confidence interval for fitted values
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
fit = arima(xr, order=c(1,0,1))
fit
# The results of fit are: 
# AR coefficient phi1Hat = 0.7800 instead of true phi1 = 0.9
# MA coefficient theta1Hat = 0.5991 instead of true theta1 = 0.5
# interceptHat = 1.7963 instead of true intercept = 0
pxr = predict(fit, n.ahead = 10) # predict previous 10 before reversed data
pxrp = rev(pxr$pred) # reorder the predictors (for plotting)
pxrse = rev(pxr$se) # reorder the SEs
nx = ts(c(pxrp, x), start=-9) # attach the backcasts to the data
tsplot(nx, ylab=expression(X[~t]), main='Backcasting')
U = nx[1:10] + pxrse; L = nx[1:10] - pxrse
# Prepare x and y for polygon
xx = c(-9:0, 0:-9); yy = c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(0.6, alpha = 0.2))
lines(-9:0, nx[1:10], col=2, type='o')

# This finishes the forecasting section 3.4
