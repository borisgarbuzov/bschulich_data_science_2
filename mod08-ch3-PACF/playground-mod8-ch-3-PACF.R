rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Example 3.16
ar2.acf = ARMAacf(ar=c(1.5,-.75), ma=0, 24)[-1]
ar2.acf
length(ar2.acf)
# There was 25 values initially, before we dropped the zero lag
ar2.pacf = ARMAacf(ar=c(1.5,-.75), ma=0, 24, pacf=TRUE)
ar2.pacf
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
