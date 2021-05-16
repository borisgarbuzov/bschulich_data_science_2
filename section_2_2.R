rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Section 2.2 Exploratory Data Analysis

# Example 2.4 and 2.5
fit = lm(chicken~time(chicken), na.action=NULL) # regress chicken on time
par(mfrow=c(2,1))
tsplot(resid(fit), main="detrended")
tsplot(diff(chicken), main="first difference")

par(mfrow=c(3,1))     # plot ACFs
acf1(chicken, 48, main="chicken")
acf1(resid(fit), 48, main="detrended")
acf1(diff(chicken), 48, main="first difference")

# Example 2.6
par(mfrow=c(2,1))
tsplot(diff(globtemp), type="o")
mean(diff(globtemp))     # drift estimate = .008
acf1(diff(gtemp), 48, main="")

# Example 2.7
par(mfrow=c(2,1))
tsplot(varve, main="varve", ylab="")
tsplot(log(varve), main="log(varve)", ylab="" )

# Example 2.8
lag1.plot(soi, 12)
lag2.plot(soi, rec, 8)

# Example 2.9
dummy = ifelse(soi<0, 0, 1)
fish  = ts.intersect(rec, soiL6=lag(soi,-6), dL6=lag(dummy,-6), dframe=TRUE)
summary(fit <- lm(rec~ soiL6*dL6, data=fish, na.action=NULL))
attach(fish)
par(mfrow=c(1, 1))
plot(soiL6, rec)
lines(lowess(soiL6, rec), col=4, lwd=2)
points(soiL6, fitted(fit), pch='+', col=2)
tsplot(resid(fit)) # not shown ...
acf1(resid(fit))   # ... but obviously not noise

# Example 2.10
set.seed(1000)  # so you can reproduce these results
x = 2*cos(2*pi*1:500/50 + .6*pi) + rnorm(500,0,1)
z1 = cos(2*pi*1:500/50)
z2 = sin(2*pi*1:500/50)
summary(fit <- lm(x~0+z1+z2))  # zero to exclude the intercept
par(mfrow=c(2,1))
tsplot(x)
tsplot(x, col=8, ylab=expression(hat(x)))
lines(fitted(fit), col=2)
