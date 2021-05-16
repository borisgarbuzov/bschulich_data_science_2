rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Section 3.8 Regression with Autocorrelated Errors

# Example 3.44
trend  = time(cmort)
temp   = tempr - mean(tempr)
temp2  = temp^2
summary(fit <- lm(cmort~trend + temp + temp2 + part, na.action=NULL))
acf2(resid(fit), 52) # implies AR2
sarima(cmort, 2,0,0, xreg=cbind(trend,temp,temp2,part) )

# Example 3.45
# Note: this could benefit from a seasonal model fit, but it hasn't
#  been talked about yet - you could come back to this after the next section
dummy = ifelse(soi<0, 0, 1)
fish = ts.intersect(rec, soiL6=lag(soi,-6), dL6=lag(dummy,-6), dframe=TRUE)
summary(fit <- lm(rec ~soiL6*dL6, data=fish, na.action=NULL))
attach(fish)
plot(resid(fit))
acf2(resid(fit))     # indicates AR(2)
intract = soiL6*dL6  # interaction term
sarima(rec,2,0,0, xreg = cbind(soiL6, dL6, intract))
# not in text, but this works better
# sarima(rec,2,0,0,0,1,1,12, xreg = cbind(soiL6, dL6, intract))
