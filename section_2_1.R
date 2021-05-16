rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Section 2.1 Classical Regression in the Time Series Context

# Example 2.1
summary(fit <- lm(chicken~time(chicken))) # regress price on time
tsplot(chicken, ylab="cents per pound", col=4, lwd=2)
abline(fit)           # add the fitted regression line to the plot

# Example 2.2
par(mfrow=c(3,1))
tsplot(cmort, main="Cardiovascular Mortality", ylab="")
tsplot(tempr, main="Temperature",  ylab="")
tsplot(part, main="Particulates", ylab="")

pairs(cbind(Mortality=cmort, Temperature=tempr, Particulates=part))

#  Regression
temp  = tempr-mean(tempr)  # center temperature
temp2 = temp^2             # square it
trend = time(cmort)        # time

fit = lm(cmort~ trend + temp + temp2 + part, na.action=NULL)

summary(fit)       # regression results
summary(aov(fit))  # ANOVA table   (compare to next line)
summary(aov(lm(cmort~cbind(trend, temp, temp2, part)))) # Table 2.1

num = length(cmort)                                     # sample size
AIC(fit)/num - log(2*pi)                                # AIC
BIC(fit)/num - log(2*pi)                                # BIC
# AIC(fit, k=log(num))/num - log(2*pi)                  # BIC (alt method)
(AICc = log(sum(resid(fit)^2)/num) + (num+5)/(num-5-2)) # AICc

# Example 2.3
fish = ts.intersect(rec, soiL6=lag(soi,-6), dframe=TRUE)
summary(fit <- lm(rec~soiL6, data=fish, na.action=NULL))
par(mfrow=c(1, 1))
tsplot(fish$rec, ylim=c(0,111))  # plot the data and the fitted values (not shown in text)
lines(fitted(fit), col=2)
