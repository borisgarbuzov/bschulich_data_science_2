if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# experiment related to Example 2.1------------------ 
chicken
dim(chicken)
is.ts(chicken)
chickenMatrix = as.matrix(chicken)
dim(chickenMatrix)
tsp(chicken)
time(chicken)
plot(chicken)
# ---------------------------- 
# Example 2.1
summary(fit <- lm(chicken~time(chicken))) # regress price on time
tsplot(chicken, ylab="cents per pound", col=4, lwd=2)
abline(fit)           # add the fitted regression line to the plot

# ----------------------------- 
# Example 2.2
par(mfrow=c(3,1))
tsplot(cmort, main="Cardiovascular Mortality", ylab="")
tsplot(tempr, main="Temperature",  ylab="")
tsplot(part, main="Particulates", ylab="")

pairs(cbind(Mortality=cmort, Temperature=tempr, Particulates=part))

# Regression
temp  = tempr-mean(tempr)  # center temperature
temp2 = temp^2             # square it
trend = time(cmort)        # time

# only the fullest model is shown here?
fit = lm(cmort~ trend + temp + temp2 + part, na.action=NULL)

summary(fit)       # regression results
summary(aov(fit))  # ANOVA table   (compare to next line)
summary(aov(lm(cmort~cbind(trend, temp, temp2, part)))) # Table 2.1

num = length(cmort)                                     # sample size
AIC(fit)/num - log(2*pi)                                # AIC
BIC(fit)/num - log(2*pi)                                # BIC
# AIC(fit, k=log(num))/num - log(2*pi)                  # BIC (alt method)
(AICc = log(sum(resid(fit)^2)/num) + (num+5)/(num-5-2)) # AICc

# ----- Experiments for Example 2.2 ------------------- 
plot(temp)
plot(temp2)
plot(trend)
hist(temp)
hist(temp2)
trend = time(cmort) 
trend
myTsp = tsp(cmort)
myTsp
# plot(myTsp) # just 3 points
plot(trend)

# -------------- Example 2.3 ---------------------- 
# Bind several time series, having the same frequency
# Perhaps, it intersects the common time intervals
fish = ts.intersect(rec, soiL6=lag(soi,-6), dframe=TRUE)
summary(fit <- lm(rec~soiL6, data=fish, na.action=NULL))
par(mfrow=c(1, 1))
tsplot(fish$rec, ylim=c(0,111))  # plot the data and the fitted values (not shown in text)
lines(fitted(fit), col=2)

#----------- Experiments for Example 2.3
tsp(rec)
tsp(soi)
soiLagged6 = lag(soi,-6)
tsp(soiLagged6)

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
lag1.plot(series = soi, max.lag = 12)
# In the above, the 0 lag is dropped
# But below, it is kept, because it is already non-trivial
lag2.plot(series1 = soi, series2 = rec, max.lag = 8)

# Example 2.9
dummy = ifelse(soi<0, 0, 1)
dummy
length(dummy)
sum(dummy)
unique(dummy)
table(dummy)
fish  = ts.intersect(rec, soiL6=lag(soi,-6), dL6=lag(dummy,-6), dframe=TRUE)
summary(fit <- lm(rec~ soiL6*dL6, data=fish, na.action=NULL))
attach(fish)
par(mfrow=c(1, 1))
plot(soiL6, rec)
lines(lowess(soiL6, rec), col=4, lwd=2)
points(soiL6, fitted(fit), pch='+', col=2)
tsplot(resid(fit)) # not shown ...
acf1(resid(fit))   # ... but obviously not noise

# 3d scatter experiment
if (! require("plot3D"))
  install.packages("plot3D")
library("plot3D")
scatter3D(x=x1, y=x2, z=y) 
# Example 2.10 experiment
set.seed(1000)  # so you can reproduce these results
# no noise to see the structure
x = 2*cos(2*pi*1:500/50 + .6*pi) 
x = 2*cos(2*pi*1:500/50 + .6*pi) + rnorm(500,0,5)/10
z1 = cos(2*pi*1:500/50)
z2 = sin(2*pi*1:500/50)
scatter3D(x=z1, y=z2, z=x) 

# Example 2.10
set.seed(1000)  # so you can reproduce these results
x = 2*cos(2*pi*1:500/50 + .6*pi) + rnorm(500,0,5)
z1 = cos(2*pi*1:500/50)
z2 = sin(2*pi*1:500/50)
scatter3D(x=z1, y=z2, z=x) 

summary(fit <- lm(x~0+z1+z2))  # zero to exclude the intercept
par(mfrow=c(2,1))
tsplot(x)
tsplot(x, col=8, ylab=expression(hat(x)))
lines(fitted(fit), col=2)

# we can see our fitted surface if do something like this
par(mfrow = c(1, 1))
z <- matrix(fitted(fit), nrow = 500, ncol = 500)
persp(x = 1:500, y = 1:500, z = z, theta = 30, phi = 30, expand = 0.5, 
      col=hcl(255,100,80), border="white", lwd = 0.5)

# Experiment for multiple regression
x1 = c(1, 2, 3, 4)
x2 = c(1, 2, 3, 4)
y = c(1, 1, 3, 5)
multipleFit = lm(y ~ x1 + x2)
zFitted <- matrix(fitted(multipleFit), nrow=length(x1), ncol=length(x2))
dim(zFitted)
zFitted
par(mfrow=c(1, 2))
plot(fitted(multipleFit))
persp(x1, x2, zFitted, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
# persp(x1, x2, y, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
# The above does not work so far. We need y to be a matrix

# Try to complicate the case by repeating x values
x1 = c(1, 2, 3, 4, 4)
x2 = c(1, 2, 3, 4, 4)
y = c(1, 1, 3, 5, 2)
multipleFit = lm(y ~ x1 + x2)
zFitted <- matrix(fitted(multipleFit), nrow=length(x1), ncol=length(x2))
dim(zFitted)
zFitted
par(mfrow=c(1, 2))
plot(fitted(multipleFit))
persp(x1, x2, zFitted, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
# wants increasing values

# =========== Smoothing =========================== 
# Example 2.11
wgts = c(.5, rep(1,11), .5)/12
soif = filter(soi, sides=2, filter=wgts)
par(mfrow=c(1, 1))
tsplot(soi)
lines(soif, lwd=2, col=4)
par(fig = c(.75, 1, .75, 1), new = TRUE) # the insert
nwgts = c(rep(0,20), wgts, rep(0,20))
plot(nwgts, type="l", ylim = c(-.02,.1), xaxt='n', yaxt='n', ann=FALSE)

# Example 2.12
par(mfrow = c(1, 1))
tsplot(soi)
lines(ksmooth(x = time(soi), y = soi, "normal", bandwidth=1), lwd=2, col=4)
par(fig = c(.75, 1, .75, 1), new = TRUE) # the insert
gauss = function(x) { 1/sqrt(2*pi) * exp(-(x^2)/2) }
x = seq(from = -3, to = 3, by = 0.001)
plot(x, gauss(x), type ="l", ylim=c(-.02,.45), xaxt='n', yaxt='n', ann=FALSE)

# Example 2.13
par(mfrow=c(1, 1))
tsplot(soi)
lines(lowess(soi, f=.05), lwd=2, col=4) # El Nino cycle
lines(lowess(soi), lty=2, lwd=2, col=2) # trend (with default span)

# Example 2.14. Splines
tsplot(soi)
lines(smooth.spline(time(soi), soi, spar=.5), lwd=2, col=4)
lines(smooth.spline(time(soi), soi, spar= 1), lty=2, lwd=2, col=2)

# Example 2.15
plot(tempr, cmort, xlab="Temperature", ylab="Mortality")
lines(lowess(tempr, cmort))

# quiz practice
library(astsa)
lowess1 = lowess(soi, f=1/10)
lowess2 = lowess(soi, f=2/3)
plot.ts(soi)
lines(lowess(soi, f=1/10), lwd = 2, col = "red")
lines(lowess(soi, f=2/3), lwd = 2, col = "blue")

# Theoretical quiz question on smoothing
x = c(1, 2, 3)
y = c(5, 7, 6)
plot(x, y, type="o")
abline(v=1.5)

