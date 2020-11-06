rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

x = rnorm(n=10)
plot(x)

s = c(rep(0,100), 10*exp(-(1:100)/200)*cos(2*pi*1:100/4))
x = s + rnorm(200)
plot.ts(x)




# Example 1.1
## Note: tsplot is an astsa version 1.7.7+ script
## you can change tsplot to plot for an uglier graphic (here and below)

# regular plot without a grid
plot(jj, type="o", ylab="Quarterly Earnings per Share")
# plot using library astsa. It has grid.
# This plot is buildup on top of the regular plot. 
tsplot(jj, type="o", ylab="Quarterly Earnings per Share")


# Example 1.2
tsplot(globtemp, type="o", ylab="Global Temperature Deviations")

# Example 1.3
tsplot(speech)

# Example 1.4
############################################################################
## the lines below are in the text but they don't work anymore ...
##  ... but the data set djia is included in astsa
## library(TTR)
## djia  = getYahooData("^DJI", start=20060420, end=20160420, freq="daily")
############################################################################
if (suppressWarnings(!require("xts"))) {
  install.packages("xts")
  library(xts)
}
djia
typeof(djia)
class(djia)
time(djia)
time(djia$Close)

plot(djia$Close)
head(diff(log(djia$Close)))
# Drop the first NA
djiar = diff(log(djia$Close))[-1]         # approximate returns
plot(djiar, main="DJIA Returns", type="n")
lines(djiar)

# Example 1.5
par(mfrow = c(2,1))  # set up the graphics
tsplot(soi, ylab="", main="Southern Oscillation Index")
tsplot(rec, ylab="", main="Recruitment")

# Example 1.6
par(mfrow=c(2,1), mar=c(3,2,1,0)+.5, mgp=c(1.6,.6,0))
# ts.plot is standard function. Plots several series at the same time
ts.plot(fmri1[,2:5], col=1:4, ylab="BOLD", xlab="", main="Cortex")
ts.plot(fmri1[,6:9], col=1:4, ylab="BOLD", xlab="", main="Thalamus & Cerebellum")
# Potentially, there is another standard function plot.ts
# The function tsplot() is from the book's library. Buildup on top of the regular. 
mtext("Time (1 pt = 2 sec)", side=1, line=2)

# Example 1.7
par(mfrow=c(2,1))
tsplot(EQ5, main="Earthquake")
tsplot(EXP6, main="Explosion")

# Example 1.9
w = rnorm(500,0,1)  # 500 N(0,1) variates
v = filter(w, sides=2, rep(1/3,3))  # moving average
par(mfrow=c(2,1))
tsplot(w, main="white noise")
tsplot(v, ylim=c(-3,3), main="moving average")

# now try this (not in text):
ts.plot(w, v, lty=2:1, col=1:2, lwd=1:2)

# Example 1.10
w = rnorm(550,0,1)  # 50 extra to avoid startup problems
x = filter(w, filter=c(1,-.9), method="recursive")
# What is recursive doing here? 
# Why do we need to drop the first 50?
# Most likely, to avoid NA's, it uses 2 extra zeros for the noise
# And since it was innatural, the consequences of this will disappear after a while.
# So we avoid more than just 2. 50 may be too big. 
# Hypothesis about recursive. 
# Because we said recursive, 
# the filter coefficients will apply not to a noise but to its own history.
# Then it is autoregressive model. 
length(x)
head(x)
x = x[-(1:50)]
tsplot(x, main="autoregression")


#-------------------------- 

# Example 1.11
set.seed(154) # so you can reproduce the results
par(mfrow=c(1,1))
w = rnorm(200); x = cumsum(w) # two commands in one line
length(x)
head(x)
# cumsum has the same length and no NA
# x(1) = w(1), x(2) = w(1) + w(2), ...
# Now we shift it by trend 0.2
wd = w +.2;    xd = cumsum(wd)
tsplot(xd, ylim=c(-5,55), main="random walk", ylab='')
# col=4 is a blue color
# First we plotted xd, the random walk with trend
# And then in blue we added the original walk by lines command
lines(x, col=4)
abline(h=0, col=4, lty=2)
abline(a=0, b=.2, lty=2)
