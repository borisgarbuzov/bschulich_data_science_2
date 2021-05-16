rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Section 1.1 The Nature of Time Series Data

# Example 1.1
## Note: tsplot is an astsa version 1.7.7+ script
## you can change tsplot to plot for an uglier graphic (here and below)

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
djiar = diff(log(djia$Close))[-1]         # approximate returns
plot(djiar, main="DJIA Returns", type="n")
lines(djiar)

# Example 1.5
par(mfrow = c(2,1))  # set up the graphics
tsplot(soi, ylab="", main="Southern Oscillation Index")
tsplot(rec, ylab="", main="Recruitment")

# Example 1.6
par(mfrow=c(2,1), mar=c(3,2,1,0)+.5, mgp=c(1.6,.6,0))
ts.plot(fmri1[,2:5], col=1:4, ylab="BOLD", xlab="", main="Cortex")
ts.plot(fmri1[,6:9], col=1:4, ylab="BOLD", xlab="", main="Thalamus & Cerebellum")
mtext("Time (1 pt = 2 sec)", side=1, line=2)

# Example 1.7
par(mfrow=c(2,1))
tsplot(EQ5, main="Earthquake")
tsplot(EXP6, main="Explosion")
