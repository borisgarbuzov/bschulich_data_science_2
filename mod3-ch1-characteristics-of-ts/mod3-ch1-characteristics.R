rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

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

# Example 1.9
w = rnorm(500,0,1)  # 500 N(0,1) variates
v = filter(w, sides=2, rep(1/3,3))  # moving average
par(mfrow=c(2,1))
tsplot(w, main="white noise")
tsplot(v, ylim=c(-3,3), main="moving average")

# now try this (not in text):
par(mfrow = c(1, 1))
ts.plot(w, v, lty=2:1, col=1:2, lwd=1:2)

# Example 1.10
w = rnorm(550,0,1)  # 50 extra to avoid startup problems
x = filter(w, filter=c(1,-.9), method="recursive")[-(1:50)]
tsplot(x, main="autoregression")

# Example 1.11
set.seed(154) # so you can reproduce the results
w = rnorm(200); x = cumsum(w) # two commands in one line
wd = w +.2;    xd = cumsum(wd)
tsplot(xd, ylim=c(-5,55), main="random walk", ylab='')
lines(x, col=4)
abline(h=0, col=4, lty=2)
abline(a=0, b=.2, lty=2)

# Example 1.12
cs = 2*cos(2*pi*(1:500)/50 + .6*pi)
w = rnorm(500,0,1)
par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)   # help(par) for info
tsplot(cs, ylab="", main = expression(x[t]==2*cos(2*pi*t/50+.6*pi)))
tsplot(cs + w, ylab="", main = expression(x[t]==2*cos(2*pi*t/50+.6*pi)+N(0,1)))
tsplot(cs + 5*w, ylab="", main = expression(x[t]==2*cos(2*pi*t/50+.6*pi)+N(0,25)))

# Example 1.24
set.seed(2)
x = rnorm(100)
y = lag(x, -5) + rnorm(100)
par(mfrow=c(1,1))
ccf(y, x, ylab='CCovF', type='covariance')
abline(v=0, lty=2)
text(11, .9, 'x leads')
text(-9, .9, 'y leads')

# Example 1.25
(r = round(acf(soi, 6, plot=FALSE)$acf[-1], 3)) # first 6 sample acf values
par(mfrow=c(1,2))
plot(lag(soi,-1), soi); legend('topleft', legend=r[1])
plot(lag(soi,-6), soi); legend('topleft', legend=r[6])

# Example 1.26
set.seed(101010)
x1 = 2*rbinom(11, 1, .5) - 1 # simulated sequence of coin tosses
x2 = 2*rbinom(101, 1, .5) - 1
y1 = 5 + filter(x1, sides=1, filter=c(1,-.7))[-1]
y2 = 5 + filter(x2, sides=1, filter=c(1,-.7))[-1]
tsplot(y1, type='s')  # plot series
tsplot(y2, type='s')
c(mean(y1), mean(y2))  # the sample means
acf(y1, lag.max=4, plot=FALSE)
acf(y2, lag.max=4, plot=FALSE)

# Example 1.27
par(mfrow=c(1,1))
acf1(speech, 250)

# Example 1.28
# acf1 and ccf2 are astsa v1.7.7+ scripts
# you can use acf and ccf instead
par(mfrow=c(3,1))
acf1(soi, 48, main="Southern Oscillation Index")
acf1(rec, 48, main="Recruitment")
ccf2(soi, rec, 48, main="SOI vs Recruitment")

# Example 1.29
set.seed(1492)
num=120; t=1:num
X = ts(2*cos(2*pi*t/12) + rnorm(num), freq=12)
Y = ts(2*cos(2*pi*(t+5)/12) + rnorm(num), freq=12)
Yw = resid( lm(Y~ cos(2*pi*t/12) + sin(2*pi*t/12), na.action=NULL) )
par(mfrow=c(3,2), mgp=c(1.6,.6,0), mar=c(3,3,1,1) )
tsplot(X)
tsplot(Y)
acf1(X, 48, ylab='ACF(X)')
acf1(Y, 48, ylab='ACF(Y)')
ccf2(X, Y, 24)
ccf2(X, Yw, 24, ylim=c(-.6,.6))

# Example 1.30
par(mfrow=c(1,1))
persp(1:64, 1:36, soiltemp, phi=30, theta=30, scale=FALSE, expand=4,
      ticktype="detailed", xlab="rows", ylab="cols", zlab="temperature")
tsplot(rowMeans(soiltemp), xlab="row", ylab="Average Temperature")

# Example 1.31
fs = abs(fft(soiltemp-mean(soiltemp)))^2/(64*36) # see Ch 4 for info on FFT
cs = Re(fft(fs, inverse=TRUE)/sqrt(64*36))  # ACovF
rs = cs/cs[1,1]                             # ACF

rs2 = cbind(rs[1:41,21:2], rs[1:41,1:21])   #  these lines are just to center
rs3 = rbind(rs2[41:2,], rs2)                #  the 0 lag

par(mar = c(1,2.5,0,0)+.1)
persp(-40:40, -20:20, rs3, phi=30, theta=30, expand=30, scale="FALSE",
      ticktype="detailed", xlab="row lags", ylab="column lags", zlab="ACF")
