rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Section 3.9 Multiplicative Seasonal ARIMA Models

# Example 3.46
set.seed(666)
phi  = c(rep(0,11),.9)
sAR  = arima.sim(list(order=c(12,0,0), ar=phi), n=37)
sAR  = ts(sAR, freq=12)
layout(matrix(c(1,1,2, 1,1,3), nc=2))
par(mar=c(3,3,2,1), mgp=c(1.6,.6,0))
plot(sAR, axes=FALSE, main='seasonal AR(1)', xlab="year", type='c')
Months = c("J","F","M","A","M","J","J","A","S","O","N","D")
points(sAR, pch=Months, cex=1.25, font=4, col=1:4)
axis(1, 1:4)
abline(v=1:4, lty=2, col=gray(.7))
axis(2)
box()
ACF  = ARMAacf(ar=phi, ma=0, 100)
PACF = ARMAacf(ar=phi, ma=0, 100, pacf=TRUE)
plot(ACF,type="h", xlab="LAG", ylim=c(-.1,1))
abline(h=0)
plot(PACF, type="h", xlab="LAG", ylim=c(-.1,1))
abline(h=0)

# Example 3.47
phi  = c(rep(0,11),.8)
ACF  = ARMAacf(ar=phi, ma=-.5, 50)[-1] # [-1] removes 0 lag
PACF = ARMAacf(ar=phi, ma=-.5, 50, pacf=TRUE)
par(mfrow=c(1,2))
plot(ACF, type="h", xlab="LAG", ylim=c(-.4,.8)); abline(h=0)
plot(PACF, type="h", xlab="LAG", ylim=c(-.4,.8)); abline(h=0)

# Example 3.49
x     = AirPassengers
lx    = log(x)
dlx   = diff(lx)
ddlx  = diff(dlx, 12)
plot.ts(cbind(x, lx, dlx, ddlx), main="")
# below of interest for showing seasonal RW (not shown here):
par(mfrow=c(2,1))
monthplot(dlx)
monthplot(ddlx)

sarima(lx, 1,1,1, 0,1,1, 12)   # model 1
sarima(lx, 0,1,1, 0,1,1, 12)   # model 2 (the winner)
sarima(lx, 1,1,0, 0,1,1, 12)   # model 3

par(mfrow=c(1,1))
sarima.for(lx, 12, 0,1,1, 0,1,1,12)  # forecasts
