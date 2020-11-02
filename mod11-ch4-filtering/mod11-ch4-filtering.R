rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Example 4.22
par(mfrow=c(3,1))
tsplot(soi)                         # plot data
tsplot(diff(soi))                   # plot first difference
k = kernel("modified.daniell", 6)   # filter weights
tsplot(soif <- kernapply(soi, k))   # plot 12 month filter
par(mfrow = c(1 ,1))
spectrum(soif, spans=9, log="no") # spectral analysis (not shown)
abline(v=12/52, lty="dashed")
##-- frequency responses --##
par(mfrow=c(2,1), mar=c(3,3,1,1), mgp=c(1.6,.6,0))
w = seq(0, .5, by=.01)
FRdiff = abs(1-exp(2i*pi*w))^2
plot(w, FRdiff, type='l', xlab='frequency')
u = cos(2*pi*w)+cos(4*pi*w)+cos(6*pi*w)+cos(8*pi*w)+cos(10*pi*w)
FRma = ((1 + cos(12*pi*w) + 2*u)/12)^2
plot(w, FRma, type='l', xlab='frequency')

# Example 4.24 THIS EXAMPLE DOESN'T WORK FOR NOW BECAUSE OF NA
# LagReg(soi, rec, L=15, M=32, threshold=6)
# LagReg(rec, soi, L=15, M=32, inverse=TRUE, threshold=.01)
# # armax model
# fish = ts.intersect(R=rec, RL1=lag(rec,-1), SL5=lag(soi,-5))
# (u = lm(fish[,1]~fish[,2:3], na.action=NULL))
# acf2(resid(u))       # suggests ar1
# sarima(fish[,1], 1, 0, 0, xreg=fish[,2:3])

# Example 4.25
SigExtract(soi, L=9, M=64, max.freq=.05)

# Example 4.26
per = abs(fft(soiltemp-mean(soiltemp))/sqrt(64*36))^2
per2 = cbind(per[1:32,18:2], per[1:32,1:18])   # this and line below is just rearranging
per3 = rbind(per2[32:2,], per2)                # results to get 0 frequency in the middle

par(mfrow = c(1, 1), mar=c(1,2.5,0,0)+.1)
persp(-31:31/64, -17:17/36, per3, phi=30, theta=30, expand=.6, ticktype="detailed", xlab="cycles/row",
      ylab="cycles/column", zlab="Periodogram Ordinate")
