rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Section 4.1 Cyclical Behavior and Periodicity

# Example 4.1
x1 = 2*cos(2*pi*1:100*6/100)  + 3*sin(2*pi*1:100*6/100)
x2 = 4*cos(2*pi*1:100*10/100) + 5*sin(2*pi*1:100*10/100)
x3 = 6*cos(2*pi*1:100*40/100) + 7*sin(2*pi*1:100*40/100)
x = x1 + x2 + x3

par(mfrow=c(2,2))
tsplot(x1, ylim=c(-10,10), main = expression(omega==6/100~~~A^2==13))
tsplot(x2, ylim=c(-10,10), main = expression(omega==10/100~~~A^2==41))
tsplot(x3, ylim=c(-10,10), main = expression(omega==40/100~~~A^2==85))
tsplot(x, ylim=c(-16,16), main="sum")

# Example 4.2
P = abs(2*fft(x)/100)^2
Fr = 0:99/100
par(mfrow = c(1, 1))
plot(Fr, P, type="o", xlab="frequency", ylab="periodogram")

# Example 4.3
# modulation
t = 1:200
tsplot(x <- 2*cos(2*pi*.2*t)*cos(2*pi*.01*t))     # not shown
lines(cos(2*pi*.19*t)+cos(2*pi*.21*t), col=2)     # the same
Px = Mod(fft(x))^2; plot(0:199/200, Px, type='o') # the periodogram

# star mag analysis
n    = length(star)
par(mfrow=c(2,1), mar=c(3,3,1,1), mgp=c(1.6,.6,0))
tsplot(star, ylab="star magnitude", xlab="day")
Per   = Mod(fft(star-mean(star)))^2/n
Freq  = (1:n -1)/n
plot(Freq[1:50], Per[1:50], type='h', lwd=3, ylab="Periodogram", xlab="Frequency")
u     = which.max(Per[1:50])     # 22 freq=21/600=.035 cycles/day
uu    = which.max(Per[1:50][-u]) # 25 freq=25/600=.041 cycles/day
1/Freq[22]; 1/Freq[26]           # period = days/cycle
text(.05, 7000, "24 day cycle")
text(.027, 9000, "29 day cycle")
#- another way to find the two peaks is to order on Per
y = cbind(1:50, Freq[1:50], Per[1:50]); y[order(y[,3]),]

# Section 4.2 The Spectral Density

# Example 4.5, 4.6, 4.7
par(mfrow=c(3,1))
arma.spec(log="no", main="White Noise")
arma.spec(ma=.5, log="no", main="Moving Average")
arma.spec(ar=c(1,-.9), log="no", main="Autoregression")
