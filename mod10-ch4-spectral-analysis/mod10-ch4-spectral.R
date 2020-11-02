rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

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

# Example 4.5, 4.6, 4.7
par(mfrow=c(3,1))
arma.spec(log="no", main="White Noise")
arma.spec(ma=.5, log="no", main="Moving Average")
arma.spec(ar=c(1,-.9), log="no", main="Autoregression")

# Example 4.10
x      = c(1,2,3,2,1)
c1     = cos(2*pi*1:5*1/5)
s1     = sin(2*pi*1:5*1/5)
c2     = cos(2*pi*1:5*2/5)
s2     = sin(2*pi*1:5*2/5)
omega1 = cbind(c1, s1)
omega2 = cbind(c2, s2)
anova(lm(x~ omega1+omega2) )  # ANOVA Table
Mod(fft(x))^2/5               # the periodogram (as a check)

# Example 4.13
par(mfrow=c(2,1))
soi.per = mvspec(soi, log="no")
abline(v=1/4, lty="dotted")
rec.per = mvspec(rec, log="no")
abline(v=1/4, lty="dotted")

soi.per$spec[40]  # 0.97223;  soi pgram at freq 1/12 = 40/480
soi.per$spec[10]  # 0.05372;  soi pgram at freq 1/48 = 10/480

# conf intervals -  returned value:
U = qchisq(.025,2)    # 0.05063
L = qchisq(.975,2)    # 7.37775
2*soi.per$spec[10]/L  # 0.01456
2*soi.per$spec[10]/U  # 2.12220
2*soi.per$spec[40]/L  # 0.26355
2*soi.per$spec[40]/U  # 38.40108

# Repeat lines above using rec in place of soi

# Example 4.14
par(mfrow = c(1, 1))
soi.ave = mvspec(soi, kernel('daniell',4), log='no')
abline(v = c(.25,1,2,3), lty=2)
soi.ave$bandwidth      # = 0.225
df  = soi.ave$df       # df = 16.9875
U   = qchisq(.025, df) # U = 7.555916
L   = qchisq(.975, df) # L = 30.17425
soi.ave$spec[10]       # 0.0495202
soi.ave$spec[40]       # 0.1190800
# intervals
df*soi.ave$spec[10]/L  # 0.0278789
df*soi.ave$spec[10]/U  # 0.1113333
df*soi.ave$spec[40]/L  # 0.0670396
df*soi.ave$spec[40]/U  # 0.2677201

# Repeat above commands with soi replaced by rec, for example:
# rec.ave = mvspec(rec, k, log="no")
# abline(v=c(.25,1,2,3), lty=2)
# and so on.

# Example 4.15
t = seq(0, 1, by=1/200)  # WARNING: using t is bad pRactice because it's reserved- but let's be bad
amps = c(1, .5, .4, .3, .2, .1)
x = matrix(0, 201, 6)
for (j in 1:6) x[,j] = amps[j]*sin(2*pi*t*2*j)
x = ts(cbind(x, rowSums(x)), start=0, deltat=1/200)
ts.plot(x, lty=c(1:6, 1), lwd=c(rep(1,6), 2), ylab="Sinusoids")
names = c("Fundamental","2nd Harmonic","3rd Harmonic","4th Harmonic","5th Harmonic",
          "6th Harmonic","Formed Signal")
legend("topright", names, lty=c(1:6, 1), lwd=c(rep(1,6), 2))
rm(t)                    # Redemption

# Example 4.16
kernel("modified.daniell", c(3,3))          # for a list
plot(kernel("modified.daniell", c(3,3)))    # for a graph

k        = kernel("modified.daniell", c(3,3))
soi.smo  = mvspec(soi, kernel=k, taper=.1, log="no")
abline(v = c(.25,1), lty=2)
## Repeat above lines with rec replacing soi
df       = soi.smo$df    # df = 17.42618
soi.smo$bandwidth        # B  = 0.2308103

# An easier way to obtain soi.smo:
soi.smo = mvspec(soi, spans=c(7,7), taper=.1, log="no")

# Example 4.17
s0  = mvspec(soi, spans=c(7,7), plot=FALSE)            # no taper
s50 = mvspec(soi, spans=c(7,7), taper=.5, plot=FALSE)  # full taper
plot(s50$freq, s50$spec, log="y", type="l", ylab="spectrum", xlab="frequency")
lines(s0$freq, s0$spec, lty=2)

# Example 4.18
spaic = spec.ar(soi, log="no", ylim=c(0,.3))             # min AIC spec, order = 15
text(frequency(soi)*1/52, .07, substitute(omega==1/52))  # El Nino Cycle
text(frequency(soi)*1/12, .27, substitute(omega==1/12))  # Yearly Cycle
(soi.ar = ar(soi, order.max=30))                         # estimates and AICs
plot(1:30, soi.ar$aic[-1], type="o")                     # plot AICs


# Better comparison of pseudo-ICs
n = length(soi)
c() -> AIC -> AICc -> BIC
for (k in 1:30){
  fit = ar(soi, order=k, aic=FALSE)
  sigma2  = fit$var.pred
  BIC[k]  = log(sigma2) + (k*log(n)/n)
  AICc[k] = log(sigma2) + ((n+k)/(n-k-2))
  AIC[k]  = log(sigma2) + ((n+2*k)/n)
}

IC = cbind(AIC, BIC+1)
ts.plot(IC, type="o", xlab="p", ylab="AIC / BIC")
grid()
text(15.2, -1.48, "AIC")
text(15,   -1.35, "BIC")
