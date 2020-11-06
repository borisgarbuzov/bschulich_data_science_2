rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Section 4.3 Periodogram and Discrete Fourier Transform

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

# Section 4.4 Nonparametric Spectral Estimation

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
