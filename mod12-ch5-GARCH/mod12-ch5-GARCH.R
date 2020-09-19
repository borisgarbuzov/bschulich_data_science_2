rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Example 5.7
tsplot(flu, type="c")
Months = c("J","F","M","A","M","J","J","A","S","O","N","D")
points(flu, pch=Months, cex=.8, font=2)
# Start analysis
dflu = diff(flu)
lag1.plot(dflu, corr=FALSE) # scatterplot with lowess fit
thrsh = .05 # threshold
Z = ts.intersect(dflu, lag(dflu,-1), lag(dflu,-2), lag(dflu,-3),
                 lag(dflu,-4) )
ind1 = ifelse(Z[,2] < thrsh, 1, NA) # indicator < thrsh
ind2 = ifelse(Z[,2] < thrsh, NA, 1) # indicator >= thrsh
X1 = Z[,1]*ind1
X2 = Z[,1]*ind2
summary(fit1 <- lm(X1~ Z[,2:5]) ) # case 1
summary(fit2 <- lm(X2~ Z[,2:5]) ) # case 2
D = cbind(rep(1, nrow(Z)), Z[,2:5]) # design matrix
p1 = D %*% coef(fit1) # get predictions
p2 = D %*% coef(fit2)
prd = ifelse(Z[,2] < thrsh, p1, p2)
plot(dflu, ylim=c(-.5,.5), type='p', pch=3)
lines(prd)
prde1 = sqrt(sum(resid(fit1)^2)/df.residual(fit1) )
prde2 = sqrt(sum(resid(fit2)^2)/df.residual(fit2) )
prde = ifelse(Z[,2] < thrsh, prde1, prde2)
tx = time(dflu)[-(1:4)]
xx = c(tx, rev(tx))
yy = c(prd-2*prde, rev(prd+2*prde))
polygon(xx, yy, border=8, col=gray(.6, alpha=.25) )
abline(h=.05, col=4, lty=6)

# Using tsDyn (not in text)
if (suppressWarnings(!require("tsDyn"))) {
  install.packages("tsDyn")
  library(tsDyn)
}

# vignette("tsDyn")   # for package details (it's quirky, so you'll need this)
dflu = diff(flu)
(u = setar(dflu, m=4, thDelay=0))  # fit model and view results (thDelay=0 is lag 1 delay)
# BIC(u) THIS ONE DOESN'T WORK
AIC(u)                     # if you want to try other models ... m=3 works well too
plot(u)                            # graphics -  ?plot.setar for information

# Example 5.8 and 5.9
soi.d   = resid(lm(soi~time(soi), na.action=NULL)) # detrended SOI
acf2(soi.d)
fit     = arima(soi.d, order=c(1,0,0))
ar1     = as.numeric(coef(fit)[1]) # = 0.5875
soi.pw  = resid(fit)
rec.fil = filter(rec, filter=c(1, -ar1), sides=1)
ccf2(soi.pw, rec.fil, na.action=na.omit)

fish  = ts.intersect(rec, RL1=lag(rec,-1), SL5=lag(soi.d,-5))
(u    = lm(fish[,1]~fish[,2:3], na.action=NULL))
acf2(resid(u)) # suggests ar1
(arx  = sarima(fish[,1], 1, 0, 0, xreg=fish[,2:3])) # final model
pred  = rec + resid(arx$fit) # 1-step-ahead predictions
ts.plot(pred, rec, col=c('gray90',1), lwd=c(7,1))

# Example 5.10 and 5.11
library(vars)
x = cbind(cmort, tempr, part)
summary(VAR(x, p=1, type="both"))  # "both" fits constant + trend

VARselect(x, lag.max=10, type="both")
summary(fit <- VAR(x, p=2, type="both"))
acf(resid(fit), 52)
serial.test(fit, lags.pt=12, type="PT.adjusted")

(fit.pr = predict(fit, n.ahead = 24, ci = 0.95))  # 4 weeks ahead
fanchart(fit.pr)  # plot prediction + error

# Example 5.12
if (suppressWarnings(!require("marima"))) {
  install.packages("marima")
  library(marima)
}
model   = define.model(kvar=3, ar=c(1,2), ma=c(1))
arp     = model$ar.pattern
map     = model$ma.pattern
cmort.d = resid(detr <- lm(cmort~ time(cmort), na.action=NULL))
xdata   = matrix(cbind(cmort.d, tempr, part), ncol=3)  # strip ts attributes
fit     = marima(xdata, ar.pattern=arp, ma.pattern=map, means=c(0,1,1), penalty=1)
# resid analysis (not displayed)
innov   = t(resid(fit))
plot.ts(innov)
# acf(innov) DOESN'T WORK BECAUSE OF NA
# fitted values for cmort
pred    = ts(t(fitted(fit))[,1], start=start(cmort), freq=frequency(cmort)) +
  detr$coef[1] + detr$coef[2]*time(cmort)
plot(pred, ylab="Cardiovascular Mortality", lwd=2, col=4)
points(cmort)
# print estimates and corresponding t^2-statistic
short.form(fit$ar.estimates, leading=FALSE)
short.form(fit$ar.fvalues,   leading=FALSE)
short.form(fit$ma.estimates, leading=FALSE)
short.form(fit$ma.fvalues,   leading=FALSE)
fit$resid.cov # estimate of noise cov matrix
