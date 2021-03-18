rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

#=============Book section 3.4. Forecasting ===================== 
# --------------- Example 3.25 ---------------------- 
# Add confidence interval for fitted values
par(mfrow=c(1, 1))
regr = ar.ols(rec, order=2, demean=FALSE, intercept=TRUE)
fore = predict(regr, n.ahead=24)
# Plot 2 arrays in 2 colors
ts.plot(rec, fore$pred, col=1:2, xlim=c(1980,1990), ylab="Recruitment")
# Add red circles over red line for some reason. 
lines(fore$pred, type="p", col=2)
lines(fore$pred+fore$se, lty="dashed", col=4)
lines(fore$pred-fore$se, lty="dashed", col=4)
# There is no true future to compare. 
# We could also use forecast::forecast, 
# but this is simpler. 
# It is sensible to use forecast::forecast, when using forecast::auto.arima.
# stopped here

# Example 3.26
set.seed(90210)
x = arima.sim(list(order = c(1,0,1), ar =.9, ma=.5), n = 100)
xr = rev(x) # xr is the reversed data
xr
typeof(xr)
class(xr)
fit = arima(xr, order=c(1,0,1))
fit
# The results of fit are: 
# AR coefficient phi1Hat = 0.7800 instead of true phi1 = 0.9
# MA coefficient theta1Hat = 0.5991 instead of true theta1 = 0.5
# interceptHat = 1.7963 instead of true intercept = 0
pxr = predict(fit, n.ahead = 10) # predict previous 10 before reversed data
pxrp = rev(pxr$pred) # reorder the predictors (for plotting)
pxrse = rev(pxr$se) # reorder the SEs
# start=-9 because n.ahead = 10 minus 1 (starting time)
nx = ts(c(pxrp, x), start=-9) # attach the backcasts to the data
tsplot(nx, ylab=expression(X[~t]), main='Backcasting')
U = nx[1:10] + pxrse; L = nx[1:10] - pxrse
# Prepare x and y for polygon
xx = c(-9:0, 0:-9); yy = c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(0.6, alpha = 0.2))
lines(-9:0, nx[1:10], col=2, type='o')

# Section 3.5 Estimation

# Example 3.28
rec.yw = ar.yw(rec, order=2) # fit
# Experiments------------
names(rec.yw)
rec.yw$order
rec.yw$ar
rec.yw$aic
rec.yw$n.used
rec.yw$n.obs # This may be smaller, excluding NA
rec.yw$order.max
rec.yw$partialacf # Lags up to order max
rec.yw$partialacf
rec.yw$partialacf
rec.yw$partialacf
rec.yw$resid
rec.yw$method
rec.yw$series # name
rec.yw$method
rec.yw$frequency # inhereted from ts
rec.yw$call
rec.yw$frequency
rec.yw$asy.var.coef # var-cov matrix of coef vector
# End of experiments------------

rec.yw$x.mean  # = 62.26278 (mean estimate)
rec.yw$ar      # = 1.3315874, -.4445447  (parameter estimates)
sqrt(diag(rec.yw$asy.var.coef))  # = .04222637, .04222637  (standard errors)
rec.yw$var.pred  # = 94.79912 (error variance estimate)


rec.pr = predict(rec.yw, n.ahead=24)
U = rec.pr$pred + rec.pr$se
L = rec.pr$pred - rec.pr$se
minx = min(rec,L); maxx = max(rec,U)
ts.plot(rec, rec.pr$pred, xlim=c(1980,1990), ylim=c(minx,maxx))
lines(rec.pr$pred, col="red", type="o")
lines(U, col="blue", lty="dashed")
lines(L, col="blue", lty="dashed")

# Example 3.29
set.seed(2)
trueTheta = 0.9
ma1 = arima.sim(list(order = c(0,0,1), ma = trueTheta), n = 50)
rhoHat1 = acf(ma1, plot=FALSE)[1]  # = .507 (lag 1 sample ACF)
rhoHat1 = rhoHat1$acf[1][1]
rhoHat1^2
thetaHat = (1- sqrt(1 - 4*rhoHat1^2)) / (2*rhoHat1)
thetaHat 
trueTheta

# -------Maximum likelihood---------------------

# Example 3.31
# Note: I'm not convinced this is really the MLE...
#  ... but eventually 'sarima()' will be used
rec.mle = ar.mle(rec, order=2)
rec.mle$x.mean
rec.mle$ar
sqrt(diag(rec.mle$asy.var.coef))
rec.mle$var.pred

# Example 3.33
x = diff(log(varve))
# Evaluate Sc on a Grid
c(0) -> w -> z
c() -> Sc -> Sz -> Szw
num = length(x)
th = seq(-.3,-.94,-.01)
for (p in 1:length(th)){
  for (i in 2:num){ w[i] = x[i]-th[p]*w[i-1] }
  Sc[p] = sum(w^2) }
plot(th, Sc, type="l", ylab=expression(S[c](theta)), xlab=expression(theta),
     lwd=2)
# Gauss-Newton Estimation
r = acf(x, lag=1, plot=FALSE)$acf[-1]
rstart = (1-sqrt(1-4*(r^2)))/(2*r) # from (3.105)
c(0) -> w -> z
c() -> Sc -> Sz -> Szw -> para
niter = 12
para[1] = rstart
for (p in 1:niter){
  for (i in 2:num){ w[i] = x[i]-para[p]*w[i-1]
  z[i] = w[i-1]-para[p]*z[i-1] }
  Sc[p] = sum(w^2)
  Sz[p] = sum(z^2)
  Szw[p] = sum(z*w)
  para[p+1] = para[p] + Szw[p]/Sz[p] }

round(cbind(iteration=0:(niter-1), thetahat=para[1:niter] , Sc , Sz ), 3)
abline(v = para[1:12], lty=2)
points(para[1:12], Sc[1:12], pch=16)

# Example 3.36
# generate data
set.seed(101010)
e   = rexp(150, rate=.5)
u   = runif(150,-1,1)
de  = e*sign(u)
dex = 50 + arima.sim(n=100, list(ar=.95), innov=de, n.start=50)
tsplot(dex, type='o', ylab=expression(X[~t]))

# small sample and asymptotic distn
set.seed(111)
phi.yw = rep(NA, 1000)
for (i in 1:1000){
  e = rexp(150, rate=.5); u = runif(150,-1,1); de = e*sign(u)
  x = 50 + arima.sim(n=100,list(ar=.95), innov=de, n.start=50)
  phi.yw[i] = ar.yw(x, order=1)$ar
}
hist(phi.yw, prob=TRUE, main="", ylim=c(0,14), xlim=c(.70,1.05))
lines(density(phi.yw, bw=.015))
u = seq(.75, 1.1, by=.001)
lines(u, dnorm(u, mean=.96, sd=.03), lty=2, lwd=2)

# Bootstrap
set.seed(666)                 # not that 666
fit     = ar.yw(dex, order=1) # assumes the data were retained
m       = fit$x.mean          # estimate of mean
phi     = fit$ar              # estimate of phi
nboot   = 250                 # number of bootstrap replicates
resids  = fit$resid[-1]       # the 99 innovations
x.star  = dex                 # initialize x*
phi.star.yw = rep(NA, nboot)
#- start it up
for (i in 1:nboot) {
  resid.star = sample(resids, replace=TRUE)
  for (t in 1:99){ x.star[t+1] = m + phi*(x.star[t]-m) + resid.star[t]
  }
  phi.star.yw[i] = ar.yw(x.star, order=1)$ar
}
# Picture
culer = rgb(.5,.7,1,.5)
hist(phi.star.yw, 15, main="", prob=TRUE, xlim=c(.65,1.05), ylim=c(0,14),
     col=culer, xlab=expression(hat(phi)))
lines(density(phi.yw, bw=.02), lwd=2) # from previous simulation
u = seq(.75, 1.1, by=.001) # normal approximation
lines(u, dnorm(u, mean=.96, sd=.03), lty=2, lwd=2)
legend(.65, 14, legend=c('true distribution', 'bootstrap distribution',
                         'normal approximation'), bty='n', lty=c(1,0,2), lwd=c(2,0,2),
       col=1, pch=c(NA,22,NA), pt.bg=c(NA,culer,NA), pt.cex=2.5)
