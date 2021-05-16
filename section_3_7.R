rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Section 3.7 Building ARIMA Models

# Example 3.39, 3.40 and 3.43
plot(gnp)
acf2(gnp, 50)
gnpgr = diff(log(gnp))      # growth rate
plot(gnpgr)
acf2(gnpgr, 24)
sarima(gnpgr, 1, 0, 0)      # AR(1)
sarima(gnpgr, 0, 0, 2)      # MA(2)
ARMAtoMA(ar=.35, ma=0, 10)  # prints psi-weights

# Example 3.41
sarima(log(varve), 0, 1, 1, no.constant=TRUE)   # ARIMA(0,1,1)
sarima(log(varve), 1, 1, 1, no.constant=TRUE)   # ARIMA(1,1,1)
