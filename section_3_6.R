rm(list = ls())

if (suppressWarnings(!require("astsa"))) {
  install.packages("astsa")
  library(astsa)
}

# Section 3.6 Integrated Models for Nonstationary Data

# Example 3.38
set.seed(666)
x = arima.sim(list(order = c(0,1,1), ma = -0.8), n = 100)
(x.ima = HoltWinters(x, beta=FALSE, gamma=FALSE))  # Î± is 1-Î» here
plot(x.ima)
