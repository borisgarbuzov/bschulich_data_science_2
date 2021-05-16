library(forecast)

computeInnovations <- function(x)
{
  fit <- arima(x, order = c(1, 0, 0))
  x_hats <- 0
  e <- x[1]

  for (i in 1:(length(x) - 1)) {
    forecast_value_new = forecast(x[1:i], h = 1, model = fit)$mean
    x_hats <- c(x_hats, forecast_value_new) # append the new value
    e_new = x[i+1] - x_hats[i+1]
    e <- c(e, e_new) # append to innovations array
  }

  return(e)
}


simulated = arima.sim(n = 1000, list(ar = c(1/2)))
myInnovations = computeInnovations(simulated)
plot(myInnovations)
acf(myInnovations, type="covariance")
acf(myInnovations, type="correlation")
