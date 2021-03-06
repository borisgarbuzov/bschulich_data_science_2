# Simulate the process with given parametsrs
# Predictcs the given number of units forward
# Plots both history, real furture and predicted future.
basic_forecast <- function(p = 0, # AR order
                           d = 0, # Integration order
                           q = 0, # MA order
                           theta = NULL, # scalar or vector
                           phi = NULL, # scalar or vector
                           n = 100, # total size for simulation
                           train_lenght = 80) # length of training part
{
  # remove all the previous plots
  graphics.off()

  # Simulate the whole vector of train and test together
  sim <- arima.sim(model = list(order = c(p, d, q), ar = phi, ma = theta),
                   n = n)
  # Split the whole array into train and test parts
  train_sim <- ts(sim[1:train_lenght])
  test_sim <- ts(sim[train_lenght:n], start = train_lenght)

  # Estimate the ARIMA coefficients and pack them into a model.
  arima_fit <- arima(train_sim, order = c(p, d, q))

  # By the estimated model, predict the future of size n - train_lenght
  arima_predict <- predict(arima_fit, n.ahead = n - train_lenght)
  # Extract the predicted values
  predicted <- arima_predict$pred
  # Extract the standard errors
  standard_err <- arima_predict$se

  # plot forecast with past and confidence interval
  upper <- predicted + standard_err
  lower <- predicted - standard_err
  arima_title_prefix = 'Test values with past vs forecast for'
  arima_title_suffix <- paste0('ARIMA(', p, ', ', d, ', ', q, ')')

  plot(test_sim, type = 'l', ylab = 'values', col = "purple",
       main = paste(arima_title_prefix, arima_title_suffix),
       ylim = c(min(test_sim, predicted, lower), max(test_sim, predicted, upper)))
  abline(h = 0)
  lines(predicted, col = 'red')
  lines(upper, lty = 'dashed', lwd = 2, col = 'springgreen4')
  lines(lower, lty = 'dashed', lwd = 2, col = 'springgreen4')
  abline(v = start(predicted)[1], col = 'blue')
  legend("topright",
         legend = c('test', 'forecast', 'where forecast starts',
                    'confidence interval'),
         col = c('purple', 'red', 'blue', 'springgreen4'), lty = c(rep(1, 3), 2),
         y.intersp = .7, cex = .7)

  # plot residuals for forecast
  resid_forecast <- test_sim[2:length(test_sim)] - predicted
  plot(resid_forecast, type = 'p', ylab = 'residuals',
       main = paste('Residuals plot for forecast', arima_title_suffix))
  lines(lowess(resid_forecast), col = 'red')
  abline(h = 0, lty = 3)
}

# White noise
basic_forecast (p = 0,
               d = 0,
               q = 0,
               theta = NULL,
               phi = NULL,
               n = 1000,
               train_lenght = 990)

basic_forecast (p = 1,
                d = 0,
                q = 0,
                theta = NULL,
                phi = 1/2,
                n = 1000,
                train_lenght = 990)

basic_forecast (p = 0,
                d = 0,
                q = 1,
                theta = 1,
                phi = NULL,
                n = 1000,
                train_lenght = 990)

# ARMA(1, 1)
basic_forecast (p = 1,
                d = 0,
                q = 1,
                theta = 1,
                phi = 1/2,
                n = 1000,
                train_lenght = 990)


# ARIMA(1, 0, 1),
basic_forecast (p = 1,
                d = 0,
                q = 1,
                theta = 1,
                phi = -1/2,
                n = 1000,
                train_lenght = 800)


# ARIMA(5, 0, 5),
basic_forecast (p = 5,
                d = 0,
                q = 5,
                theta = c(1, 1, 1, 1, 1),
                phi = c(-1/2, -1/2, -1/2, -1/2, -1/2),
                n = 1000,
                train_lenght = 800)

# ARIMA(0, 1, 0), random walk
basic_forecast (p = 0,
                d = 1,
                q = 0,
                theta = NULL,
                phi = NULL,
                n = 1000,
                train_lenght = 800)

# ARIMA(1, 1, 1),
basic_forecast (p = 1,
                d = 1,
                q = 1,
                theta = 1,
                phi = 1/2,
                n = 1000,
                train_lenght = 800)
