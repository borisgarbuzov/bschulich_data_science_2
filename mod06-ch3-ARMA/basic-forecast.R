basic_forecast <- function(p = 0,
                           d = 0,
                           q = 0,
                           theta = NULL,
                           phi = NULL,
                           n = 100,
                           train_lenght = 80)
{
  # remove all the previous plots
  graphics.off()
  
  sim <- arima.sim(model = list(order = c(p, d, q), ar = phi, ma = theta),
                   n = n)
  train_sim <- ts(sim[1:train_lenght])
  test_sim <- ts(sim[train_lenght:n], start = train_lenght)
  
  arima_fit <- arima(train_sim, order = c(p, d, q))
  
  arima_predict <- predict(arima_fit, n.ahead = n - train_lenght)
  predicted <- arima_predict$pred
  standard_err <- arima_predict$se
  
  # plot forecast with past and confidence interval
  upper <- predicted + standard_err
  lower <- predicted - standard_err
  arima_title <- paste0('ARIMA(', p, ', ', d, ', ', q, ')')
  
  plot(test_sim, type = 'l', ylab = 'values',
       main = paste('Test values with past vs forecast for', arima_title),
       ylim = c(min(test_sim, predicted, lower), max(test_sim, predicted, upper)))
  lines(predicted, col = 'red')
  lines(upper, lty = 'dashed', lwd = 2, col = 'springgreen4')
  lines(lower, lty = 'dashed', lwd = 2, col = 'springgreen4')
  abline(v = start(predicted)[1], col = 'blue')
  legend("topright",
         legend = c('test', 'forecast', 'where forecast starts',
                    'confidence interval'),
         col = c('black', 'red', 'blue', 'springgreen4'), lty = c(rep(1, 3), 2),
         y.intersp = .7, cex = .7)
  
  # plot residuals for forecast
  resid_forecast <- test_sim[2:length(test_sim)] - predicted
  plot(resid_forecast, type = 'p', ylab = 'residuals',
       main = paste('Residuals plot for forecast', arima_title))
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
               train_lenght = 80)

basic_forecast (p = 1,
                d = 0,
                q = 0,
                theta = NULL,
                phi = 1/2,
                n = 1000,
                train_lenght = 800)

basic_forecast (p = 0,
                d = 0,
                q = 1,
                theta = 1,
                phi = NULL,
                n = 1000,
                train_lenght = 800)

# ARMA(1, 1)
basic_forecast (p = 1,
                d = 0,
                q = 1,
                theta = 1,
                phi = 1/2,
                n = 1000,
                train_lenght = 800)

# ARIMA(0, 1, 0), random walk
basic_forecast (p = 1,
                d = 1,
                q = 1,
                theta = 0,
                phi = 0,
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


