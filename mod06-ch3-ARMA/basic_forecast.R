sample_size = 100
train_length = 90
wn = rnorm(sample_size)
train = wn[1 : train_length]
arimaFit = arima(wn, order = c(0, 0, 0))
fitted = predict(arimaFit, n.ahead = sample_size - train_length)
# pictures
# AR1, MA1, 