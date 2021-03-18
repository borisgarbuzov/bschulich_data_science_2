rm(list = ls())

# set.seed(1) # set seed to reproduce results below
x <- rnorm(50)
y <- 1*x + rnorm(50)
fit <- lm(y ~ x) # fit linear model

# we can get summary for our model (residuals, coefficients, R-squared, etc.)
summary(fit)

# get all properties that we can extract from the fit object
names(fit)

# also we can extract necessary information from lm object using one of two forms below
fit$residuals # get residuals from fitted model
resid(fit)

fit$coefficients # get model coefficients
coef(fit)

fit$fitted.values # get fitted values from the model
fitted(fit)

# plot our points and then with abline function add fitted line
plot(x, y)
abline(fit)

# get all 4 diagnostic plots for fitted model
par(mfrow = c(2, 2)) # make 2x2 grid for our plots
plot(fit) # extract all 4 plots

# exclude intercept from our model by adding 0
fitWithoutIntercept <- lm(y ~ 0 + x)

fitData <- lm(mpg ~ cyl, data = mtcars)
summary(fitData)
