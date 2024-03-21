# load the data
rm(list = ls())
library(tseries)
library(forecast)
library(repr)
library(rugarch) # package for GARCH modelling
library(moments) # package for kurtosis estimation
library(aTSA) # package for ARCH-LM test
library(rmgarch) # multivariate garch modelling
library(FinTS)
library(quantmod) # download data
options(repr.plot.width = 8, repr.plot.height = 4) # set the plotting parameters
load('AAL_dataset.R')
price = data[,'AAL.Adjusted']
log_ret <- diff(log(price)) 
length(log_ret)
log_ret = na.omit(log_ret)
length(log_ret)

#returns
plot.ts(log_ret)

# absolute returns - proxy for volatility
# returns^2 - proxy for variance
options(repr.plot.width=10, repr.plot.height=4)
par(mfrow = c(1,2))
plot.ts(abs(log_ret), main='absolute returns')
plot.ts(log_ret^2, main='squared returns')

# when we estimate ARMA model (without GARCH), we assume fixed variance
fit = auto.arima(log_ret)
fit
# in case of ARMA(1,0) sigma^2 estimated as 0.001092

# the variance estimated:
fit$sigma2

# if we use fixed variance in our model (ARMA model): (first absolute returns vs fixed volatility, second: squared returns vs estimated variance)
options(repr.plot.width=10, repr.plot.height=4)
par(mfrow = c(1,2))
plot.ts(abs(log_ret), main='absolute returns, estimated volatility')
abline(h=sqrt(fit$sigma2), col = "red")
plot.ts(abs(log_ret)^2, main='squared returns, estimated variance')
abline(h=(fit$sigma2), col = "red")

# checking arch effects: ( + we have formal tests)
# squared returns - clustering of volatility
par(mfrow = c(1,2))
Acf((fit$residuals)^2)
Pacf((fit$residuals)^2)

# fit ARMA-GARCH to get time varying estimate of volatility:
# specify the model that we want to estimate
model_specification <- ugarchspec(mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), 
                      variance.model = list(garchOrder = c(1, 1)))

# estimate the model
fit_garch <- ugarchfit(spec = model_specification, data = log_ret)
#fit_garch

# get estimated time-varying volatility:
volatility_est = sigma(fit_garch)
head(volatility_est)

# estimated volatality ARMA vs ARMA-GARCH model"
plot.ts(abs(log_ret)) 
abline(h=sqrt(fit$sigma2), col = "green") # ARMA only
lines(array(volatility_est),col="red") # ARMA-GARCH estimated volatility, estimated sigma

# with our models, we assume that standardized residuals follow standard normal distribution
# comparison of ARMA residuals, ARMA-GARCH residuals and gaussian white noise

# standardized residuals = residuals from the model / square root of the estimated (conditional) variance

#ARMA: 
std_res_arma = fit$residuals/sqrt(fit$sigma2)

# GARCH get standardized residuals:
volatility_est = sigma(fit_garch) #time varying volatility estimated by garch
res = residuals(fit_garch, standardize = FALSE)
std_res = res/(volatility_est)
# GARCHalternatively (same result)
std_res2 = residuals(fit_garch, standardize = TRUE)

# plot standardized residuals from arma-garch model:
options(repr.plot.width=10, repr.plot.height=4)
par(mfrow = c(1,3))
plot.ts(std_res_arma, main='standardized residuals from ARMA')
plot.ts(std_res, main='standardized residuals from ARMA-GARCH')
plot.ts(rnorm(length(std_res)), main='gaussian white noise') # as a comparison, 

# 95% confidence intervals for fitted values
upper = fitted(fit_garch) + 1.96*volatility_est
lower = fitted(fit_garch) - 1.96*volatility_est


options(repr.plot.width=10, repr.plot.height=4)
par(mfrow = c(1,2))
#GARCH
plot.ts(log_ret, main='ARMA-GARCH model')
lines(array(fitted(fit_garch)), col='green')
lines(array(upper),col="red") 
lines(array(lower),col="red") 

#ARMA:
plot.ts(log_ret, main='ARMA model')
lines(array(fitted(fit)), col='green')
lines(array(fit$fitted + 1.96*sqrt(fit$sigma2)), col = "blue") # ARMA only
lines(array(fit$fitted - 1.96*sqrt(fit$sigma2)), col = "blue") # ARMA only

# comparison of ARMA coefficients and ARMA-GARCH coefficients:
# ar1 lower after we include GARCH (this change before and after volatility modelling is kinda similar concept as omited variable bias)
fit

fit_garch


