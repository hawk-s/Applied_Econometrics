rm(list = ls())

#install.packages(c("rugarch", "moments", "aTSA", "FinTS", "rmgarch"), repos = "http://cran.us.r-project.org")

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

# download data for American Airlines Group Inc
#data = getSymbols(Symbols = 'AAL',  from = "2010-01-01", to= '2020-12-30', srs = "yahoo", return.class='xts', auto.assign=FALSE)

#alternatively use dataset provided and load it:
load(file= 'AAL_dataset.R')

price = data[,'AAL.Adjusted']
log_ret <- diff(log(price)) 
length(log_ret)
# omit NAs
log_ret = na.omit(log_ret)
length(log_ret)

head(data)

head(log_ret)

options(repr.plot.width=10, repr.plot.height=4)
par(mfrow = c(1,2))
ts.plot(price)
ts.plot(log_ret)

par(mfrow = c(1,2))
Acf(price, lag.max = 50, main = "ACF")
Pacf(price, lag.max = 50, main = "PACF")

tseries::adf.test(price)

# ACF PACF plots
par(mfrow = c(1,2))
Acf(log_ret, lag.max = 50, main = "ACF")
Pacf(log_ret, lag.max = 50, main = "PACF")

#ADF test
tseries::adf.test(log_ret)

#KPSS test
kpss.test(log_ret)

#?adf.test
#?kpss.test

# choose the best model based on the IC
(fit_auto <- auto.arima(log_ret, ic = c("bic"), stationary = TRUE))
checkresiduals(fit_auto)
# AIC and BIC both select ar(1)

Box.test(fit_auto$residuals, lag = 20, type = c("Ljung-Box"))
# null is rejected for now - significant autocorrelation present

#?Box.test

par(mfrow = c(1,2))
Acf(fit_auto$residuals, lag.max = 50, main = "ACF")
Pacf(fit_auto$residuals, lag.max = 50, main = "PACF")

#str(fit_auto)

resid <- fit_auto$residuals
resid_sqrd <- resid^2 # create squared residuals

par(mfrow = c(1,2))
Acf(resid_sqrd, lag.max = 50, main = "ACF")
Pacf(resid_sqrd, lag.max = 50, main = "PACF")

arch.test(arima(log_ret,order = c(1,0,0))) # for this function, we need to provide fitted arima model (auto.arima not accepted)

#?arch.test

ArchTest(resid, lags = 12, demean = FALSE) #this function allows us to provide time series instead of fitted arima model

#?ArchTest

# specify the model that we want to estimate
model_specification <- ugarchspec(mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), 
                      variance.model = list(garchOrder = c(1, 1)))

# estimate the model
fit <- ugarchfit(spec = model_specification, data = log_ret)
#infocriteria(fit)

# explore the results
#coef(fit) # estimated parameters
fit


#plot.ts(abs(log_ret))
plot.ts(abs(residuals(fit))) # residuals (after fitting arma model) or log_ret in absolute values
lines(as.numeric(sigma(fit)),col = "red", lwd = 3) 
# plot of the residuals/log returns in absolute value and of estimated conditional standard deviation (volatility)

resid_norm <- residuals(fit, standardize = TRUE)
resid_sqrd_norm <- resid_norm^2
par(mfrow = c(1,2))
Acf(resid_norm, lag.max = 20, main="ACF standardized residuals")
Pacf(resid_norm, lag.max = 20, main="PACF standardized residuals")

par(mfrow = c(1,2))
Acf(resid_sqrd_norm, lag.max = 20, main="ACF squared standardized residuals")
Pacf(resid_sqrd_norm, lag.max = 20, main="PACF squared standardized residuals")



# find the best GARCH model based on the AIC/BIC

p_max <- 5
q_max <- 5
aic_min <- Inf
best_p <- 0
best_q <- 0

for (i1 in 1:p_max) {
    for (i2 in 1:q_max) {
        model_specification <- ugarchspec(mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), 
                     variance.model = list(garchOrder = c(i1, i2)))
        fit <- ugarchfit(spec = model_specification, data = log_ret)
        inf_crit <- infocriteria(fit)[2] #1 aic, 2 bic
        aic_min <- ifelse(inf_crit < aic_min, inf_crit, aic_min)
        
        best_p <- ifelse(inf_crit == aic_min, i1, best_p)
        best_q <- ifelse(inf_crit == aic_min, i2, best_q)
    }
}

c(best_p, best_q)

# specify the model that we want to estimate
model_specification <- ugarchspec(mean.model = list(armaOrder = c(1, 0), include.mean = FALSE), 
                     variance.model = list(garchOrder = c(best_p, best_q)))
# estimate the model
fit <- ugarchfit(spec = model_specification, data = resid)

# Explore the results

#show(fit)

# aic suggests arma(1,0)-garch(2,3)
# bic suggests arma(1,0)-garch(1,1)

#?ugarchspec

# specify the model that we want to estimate
model_specification <- ugarchspec(mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), 
                      variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                      distribution.model = "norm")
# estimate the model
fit <- ugarchfit(spec = model_specification, data = log_ret)

# explore the results
fit

# based on BIC, GJC-GARCH is better than GARCH

# specify the model that we want to estimate
model_specification <- ugarchspec(mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), 
                      variance.model = list(model = "eGARCH", garchOrder = c(1, 1)))
# estimate the model
fit <- ugarchfit(spec = model_specification, data = log_ret)

# explore the results
fit

# BIC lower than for simple garch model

# specify the model that we want to estimate
model_specification <- ugarchspec(mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), 
                      variance.model = list(model = "iGARCH", garchOrder = c(1, 1)))
# estimate the model
fit <- ugarchfit(spec = model_specification, data = log_ret)
fit
# Beta is in this case not estimated but calculated - we do not have errors for beta

data_DCC <- read.table("PX_WIG_2005_2009.txt") # load data Prague stock exchange index and Warsaw index

str(data_DCC)
#ret_DCC <- cbind(diff(log(data_DCC[,1])),diff(log(data_DCC[,2])))
ret_DCC <- apply(data_DCC,2,function(x) diff(log(x), lag = 1))
#ret_DCC==ret_DCC_1
str(ret_DCC)

#?dccspec

spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                   variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))
mspec <- multispec(replicate(2, spec)) # using the same specification for both assets

spec_dcc <- dccspec(mspec, dccOrder = c(1,1), model = c("DCC"))
fit_dcc <- dccfit(spec_dcc, data = ret_DCC)

fit_dcc

cor_dcc <- rcor(fit_dcc)
str(cor_dcc)
plot.ts(cor_dcc[1,2,], ylab = "Correlation PX - WIG")

plot.ts(ret_DCC)

# analysis of residuals: 
resid_dcc <- fit_dcc@mfit$stdresid  # accessing the standardized residuals from the model
resid_dcc = as.data.frame(resid_dcc)
str(resid_dcc)
par(mfrow = c(2,2))
Acf(resid_dcc$V1, lag.max = 50, main = "PX")
Pacf(resid_dcc$V1, lag.max = 50, main = "PX")
Acf(resid_dcc$V2, lag.max = 50, main = "WIG")
Pacf(resid_dcc$V2, lag.max = 50, main = "WIG")

resid_dcc_sqrd<-resid_dcc^2

par(mfrow = c(2,2))
Acf(resid_dcc_sqrd$V1, lag.max = 50, main = "PX")
Pacf(resid_dcc_sqrd$V1, lag.max = 50, main = "PX")
Acf(resid_dcc_sqrd$V2, lag.max = 50, main = "WIG")
Pacf(resid_dcc_sqrd$V2, lag.max = 50, main = "WIG")

hist(resid_dcc$V1, breaks=50,freq=FALSE)
curve(dnorm(x, mean=0, sd=1), 
      col="darkgreen", lwd=2, add=TRUE, yaxt="n")

hist(resid_dcc$V2, breaks=50,freq=FALSE)
curve(dnorm(x, mean=0, sd=1), 
      col="darkgreen", lwd=2, add=TRUE, yaxt="n")



# EUR-USD exchange rate data
data <- read.csv("EUR=X2.csv", na.strings = c("", "null"))
data$Adj.Close <- as.numeric(as.character(data$Adj.Close))
data$log_returns <- c(NA, diff(log(data$Adj.Close))) 
data <- data[!is.na(data$log_returns), ]
returns = data['log_returns']


