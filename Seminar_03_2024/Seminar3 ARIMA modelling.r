rm(list = ls())

#install.packages(c("tseries", "forecast", "fracdiff"))

suppressPackageStartupMessages({
    library('tseries')    #Time Series Analysis and Computational Finance 
    library('forecast')  #Forecasting Functions for Time Series and Linear Models 
    library('fracdiff')    #Fractionally Differenced ARIMA aka ARFIMA(P,d,q) Models
    library('repr')        #Serializable Representations 
})
options(repr.plot.width=8, repr.plot.height=3) # set the plotting parameters   







rm(list = ls())
set.seed(123)

# simulate time series
AR1   <- arima.sim(model = list(ar = c(0.9)), n = 600)
AR1_ <- arima.sim(model = list(ar = c(-0.9)), n = 600)

AR2 <- arima.sim(model = list(ar = c(0.7, 0.25)), n = 600)
AR2_ <- arima.sim(model = list(ar = c(-0.7, -0.25)), n = 600)


par(mfrow = c(1, 3))
ts.plot( AR1,  main='AR(1) 0.9')
Acf(AR1, lag.max = 12, main = "AR(1) 0.9 |ACF")
Pacf(AR1, lag.max = 12, main = "AR(1) 0.9 |PACF")

ts.plot(AR1_,  main='AR(1) -0.9')
Acf(AR1_, lag.max = 12, main = "AR(1) -0.9 |ACF")
Pacf(AR1_, lag.max = 12, main = "AR(1) -0.9 |PACF")

ts.plot(AR2 ,  main='AR(2) 0.7,0.25')
Acf(AR2, lag.max = 12, main = "AR(2) 0.7,0.25 |ACF")
Pacf(AR2, lag.max = 12, main = "AR(2) 0.7,0.25 |PACF")

ts.plot(AR2_ ,  main='AR(2) -0.7,-0.25')
Acf(AR2_, lag.max = 12, main = "AR(2) -0.7,-0.25 |ACF")
Pacf(AR2_, lag.max = 12, main = "AR(2) -0.7,-0.25 |PACF")


set.seed(123)
#Series
    MA1 <- arima.sim(model = list(ma = c(0.75)), n = 1000)
    MA1_ <-arima.sim(model = list(ma = c(-0.75)), n = 1000)
    MA2 <- arima.sim(model = list(ma = c(0.75, 0.25)), n = 1000)

par(mfrow = c(1, 3))

ts.plot(MA1 , main ='MA(1) 0.75')
    Acf(MA1, lag.max = 12, main = "ACF of MA(1) 0.75")
    Pacf(MA1, lag.max = 12, main = "PACF of MA(1) 0.75")

ts.plot(MA1_ , main ='MA(1) -0.75 ')
    Acf(MA1_, lag.max = 12, main = "ACF of MA(1) -0.75")
    Pacf(MA1_, lag.max = 12, main = "PACF of MA(1) -0.75")

ts.plot(MA2 , main ='MA(2) 0.75, 0.25')
    Acf(MA2, lag.max = 12, main = "ACF of MA(2) 0.75, 0.25")
    Pacf(MA2, lag.max = 12, main = "PACF of MA(2) 0.75, 0.25")

rm(list = ls())
set.seed(123)
#Series
    y3 <- arima.sim(model = list(ar = c(0.7, -0.4),  ma = c(0.6, -0.15)), n = 1000)
#Plot
    ts.plot(y3,ylab='')
#ACF/PACF
    par(mfrow=c(1,2))
    Acf(y3, lag.max = 12, main="ACF|  ARMA(2,2)")
    Pacf(y3, lag.max = 12, main="PACF|  ARMA(2,2)")

rm(list = ls())
options(repr.plot.width=8, repr.plot.height=3)
#Import Data
    data <- read.table("CZK_EUR_2008.txt")$V1
#Plot
    plot.ts(data, main='CZK/EUR exchange rate')
#Dickey-Fuller test for stationarity 
    adf.test(data, k = 1)

#Stationarize Data     
    returns <- diff(log(data))
#Plot
    plot.ts(returns, main='Log returns from CZK/EUR exchange rate')
#Dickey-Fuller test for stationarity 
    adf.test(returns, k = 1)
#ACF/PACF 
par(mfrow=c(1,2))
    Acf(returns, lag.max = 12, main="ACF")
    Pacf(returns, lag.max = 12, main="PACF")

#Model fitting  - automatic selection of lags (here using AICc criterion)
    (fit.auto <- auto.arima(returns, seasonal = FALSE, stationary = TRUE, ic = 'aicc', stepwise = FALSE))



# manual fit (of the same model as selected by auto arima)
fit.manual <- arima(returns, order = c(1, 0, 0))
(1-pnorm(abs(fit.manual$coef)/sqrt(diag(fit.manual$var.coef))))*2 # calculate p-values for estimates

# check acf and pacf of residuals
par(mfrow = c(1,2))
    Acf(fit.manual$residuals, main = "ACF of residuals")
    Pacf(fit.manual$residuals, main = "PACF of residuals")

#The Ljung-Box test
    Box.test(fit.manual$residuals,  lag = 20, type = c("Ljung-Box"))

#store residuals in a vector
    residuals <- fit.manual$residuals
#normalize residuals 
    residuals.norm <- residuals/sqrt(fit.manual$sigma2)
#Plot the histogram
    hist(residuals.norm, prob = TRUE, breaks=40, xlab = "Normalized residuals", main="Histogram")
    curve(dnorm(x, mean = 0, sd = 1), col="blue", lwd=1, add=TRUE)

jarque.bera.test(residuals)

    residuals.sqrd <- residuals^2
    plot.ts(residuals.sqrd, ylab = "Squared residuals")

par(mfrow=c(1,2))
    Acf(residuals.sqrd, lag.max = 12, main="ACF residuals.sqrd")
    Pacf(residuals.sqrd, lag.max = 12, main="PACF residuals.sqrd")

# returns vs fitted values
    plot.ts(returns)
    lines(fitted(fit.manual), col = "red")

forecast(fit.manual, h=5)
# because we have ar(1) model, only the previous observation + estimated coefficients are used to calculate the forecast
# forecast quickly gets to the mean of the time series

fcast <- forecast(fit.manual, h=10)
plot(fcast)

set.seed(111)
r <- arima.sim(list(order = c(1,1,0), ar = 0.7), n = 200) #generate nonstationary process, after differencing it is ar(1)
tseries::adf.test(r)
auto.arima(r, ic='bic')

rm(list = ls())
set.seed(345)
#Series
    y0 <- arima.sim(model = list(ar = c(0.25)), n = 1000)
#Plot
    ts.plot(y0,ylab='')
#ACF/PACF
par(mfrow=c(1,2))
    Acf(y0, lag.max = 12, main="ACF")
    Pacf(y0, lag.max = 12, main="PACF")

# automatic selection of orders based on information criteria using AIC (same model selected for AICc)
# only the ar1 coefficient is statistically significant. Coefficients cancel each other, the ar1 coefficient is substantially higher than the true value.
auto.arima(y0, ic='aic')

# Depending on parameters used in auto.arima function, we can get even more overfitted models:
# here we include stepwise=FALSE, which allows searching through more models + AIC criterion. 
# According to the automatized procedure, the ARMA(5,0) model is the best fit. However, if you look at the estimated coefficients, only the first lag is significant. 
auto.arima(y0, seasonal = FALSE, ic = 'aic', stationary = TRUE, stepwise=FALSE) # you can use stepwise = false -- searching over more possible models, slower

# using BIC instead of AIC: (higher penalty for more parameters)
# AR(1) selected, compare the coefficient from this model to models above.
auto.arima(y0, seasonal = FALSE, ic = 'bic', stationary = TRUE, stepwise=FALSE) # you can use stepwise = false -- searching over more possible models, slower

#Model fitting - manually
arima(y0, order = c(1, 0, 0))

arima(y0, order = c(2, 0, 0))
arima(y0, order = c(2, 0, 1)) # convergence problem - this model should not be used


#(1-pnorm(abs(model$coef)/sqrt(diag(model$var.coef))))*2 # calculate p value for estimates



# if we want to compare two NESTED models, we can also use likelihood ratio test - LR test
# this allows formally testing, while with information criteria we can only compare numbers and select the highest

# using log likelihood to calculate LR:
#h0: restrictions hold - restricted model is better
#h1: unrestricted model better

restricted_model = arima(y0, order = c(1, 0, 0))
unrestricted_model = arima(y0, order = c(2, 0, 1))
df = 2 # number of restrictions in the restricted model compared to unrestricted one

lrstat <- 2*(unrestricted_model$loglik - restricted_model$loglik)
lrstat
# getting p-value 
pchisq(lrstat, df = df, lower.tail = F) 

# alternatively using lrtest function
#library(lmtest)
#lrtest(unrestricted_model, restricted_model)

# conclusion - p-value of 0.42 -> we cannot reject the null that restriction is valid (restricted model is better)

library('tseries')   #Time Series Analysis and Computational Finance 
library('forecast') #Forecasting Functions for Time Series and Linear Models 
library('repr') #Serializable Representations 

md = read.table(
    'ARMA_data.csv', # relative path to csv file.
    header = TRUE, sep = ',', skip=0)
series = md[['x']]
head(series)













rm(list = ls())
options(repr.plot.width=8, repr.plot.height=5)
# ARFIMA(0.5,0.4,0.0)
    psi <- 0.5
    dd <- 0.4
# ARFIMA with coefficient psi and difference dd 
    y1 <- fracdiff.sim(n = 1000, ar = psi, ma = 0.0, d=0.4) 
# ARIMA(1,0,0) with coefficient psi 
    y2 <- arima.sim(model = list(ar = psi), n = 1000) 
# Graphics
par(mfrow = c(2,2))
    plot.ts(y1$series, main='Time series plot of long memory', ylab='')
    Acf(y1$series, lag.max = 50, main='Autocorrelations of long memory')
    plot.ts(y2, main='Time series plot of short memory', ylab='')
    Acf(y2, lag.max = 50, main='Autocorrelations of short memory')

#estimating ARFIMA series simulated above: 
    summary(fracdiff(y1$series, nar = 1))

data <- read.table("CZK_EUR_2008.txt")$V1
length(data)/12 

length(data)


rm(list = ls())
options(repr.plot.width=8, repr.plot.height=3)

data <- read.table("CZK_EUR_2008.txt")$V1
    plot.ts(data)
    adf.test(data, k = 1)
returns <- diff(log(data), lag =25) #monthly
    plot.ts(returns)
    adf.test(returns, k = 1)
par(mfrow=c(1,2))
    Acf(returns, lag.max = 12, main="ACF returns")
    Pacf(returns, lag.max = 12, main="PACF returns")

    (fit.auto <- auto.arima(returns, seasonal = FALSE, stationary = TRUE, ic = 'aicc', stepwise = FALSE))
    (fit.manual <- arima(returns, order = c(2, 0, 0)))

par(mfrow = c(1,2))
    Acf(fit.auto$residuals, main = "ACF residuals")
    Pacf(fit.auto$residuals, main = "PACF residuals")

    Box.test(fit.manual$residuals,  lag = 20, type = c("Ljung-Box"))

    residuals <- fit.manual$residuals
    residuals.norm <- residuals/sqrt(fit.manual$sigma2)
    hist(residuals.norm, prob = TRUE, breaks=40, xlab = "Normalized residuals", main="Histogram")
    curve(dnorm(x, mean = 0, sd = 1), col="blue", lwd=1, add=TRUE)

    jarque.bera.test(residuals)

    residuals.sqrd <- residuals^2
    plot.ts(residuals.sqrd, ylab = "Squared returns")

par(mfrow=c(1,2))
    Acf(residuals.sqrd, lag.max = 12, main="ACF residuals.sqrd")
    Pacf(residuals.sqrd, lag.max = 12, main="PACF residuals.sqrd")

plot.ts(returns)
    lines(fitted(fit.manual), col = "red")

    fcast <- forecast(fit.manual, h=10)
    plot(fcast)

data <- read.table("CZK_EUR_2017-20.txt")$V1
length(data)/4 # we have four years of daily data - it includes non trading days (padded with previous value)

rm(list = ls())
options(repr.plot.width=8, repr.plot.height=3) 
data <- read.table("CZK_EUR_2017-20.txt")$V1

print("Now lets try to model exchange rate for longer period")

plot.ts(data)
adf.test(data, k = 1)

print("According to the ADF test, data are not stationary. We have to stationarize them. As it is common in finance applications, let's compute the log returns")
returns <- diff(log(data), lag = 31)
adf.test(returns, k = 1)
plot.ts(returns)

print("Which model to choose?")
par(mfrow=c(1,2))
Acf(returns, lag.max = 12, main="ACF returns")
Pacf(returns, lag.max = 12, main="PACF returns")

print("autofit")
(fit.auto <- auto.arima(returns, seasonal = FALSE, stationary = TRUE, ic = 'aicc', stepwise = FALSE))
(fit.manual <- arima(returns, order = c(1,0,0)))

par(mfrow = c(1,2))
Acf(fit.auto$residuals, main = "ACF fit.auto")
Pacf(fit.auto$residuals, main = "PACF fit.auto")

print("Ljung-Box test")
Box.test(fit.manual$residuals,  lag = 20, type = c("Ljung-Box"))

print("Let's check the residuals for manual fit. Let's investigate whether there is some dependency left in them")
residuals <- fit.manual$residuals
residuals.norm <- residuals/sqrt(fit.manual$sigma2)
hist(residuals.norm, prob = TRUE, breaks=40, xlab = "Normalized residuals", main="Histogram")
curve(dnorm(x, mean = 0, sd = 1), col="blue", lwd=1, add=TRUE)

jarque.bera.test(residuals)

residuals.sqrd <- residuals^2
plot.ts(residuals.sqrd, ylab = "Squared returns")

par(mfrow=c(1,2))
Acf(residuals.sqrd, lag.max = 12, main="ACF residuals.sqrd")
Pacf(residuals.sqrd, lag.max = 12, main="PACF residuals.sqrd")

par(mfrow=c(1,1))
plot.ts(returns)
lines(fitted(fit.manual), col = "red")

print("Let's try to forecast the future values using our model")
fcast <- forecast(fit.manual, h=5)
plot(fcast)
