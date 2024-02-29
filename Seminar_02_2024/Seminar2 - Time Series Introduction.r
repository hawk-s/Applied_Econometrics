rm(list = ls())

#install.packages(c("tseries", "forecast"))

suppressPackageStartupMessages({
    library('tseries')   #Time Series Analysis and Computational Finance 
    library('forecast') #Forecasting Functions for Time Series and Linear Models 
    library('repr')     #Serializable Representations 
})


options(repr.plot.width=8, repr.plot.height=3) # set the plotting parameters   

set.seed(123)
#The set.seed() function sets the starting number used to generate a sequence of random numbers – 
#it ensures that you get the same result if you start with that same seed each time you run the same process. 

#generate Random Walk: 
x.1 <- cumsum(rnorm(1000))
x.2 <- cumsum(rnorm(1000))
#for why we can use cumsum see appendix below. 
par(mfrow=c(1,2))
ts.plot(x.1)
ts.plot(x.2)

cat('Correlation between two random walks: ',cor(x.1, x.2))

#spurious regression
summary(lm(x.1~x.2))

# histogram of correlations of two random walks 
set.seed(345)

n <- 10000 # number of simulations
sim.cor <- rep(NA, n)

for (i in 1:n) {
    x.1 <- cumsum(rnorm(1000))
    x.2 <- cumsum(rnorm(1000))
    sim.cor[i] <- cor(x.1, x.2)
}

hist(sim.cor)

set.seed(123)
    mu <- 1
    time <- rep(mu, 1000)
#generate Random Walk with trend: 
    x.1 <- cumsum(rnorm(1000, 0, 10)) + cumsum(time) 
    x.2 <- cumsum(rnorm(1000, 0, 10)) + cumsum(time)

par(mfrow=c(1,2))
    ts.plot(x.1)
    ts.plot(x.2)

cat('Correlation between two random walks with common trend: ',cor(x.1, x.2))

#spurious regression
summary(lm(x.1~x.2))

set.seed(345)

n <- 10000 # number of simulations
sim.cor <- rep(NA, n)

for (i in 1:n) {
    x.1 <- cumsum(rnorm(1000, 0, 10)) + cumsum(time) 
    x.2 <- cumsum(rnorm(1000, 0, 10)) + cumsum(time)
    sim.cor[i] <- cor(x.1, x.2)
}

hist(sim.cor)

set.seed(123)
    x.1 <- arima.sim(model = list(ar = c(0.75)), n = 2000)
    x.2 <- arima.sim(model = list(ar = c(0.75)), n = 2000)
 par(mfrow=c(1,2))
    ts.plot(x.1)
    ts.plot(x.2)


cat('Correlation between two AR(1) processes: ',cor(x.1, x.2))

#not-spurious regression
summary(lm(x.1~x.2))

n <- 10000 # number of simulations
sim.cor <- rep(NA, n)

for (i in 1:n) {
    x.1 <- arima.sim(model = list(ar = c(0.75)), n = 2000)
    x.2 <- arima.sim(model = list(ar = c(0.75)), n = 2000)
    sim.cor[i] <- cor(x.1, x.2)
}

hist(sim.cor)



options(repr.plot.width=10, repr.plot.height=3) # set the plotting parameter
set.seed(1234)

x <- arima.sim(model = list(ar = c(0.75)), n = 1000)  # AR (1): y = 0.75 * y(t-1) + e(t)
y <- arima.sim(model = list(ma = c(0.75)), n = 1000)  # MA (1): y = 0.75 * e(t-1) + e(t)
z <- cumsum(rnorm(1000))  # x = x(t-1) + w(t)

par(mfrow=c(1,3))
    ts.plot(x, ylab='',main = 'Stationary, AR(1) process')
    ts.plot(y, ylab='',main = 'Stationary, MA(1) process')
    ts.plot(z, ylab='', main = 'Non-Stationary, random walk')

options(repr.plot.width=8, repr.plot.height=4) # set the plotting parameters
par(mfrow=c(2,3))
    Acf(x, lag.max = 12, main ='ACF: Stationary, AR(1) process')
    Acf(y, lag.max = 12, main="ACF: Stationary, MA(1) process")
    Acf(z, lag.max = 12, main="ACF: Non-Stationary, random walk")

    Pacf(x, lag.max = 12, main="PACF: Stationary, AR(1) process")
    Pacf(y, lag.max = 12, main="PACF: Stationary, MA(1) process")
    Pacf(z, lag.max = 12, main="PACF: Non-Stationary, random walk")

# ACF tells us the correlation, we can also see/plot the relationship of each observation and corresponding lagged observation

# plot the AR(1) process against its lags:
options(repr.plot.width=8, repr.plot.height=3) # set the plotting parameters
lag.plot(x, lags = 4, pch = ".")

# plot the MA(1) process against its lags:
options(repr.plot.width=8, repr.plot.height=3) # set the plotting parameters
lag.plot(y, lags = 4, pch = ".")

# plot the random walk against its lags:
lag.plot(z, lags = 4, pch = ".")











options(repr.plot.width=8, repr.plot.height=4)

# load the data
data <- read.table("CZK_EUR_2008.txt")
series <- as.matrix(data)
dif.series <- diff(series)
returns <- diff(log(series))

#lets compare with randomly generated AR(1) series. 
set.seed(123)
    TT <- 800
    xx <- ww <- rnorm(n = TT, mean = 0, sd = 10)
    for (t in 2:TT) {
        xx[t] <- 0.75*xx[t - 1] + ww[t]
    }

par(mfrow = c(1,2))
plot.ts(xx, ylab = "AR(1) time series")
plot.ts(series, ylab = "series")
par(mfrow = c(1,2))

plot.ts(dif.series, ylab = "differences")
plot.ts(returns, ylab = "log returns")



options(repr.plot.width=8, repr.plot.height=3)
    log.ret <- diff(log(series))
    ret <- diff(series)/series[-length(series)] #series[-length(series)] drops the last element of 'series'

    plot.ts(log.ret, ylab = "Net and Log returns")
    lines(ret, col = "red", lty = 3)

plot.ts(ret - log.ret, main = "Difference between net and log returns", ylab = expression(R[t] - r[t]))

options(repr.plot.width=8, repr.plot.height=12)
par(mfrow=c(4,2))
    Acf(xx, lag.max = 12, main = "ACF:AR(1)")
    Pacf(xx, lag.max = 12, main = "PACF: AR(1)")

    Acf(series, lag.max = 12, main = "ACF: Raw series")
    Pacf(series, lag.max = 12, main = "PACF: Raw series")

    Acf(dif.series, lag.max = 12, main = "ACF: Difference")
    Pacf(dif.series, lag.max = 12, main = "PACF: Difference")

    Acf(returns, lag.max = 12, main = "ACF: returns")
    Pacf(returns, lag.max = 12, main = "PACF: returns")

adf.test(series)
adf.test(series, k = 1)



adf.test(dif.series, k = 1)

kpss.test(series)

kpss.test(dif.series)

kpss.test(returns)



# to suppress warnings you can use: suppressWarnings(function_with_unnecessary_warnings(params))







library('tseries')   #Time Series Analysis and Computational Finance 
library('forecast') #Forecasting Functions for Time Series and Linear Models 
library('repr') #Serializable Representations 

options(repr.plot.width=8, repr.plot.height=3)
data <- read.table("PX50_2007_2008.txt")
data <- as.matrix(data) 
plot.ts(data)







    ## set random number seed
        set.seed(123)
    ## length of time series
        TT <- 1000
    ## initialize {x_t} and {w_t}
        xx <- ww <- rnorm(n = TT, mean = 0, sd = 1)

#1=============================================
    ## compute values 2 thru TT
            a=1
        # x(t) = a*x(t-1) + w(t)
        for (t in 2:TT) {
            xx[t] <- a*xx[t - 1] + ww[t]
        }

#2=============================================
    ## however if we iterate RW regression backwards: 
        # x(t) = x(t-1) + w(t)
        # x(t-1) = x(t-2) + w(t-1)
        # x(t-2) = x(t-3) + w(t-2) .... 
    ## Plug in: 
        # x(t) = x(t-2) + w(t-1) + w(t)
        # x(t) = w(t) + w(t-1) + w(t-2) + w(t-3) + ...+ w(t-(t-2)) + w(t-(t-1)) + x(0)
        # x(t) = w(1) + w(2) + w(3) + ... +w(t)
    ##Thus, we can write is as the cummulative sum of w(t): 
        x2 <- cumsum(ww)
    ## setup plot area
        par(mfrow = c(1, 2))
    ## plot 1st RW
        plot.ts(xx, ylab = expression(italic(x[t])))
    ## plot 2nd RW
        plot.ts(x2, ylab = expression(italic(x[t])))




