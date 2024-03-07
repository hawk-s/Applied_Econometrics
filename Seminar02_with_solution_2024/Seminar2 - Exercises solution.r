library('tseries')
library('forecast') 
set.seed = 42
series = rnorm(100)

#plotting the time series
options(repr.plot.width=8, repr.plot.height=3) # set the plot size
ts.plot(series)

options(repr.plot.width=8, repr.plot.height=3) # set the plot size
par(mfrow=c(1,2))
Acf(series)
Pacf(series)

library(tseries) # needed for the adf.test function

# generate gaussian white noise once:
set.seed(50)
series = rnorm(50)

#look at first five observations
series[0:5] 


# check adf test for this time series:
test_result = adf.test(series, k=0) # specify the AR order, zero for the simplest Dickey Fuller test
test_result

# if the p value is small we will get a warning
# to suppress it, we can use:
test_result = suppressWarnings(adf.test(series, k=0))

# check what can we find from this test
attributes(test_result)

# get the p-value from the adf test
test_result$p.value

# compare to 0.05 and check whether the null hypothesis is rejected:
test_result$p.value < 0.05



# add all of this into a for loop and collect true/false for each we perform the test on simulated times series
TT=1000
a=array() # we will add the true or false to this array

for (t in 1:TT) {
    series = rnorm(50)
    test_result = suppressWarnings(adf.test(series, k=0))
    a[t] = test_result$p.value<0.05
}

#the null hypothesis (of unit root) was rejected in 
sum(a)

# out of 
length(a)

# there is quite a difference in results depending on parameter k

library('tseries')   #Time Series Analysis and Computational Finance 
library('forecast') #Forecasting Functions for Time Series and Linear Models 
library('repr') #Serializable Representations 

options(repr.plot.width=8, repr.plot.height=3)
rm(list = ls())
data <- read.table("PX50_2007_2008.txt")
data <- as.matrix(data) 
plot.ts(data)

head(data)

# looking at the time series of prices:

# summary statistics
summary(data)

# there are different functions how to obtain summary statistics
# we would probably like to include variance, skewness and kurtosis 

# library(Hmisc) 
# describe(data)

# ACF and PACF of the time series (price of the index)
par(mfrow = c(1,2))
Acf(data, main = "ACF")
Pacf(data, main = "PACF")

# transformations: log returns
returns <- diff(log(data))

head(returns)

summary(returns)

# plot the returns
ts.plot(returns)

# ACF PACF
par(mfrow = c(1,2))
Acf(returns, main = "ACF")
Pacf(returns, main = "PACF")

# testing stationarity:
adf.test(data)
adf.test(returns)

kpss.test(data)
kpss.test(returns)

# conclusion - prices are not stationary, log returns are stationary




