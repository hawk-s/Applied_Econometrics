rm(list = ls())

dataset <- read.csv('Seminar_01_simulated_data.csv', header = TRUE)
str(dataset)
sum(is.na(dataset))
sapply(dataset, function(x1) sum(is.na(x1)))
       
head(dataset,5)
tail(dataset,5)

summary(dataset)

options(repr.plot.width = 8, repr.plot.height = 4)
par(mfrow = c(1,2))
hist(dataset$x1, n = 10, probability = FALSE)
hist(dataset$y, n = 10, probability = FALSE)

cor(dataset[,2:6])

options(repr.plot.width = 8, repr.plot.height = 4)
par(mfrow = c(1,2))
plot(dataset$x1, dataset$y, type = 'p')
plot(dataset$z1, dataset$x1, type = 'p')

library('car')
library('lmtest')
library('AER')
library('systemfit')

model.OLS <- lm(y ~ x1, data = dataset)
summary(model.OLS)

set.seed(1234)
errorA <- runif(length(dataset$x1), min = 0, max = 0.5)
errorB <- runif(length(dataset$x1), min = 0, max = 0.5)

options(repr.plot.width = 8, repr.plot.height = 4)
par(mfrow = c(1,2))
hist(errorA, n = 10, probability = FALSE)
hist(errorB, n = 10, probability = FALSE)

x1A <- dataset$x1 + errorA
x1B <- dataset$x1 - errorB

options(repr.plot.width = 8, repr.plot.height = 4)
par(mfrow = c(1,2))
plot(dataset$x1, x1B, type = 'p')
plot(x1A, x1B, type = 'p')

model.OLS.multicoll <- lm(dataset$y ~ x1A + x1B)
summary(model.OLS.multicoll)

library(car)

print(cor(x1A, x1B))

model.OLS.VIF <- lm(x1A ~ x1B)
summary(model.OLS.VIF)
VIF <- 1/(1-summary(model.OLS.VIF)$r.squared) # Variance Inflation Factor manual
print(VIF)

vif(model.OLS.multicoll) # Variance Inflation Factor automated

library(lmtest)

resettest(model.OLS, power = 2:3)

options(repr.plot.width = 4, repr.plot.height = 4)
plot(dataset$x1, resid(model.OLS), type = 'p')

library(AER)

bptest(model.OLS) #Breusch-Pagan/Koenker test
bptest(model.OLS, ~ x1 + I(x1^2), data = dataset) #White test

het.robust.OLS <- coeftest(model.OLS, vcov = vcovHC(model.OLS)) #gives White heteroscedasticity robust standard errors
print(het.robust.OLS)

dwtest(model.OLS)
bgtest(model.OLS, order = 2)

hac.robust.OLS <- coeftest(model.OLS, vcovHAC) #gives Newey-West heteroskedasticity and autocorrelation robust standard errors
print(hac.robust.OLS)

options(repr.plot.width = 4, repr.plot.height = 4)
hist(resid(model.OLS), n = 10, probability = FALSE)

shapiro.test(resid(model.OLS))

model.1.stage <- lm(x1 ~ z1, data = dataset)
summary(model.1.stage)

# using OLS in 2nd stage to get the coefficients. 
# WARNING: The standard errors you would get from this regression are incorrect, 
# as we have to adjust because we are using fitted values in the second stage. 
# We can get the errors either manually (we know the variance of the 2SLS estimator) or using ivreg function from AER package.
# see Wooldridge textbook for further explanation.
model.2.stage <- lm(y ~ fitted(model.1.stage), data = dataset)
model.2.stage$coefficients

# using function ivreg from AER package, performs
model.2SLS.1 <- ivreg(y ~ x1 | z1 , data = dataset)
summary(model.2SLS.1, diagnostics = TRUE)

library(systemfit)

ols <- systemfit(y ~ x1, data = dataset, method = 'OLS')
iv <- systemfit(y ~ x1, data = dataset, method = '2SLS', inst = ~ z1)

hausman.systemfit(iv, ols)

### available in the summary diagnostics:
model.2SLS.2 <- ivreg(y ~ x1 | z1 + z3, data = dataset)
summary(model.2SLS.2, diagnostics = TRUE)

# Sargan test calculated manually:
model.2SLS.2 <- ivreg(y ~ x1 | z1 + z3, data = dataset)

regression <- lm(resid(model.2SLS.2) ~ z1 + z3, data = dataset)
summary(regression)
LM <- summary(regression)$r.squared * length(dataset$y)
print(LM)
print(qchisq(0.95, df = 1))
print(1 - pchisq(LM, df = 1))



rm(list = ls())

datasetmroz <- read.csv('mroz.csv', header = TRUE)
str(datasetmroz)
sapply(datasetmroz, function(x) sum(is.na(x)))
       
head(datasetmroz,5)
tail(datasetmroz,5)

summary(datasetmroz)

sum(datasetmroz$wage>0)
sum(datasetmroz$wage==0)

datasetmroz2 <- datasetmroz[datasetmroz$wage > 0,]
str(datasetmroz2)

cor(datasetmroz2[,c(6,15,16)])

model.OLS.wage <- lm(log(wage) ~ educ + exper + I(exper^2), data = datasetmroz2)
summary(model.OLS.wage)

model.2SLS.M <- ivreg(log(wage) ~ educ + exper + I(exper^2) | exper + I(exper^2) + motheduc, data = datasetmroz2)
summary(model.2SLS.M, diagnostics = TRUE)

model.2SLS.MF <- ivreg(log(wage) ~ educ + exper + I(exper^2) | exper + I(exper^2) + motheduc + fatheduc, data = datasetmroz2)
summary(model.2SLS.MF, diagnostics = TRUE)






