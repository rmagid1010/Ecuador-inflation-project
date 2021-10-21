#### Loading Packages ####

library(readxl)
library(vars)
library(ggplot2)
library(urca)

#### Loading Data ####

setwd("~/R/ecuador inflation")
data <- read.csv('CPI_and_PPI.csv')
data <- data[-1]
colnames(data) <- c('date','cpi', 'ppi')
data$cpi <- data$cpi / 100
data$ppi <- data$ppi / 100

##### Data Visualisation ####

matplot(cbind(data$cpi, data$ppi), type="l")

data$integ <- data$cpi - data$ppi
ggplot(data, aes(y=integ, x = date)) + geom_point() + geom_smooth()

#### Testing for cointegration ####

adf <- ur.df(data$integ, type="none", selectlags= 'AIC')
summary(adf)

kpss <- ur.kpss(data$integ, type = 'mu', lags='short')
summary(kpss)

#### Modelling vector autoregression ####

model <- VAR(data[2:3], type='both', ic='AIC')
summary(model)

#### Granger Causality ####

causality(model, cause='ppi')

#### Plotting IRF ####

feir <- irf(model, impulse="ppi", response="cpi", n.ahead = 8, ortho = TRUE, runs = 1000)
plot(feir)
