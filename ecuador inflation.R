#### Loading Packages ####

library(readxl)
library(vars)
library(ggplot2)
library(urca)
library(e1071)

#### Loading Data ####

data <- read.csv('CPI_and_PPI.csv')
data <- data[-1]
colnames(data) <- c('date','cpi', 'ppi')
data$cpi <- data$cpi / 100
data$ppi <- data$ppi / 100

#### Summary Statistics ####
head(data)
summary(data)
mn <- cbind((mean(data$cpi)), mean(data$ppi))
sd <- cbind(sd(data$cpi), sd(data$ppi))
sk <- cbind(skewness(data$cpi), skewness(data$ppi))
kt <- cbind(kurtosis(data$cpi), kurtosis(data$ppi))

tab <- as.data.frame(rbind(mn,sd,sk,kt))
rownames(tab) <- c('Mean', 'Standard Deviation', 'Skewness', 'Kurtosis')
colnames(tab) <- c('CPI', 'PPI')
print(tab)
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

plot("Hello world")
