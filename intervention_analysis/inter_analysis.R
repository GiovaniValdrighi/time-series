library(tseries)
library(zoo)
library(ggplot2)
library(forecast)
library(dplyr)
library(lmtest)
library(pander)
library(imager)
library(TSA)
library(tsoutliers)


## --------------------------------------------------------------------------------
data <- read.csv('TOTBUSSMNSA.csv')
title = 'Total Business Sales FRED (FED St. Louis Economics Research)'
#data before intervention
pre_intervention <- data %>% filter(DATE < as.Date("2008-09-01"))
fred_FIT <- data %>% filter(DATE < as.Date("2013-01-01"))
FRED_pre <- ts(pre_intervention$TOTBUSSMNSA,
           start = c(2002, 1),
           frequency = 12)


#fit data
FRED <- ts(fred_FIT$TOTBUSSMNSA, start = c(2002, 1), frequency = 12)

#all data
FRED_all <- ts(data$TOTBUSSMNSA, start = c(2002, 1), frequency = 12)
#check if variance is the same
autoplot(FRED, main = title) +
  scale_x_continuous(breaks = scales::extended_breaks(10))


## --------------------------------------------------------------------------------

#series need stabilization
lambda <- BoxCox.lambda(FRED_all)

#data after stabilization
autoplot(BoxCox(FRED, lambda), main = title) +
  scale_x_continuous(breaks = scales::extended_breaks(10))




## --------------------------------------------------------------------------------
autoplot(BoxCox(FRED_pre, lambda), main = title) + 
  scale_x_continuous(breaks = scales::extended_breaks(10))


## --------------------------------------------------------------------------------
#checking derivation

par(mfrow = c(2, 2))
FRED_pre %>% BoxCox(lambda = lambda) %>% plot(main = "Series")
FRED_pre %>% BoxCox(lambda = lambda) %>% diff() %>% plot(main = "diff Series")
FRED_pre %>% BoxCox(lambda = lambda) %>% diff() %>% diff() %>% plot(main = " diff diff Series")
FRED_pre %>% BoxCox(lambda = lambda) %>% diff() %>% diff(lag = 12) %>% plot(main = "diff diff(12) Series")



## --------------------------------------------------------------------------------

#checking the acfs
par(mfrow = c(2, 2))
FRED_pre %>% BoxCox(lambda = lambda) %>% diff() %>% acf(main = "diff Series", lag.max = 48)
FRED_pre %>% BoxCox(lambda = lambda) %>% diff(lag = 12) %>% acf(main = "diff(12) Series", lag.max =  48)

#checking the pacfs
#par(mfrow = c(1, 2))
FRED_pre %>% BoxCox(lambda = lambda) %>% diff() %>% pacf(main = "diff Series", lag.max = 48)
FRED_pre %>% BoxCox(lambda = lambda) %>%diff(lag = 12) %>% pacf(main = "diff(12) Series", lag.max =  48)


## --------------------------------------------------------------------------------
#ARIMA(2, 1,1)(3,0 ,1)
model1 <- Arima(FRED_pre, order = c(2, 1,1), seasonal = c(3, 0, 1), lambda = lambda)
summary(model1)


## --------------------------------------------------------------------------------
checkresiduals(model1, test = FALSE)


## --------------------------------------------------------------------------------
#ARIMA(2, 1, 1)(2, 0, 1)
model2 <- Arima(FRED_pre, order = c(2, 1,1), seasonal = c(2, 0, 1), lambda = lambda)
summary(model2)


## --------------------------------------------------------------------------------
checkresiduals(model2, test = FALSE)


## --------------------------------------------------------------------------------
#ARIMA(2, 1, 1)(2, 1, 1)
model3 <- Arima(FRED_pre, order = c(2, 1,1), seasonal = c(2, 1, 1), lambda = lambda)
summary(model3)


## --------------------------------------------------------------------------------
checkresiduals(model3, test = FALSE)


## --------------------------------------------------------------------------------

plot(FRED_pre, lwd = 1.5, main = "FRED pre intervention Real x Predicted SARIMA(2, 1, 1)(2,1,1)")
lines(model3$fitted, col = 'red', lwd = 1.5)
legend("topleft", legend=c("Predictions", "Real values"), col=c('red','black'), lty = 1:1, cex=0.8)


## --------------------------------------------------------------------------------
#pred of pre intervention
plot(forecast(model3, h = 48, lambda =lambda, biasadj = TRUE), main = "FRED series x Forecast of pre-intervention model SARIMA(2, 1,1)(2,1,1)")
lines(FRED)
legend("topleft", legend=c("Forecast mean",  "90%", "95%", "Real values"), col=c('blue','#596DD5', "#D5DBFF" ,'black' ), lty = 1:1, cex=0.8)


## --------------------------------------------------------------------------------
#julh of 2008 is the 79 row
#permanent const eff
permanent_const_eff <-  1*(seq_along(fred_FIT$DATE) >= 79)
model4 <- Arima(FRED, order = c(2, 1, 1), seasonal = c(2, 1, 1), xreg = permanent_const_eff, lambda = lambda)
summary(model4)


## --------------------------------------------------------------------------------
#julh of 2008 is the 79 row
#temporary const effect
temporary_const_eff <-  1*(seq_along(fred_FIT$DATE) == 79)
model5 <- Arima(FRED, order = c(2, 1, 1), seasonal = c(2, 1, 1), xreg = temporary_const_eff, lambda = lambda)
summary(model5)


## --------------------------------------------------------------------------------
#outliers analysis
outlier.FRED <- tsoutliers::tso(FRED,types = c("LS","TC"),maxit.iloop= 2)
plot(outlier.FRED)



## --------------------------------------------------------------------------------
#AR(1) transfer function
mod.arimax <- arimax(BoxCox(FRED, lambda), order=c(2, 1, 1),
                     seasonal=list(order=c(2, 1, 1), frequency=12),
                     include.mean=FALSE,
                     xtransf=temporary_const_eff,
                     transfer=list(c(1,0)), method = 'CSS')
summary(mod.arimax)


## --------------------------------------------------------------------------------
par(mfrow = c(1, 2))
plot(fitted(mod.arimax), main = "FRED Real x Predicted with decreasing effect", col = 'red', lwd = 2)
lines(BoxCox(FRED, lambda = lambda))
legend("topleft", legend=c("Predictions", "Real values"), col=c('red','black'), lty = 1:1, cex=0.8)

eval_temp_effect <- 0.2074 * 1*(seq_along(fred_FIT$DATE) == 79)
for(i in seq(81,132)){
  eval_temp_effect[i] = eval_temp_effect[i-1]*-0.2868
}
plot(ts(eval_temp_effect, start = c(2002, 1), frequency = 12), type = 'l', main = "AR(1) effect", ylab = "Z_t")


## --------------------------------------------------------------------------------
#but with transfer starting in august
mod.arimax2 <- arimax(BoxCox(FRED, lambda), order=c(2, 1, 1),
                     seasonal=list(order=c(2, 1, 1), frequency=12),
                     include.mean=FALSE,
                     xtransf=1*(seq_along(fred_FIT$DATE) == 80),
                     transfer=list(c(1,0)), method = 'CSS')
summary(mod.arimax2)


## --------------------------------------------------------------------------------
par(mfrow = c(1, 2))
plot(fitted(mod.arimax2), main = "FRED Real x Predicted with decreasing effect", col = 'red', lwd = 2)
lines(BoxCox(FRED, lambda = lambda))
legend("topleft", legend=c("Predictions", "Real values"), col=c('red','black'), lty = 1:1, cex=0.8)

eval_temp_effect2 <- -0.1517 *1*(seq_along(fred_FIT$DATE) == 80)
for(i in seq(81,132)){
  eval_temp_effect2[i] = eval_temp_effect2[i-1]*-0.1767
}
plot(ts(eval_temp_effect2, start = c(2002, 1), frequency = 12), type = 'l', main = "AR(1) effect", ylab = "Z_t")


## --------------------------------------------------------------------------------
#arimas model with transfer curves
#starting on july
model8 <- Arima(FRED, order = c(2, 1, 1), seasonal = c(2, 1, 1), lambda = lambda, xreg = eval_temp_effect)
summary(model8)


## --------------------------------------------------------------------------------
#starting on august
model9 <- Arima(FRED, order = c(2, 1, 1), seasonal = c(2, 1, 1), lambda = lambda, xreg = eval_temp_effect2)
summary(model9)


## --------------------------------------------------------------------------------
#two permanent interventions
n <- length(FRED)

mo.ls <- outliers("LS", c(80, 83))
ls <- outliers.effects(mo.ls, n)

xreg.outliers <- cbind(ls)

model7 <- Arima(FRED, order = c(2, 1, 1), seasonal = c(2, 1, 1), xreg = xreg.outliers, lambda = lambda)
summary(model7)



## --------------------------------------------------------------------------------
#one intervention of three months
short_const_effect = 1*(seq_along(fred_FIT$DATE) >=80 & seq_along(fred_FIT$DATE) <= 83)

model6 <- Arima(FRED, order = c(2, 1, 1), seasonal = c(2, 1, 1), xreg = short_const_effect, lambda = lambda)
summary(model6)



## --------------------------------------------------------------------------------
par(mfrow = c(2, 1))
plot(forecast(model8, h = 12, lambda =lambda, biasadj = TRUE, xreg = rep(0, 12)), main = "FRED series x Forecast of one decrescent intervention model 12 months")
t <- ts(data %>% filter(DATE < as.Date("2014-01-01")) %>% select(TOTBUSSMNSA), start = c(2002, 1), frequency =  12)
lines(t)
legend("topleft", legend=c("Forecast mean",  "90%", "95%", "Real values"), col=c('blue','#596DD5', "#D5DBFF" ,'black' ), lty = 1:1, cex=0.8)

plot(forecast(model8, h = 24, lambda =lambda, biasadj = TRUE, xreg = rep(0, 24)), main = "FRED series x Forecast of of one decrescent intervention model 24 months")
lines(FRED_all)
legend("topleft", legend=c("Forecast mean",  "90%", "95%", "Real values"), col=c('blue','#596DD5', "#D5DBFF" ,'black' ), lty = 1:1, cex=0.8)


## --------------------------------------------------------------------------------
par(mfrow = c(2, 1))
plot(forecast(model7, h = 12, lambda =lambda, biasadj = TRUE, xreg = as.matrix(data.frame(LS80 = rep(1, 12), LS83 = rep(1, 12)))), main = "FRED series x Forecast of two permanent interventions model 12 months")
t <- ts(data %>% filter(DATE < as.Date("2014-01-01")) %>% select(TOTBUSSMNSA), start = c(2002, 1), frequency =  12)
lines(t)
legend("topleft", legend=c("Forecast mean",  "90%", "95%", "Real values"), col=c('blue','#596DD5', "#D5DBFF" ,'black' ), lty = 1:1, cex=0.8)

plot(forecast(model7, h = 24, lambda =lambda, biasadj = TRUE, xreg = as.matrix(data.frame(LS80 = rep(1, 24), LS83 = rep(1, 24)))), main = "FRED series x Forecast of two permanent interventions model 24 months")
lines(FRED_all)
legend("topleft", legend=c("Forecast mean",  "90%", "95%", "Real values"), col=c('blue','#596DD5', "#D5DBFF" ,'black' ), lty = 1:1, cex=0.8)

