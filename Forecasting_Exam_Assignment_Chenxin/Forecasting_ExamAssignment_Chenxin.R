# Forecasting exam assignment
# Chenxin Xie


# load library
library(readxl)
library(fpp2)
library(portes)
library(tseries)

# set working directory
setwd("C:/Users/cxie/Desktop/Forecasting/Assignment")

#------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------- Exercise 1 -----------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------#

# read in data
data <- read_excel("DataSets2020.xlsx", sheet="Turnover")

# change to time series data
turnover <- ts(data$Turnover, frequency=12, start = c(2000, 1), end = c(2020, 1))


#split train and test
train <- window(turnover, start = c(2000, 1), end = c(2015, 12))
test <- window(turnover, start = c(2016, 1), end = c(2020, 1))
h <- length(test) 

#-------------------------------------------------- Question 1 ----------------------------------------------------#

# 1. Explore the data using relevant graphs, and discuss the properties of the data.
plot(turnover, main = 'Belgian industry turnover index for manufacturing of beverages', xlab ='Year')

seasonplot(turnover,year.labels=TRUE, year.labels.left=TRUE,
           main="Belgian industry turnover index for manufacturing of beverages",
           ylab="Index(2015=100)",col=rainbow(20), pch=19)

monthplot(turnover, ylab="Index(2015=100)",xlab="Month", xaxt="n",
          main="Seasonal subseries plot: Belgian industry turnover index",
          type="l")
axis(1, at=1:12, labels=month.abb, cex=0.8)


#-------------------------------------------------- Question 2 ----------------------------------------------------#

# 2.Discuss whether a transformation of the data would be useful. If so, select the most appropriate transformation.

Acf(turnover)

trans_data <- BoxCox(turnover, lambda = 0)
plot(trans_data, main = 'Box-Cox transformation on Turnover', xlab ='Year', ylab= "Index(2015=100)")
lines(turnover, col='blue')

tsdisplay(trans_data)
tsdisplay(turnover)

#-------------------------------------------------- Question 3 ----------------------------------------------------#

# 3. Create forecasts using the seasonal naive method. Check the residual diagnostics and the forecast accuracy.
  
sn_f <- snaive(train, h = h)
plot(turnover, main = 'Forecast using Seasonal Naïve Method', 
     ylab="Index(2015=100)", xlab="Year")
lines(sn_f$mean,col=4)


# residual
checkresiduals(sn_f)
#            Ljung-Box test
# 
# data:  Residuals from Seasonal naive method
# Q* = 70.988, df = 24, p-value = 1.548e-06
# 
# Model df: 0.   Total lags used: 24

res <- residuals(sn_f)
tsdisplay(res)

# accuarcy
accuracy(sn_f,test)
#                     ME      RMSE       MAE      MPE     MAPE     MASE       ACF1 Theil's U
# Training set  2.659611  6.612143  5.158722  3.09471  6.23105 1.000000 0.04369396        NA
# Test set     21.338367 24.051192 21.338367 17.11021 17.11021 4.136367 0.52611586  1.926643

#-------------------------------------------------- Question 4 ----------------------------------------------------#

# 4. Use an STL decomposition to forecast the turnover index. Use the appropriate underlying methods to do so. 
#   Check the residual diagnostics and the forecast accuracy.


stl_de <- stl(train, s.window = 5, robust=TRUE) 
fcast_de <- forecast(stl_de, method="rwdrift", h=h)
plot(fcast_de)
lines(turnover) 

res_de <- residuals(fcast_de)
tsdisplay(res_de)
Box.test(res_de, lag=10, fitdf=0, type="Lj")
#      Box-Ljung test
# 
# data:  res_de
# X-squared = 74.678, df = 10, p-value = 5.496e-12


accuracy(fcast_de,test)
#                        ME      RMSE       MAE        MPE      MAPE      MASE       ACF1 Theil's U
# Training set -3.42241e-15  5.187418  3.599768 -0.1294216  4.483651 0.6978023 -0.4871314        NA
# Test set      1.53254e+01 17.322163 15.325395 12.2741012 12.274101 2.9707735  0.3556888  1.390659

#-------------------------------------------------- Question 5 ----------------------------------------------------#

# 5. Generate forecasts using Holt-Winters' method. Check the residual diagnostics and the forecast accuracy.

fcast_holtM <- hw(train, seasonal="mult", h=h)
fcast_holtA <- hw(train, seasonal="additive", h=h)
plot(turnover,ylab="Index(2015=100)", shadecols = "white",
     type="o", fcol="white", xlab="Year")
lines(fitted(fcast_holtA), col="red", lty=2)
lines(fitted(fcast_holtM), col="green", lty=2)
lines(fcast_holtA$mean, type="o", col="red")
lines(fcast_holtM$mean, type="o", col="green")
legend("topleft",lty=1, pch=1, col=1:3,
       c("data","Holt Winters' Additive",
         "Holt Winters' Multiplicative"))

checkresiduals(fcast_holtM)
checkresiduals(fcast_holtA)

res_holtM <- residuals(fcast_holtM)
LjungBox(res_holtM, lags=24, order=length(fcast_holtM$model$par))
# lags statistic df      p-value
# 24  93.38013  8 1.110223e-16


res_holtA <- residuals(fcast_holtA)
LjungBox(res_holtA, lags=24, order=length(fcast_holtA$model$par))
# lags statistic df      p-value
# 24  83.16741  8 1.121325e-14


accuracy(fcast_holtM, test)[,c(2,3,5,6)]
#                  RMSE       MAE      MAPE      MASE
# Training set  4.30480  3.333093  4.152035 0.6461082
# Test set     15.46606 13.353027 10.681029 2.5884369

accuracy(fcast_holtA, test)[,c(2,3,5,6)]
#                   RMSE       MAE      MAPE      MASE
# Training set  4.389998  3.549698  4.474374 0.6880964
# Test set     16.077726 13.911877 10.987226 2.6967680


#-------------------------------------------------- Question 6 ----------------------------------------------------#

# 6. Generate forecasts using ETS. First select the appropriate model(s) yourself and discuss their performance. 
#    Compare these models with the results of the automated ETS procedure. 
#   Check the residual diagnostics and the forecast accuracy for the various ETS models you've considered.

fc_MAA <- forecast(ets(train, model="MAA"), h=h)
plot(fc_MAA)
lines(turnover)

fc_ETS <- forecast(ets(train), h=h)
plot(fc_ETS)
lines(turnover)
summary(ets(train))

checkresiduals(fc_MAA)
checkresiduals(fc_ETS)

accuracy(fc_MAA, test)[,c(2,3,5,6)]
#                   RMSE      MAE      MAPE      MASE
# Training set  4.395102  3.53572  4.464581 0.6853868
# Test set     15.317092 13.16739 10.385782 2.5524517


accuracy(fc_ETS, test)[,c(2,3,5,6)]
#                   RMSE       MAE      MAPE      MASE
# Training set  4.288583  3.367403  4.193484 0.6527591
# Test set     15.164648 13.060222 10.444933 2.5316777


#-------------------------------------------------- Question 7 ----------------------------------------------------#

# 7. Generate forecasts using ARIMA. First select the appropriate model(s) yourself and discuss their performance. 
#    Compare these models with the results of the auto.arima procedure. 
#    Check the residual diagnostics and the forecast accuracy for the ARIMA models you've considered.


tsdisplay(diff(train))

fc_ets <- forecast(ets(train, lambda=0), h=h)
plot(fc_ets)
lines(turnover)
summary(ets(train, lambda=0))

checkresiduals(fc_ets)

accuracy(fc_ets, test)[,c(2,3,5,6)]
#                  RMSE      MAE     MAPE      MASE
# Training set  4.28433  3.39688 4.237747 0.6584731
# Test set     12.94274 11.04910 8.848813 2.1418282

ARIMA_1 <- Arima(train, lambda=0, order=c(0,1,0), include.drift=TRUE)
fc_A1 <- forecast(ARIMA_1, h=h)
plot(fc_A1)
lines(turnover)

checkresiduals(ARIMA_1)

LjungBox(fc_A1$residuals, lags=seq(4,24,4), order=length(fc_A1$coef))

accuracy(fc_A1, test)[,c(2,3,5,6)]
#                   RMSE       MAE      MAPE     MASE
# Training set  9.081055  7.143836  8.809198 1.384807
# Test set     16.884013 14.115244 11.184654 2.736190

ARIMA_2 <- Arima(train, lambda=0,order=c(1,0,0), seasonal=c(0,1,3), include.drift=TRUE)
fc_A2 <- forecast(ARIMA_2, h=h)
plot(fc_A2)
lines(turnover)

checkresiduals(ARIMA_2)

LjungBox(fc_A2$residuals, lags=seq(4,24,4), order=length(fc_A2$coef))

summary(fc_A2)
# Forecast method: ARIMA(1,0,0)(0,1,3)[12] with drift
# 
# Model Information:
#   Series: train 
# ARIMA(1,0,0)(0,1,3)[12] with drift 
# Box Cox transformation: lambda= 0 
# 
# Coefficients:
#          ar1     sma1     sma2    sma3   drift
#       0.1329  -0.7688  -0.2506  0.0194  0.0028
# s.e.  0.0785   0.1557   0.1035  0.1026  0.0001
# 
# sigma^2 estimated as 0.003386:  log likelihood=244.28
# AIC=-476.55   AICc=-476.07   BIC=-457.39
# 
# Error measures:
#                     ME     RMSE      MAE       MPE     MAPE      MASE        ACF1
# Training set 0.4417251 4.567583 3.512183 0.4994677 4.275679 0.6808243 -0.04844832

accuracy(fc_A2, test)[,c(2,3,5,6)]
#                   RMSE      MAE     MAPE      MASE
# Training set  4.567583 3.512183 4.275679 0.6808243
# Test set     11.607418 9.554709 7.630307 1.8521465



ARIMA_auto <- auto.arima(train, lambda = 0, allowdrift = TRUE)
summary(ARIMA_auto)
# Series: train 
# ARIMA(1,0,1)(2,1,2)[12] with drift 
# Box Cox transformation: lambda= 0 
# 
# Coefficients:
#          ar1      ma1    sar1     sar2    sma1    sma2   drift
#       0.9867  -0.8182  0.6514  -0.3823  -1.546  0.7263  0.0031
# s.e.  0.0146   0.0467  0.1360   0.0911   0.162  0.1558  0.0009
# 
# sigma^2 estimated as 0.002921:  log likelihood=261.19
# AIC=-506.37   AICc=-505.53   BIC=-480.83
# 
# Training set error measures:
#                    ME     RMSE     MAE       MPE     MAPE      MASE       ACF1
# Training set -0.19898 4.238087 3.21202 -0.374753 3.913788 0.6226386 -0.1100893


fc_AUTO <- forecast(ARIMA_auto, h=h)

plot(fc_AUTO)
lines(turnover)

checkresiduals(ARIMA_auto)

LjungBox(fc_AUTO$residuals, lags=seq(4,24,4), order=length(fc_AUTO$coef))


accuracy(fc_AUTO, test)[,c(2,3,5,6)]
#                   RMSE      MAE      MAPE      MASE
# Training set  4.238087  3.21202  3.913788 0.6226386
# Test set     14.812742 12.84807 10.263672 2.4905527

#-------------------------------------------------- Question 8 ----------------------------------------------------#

# 8. Compare the difierent models in terms of residual diagnostics, model fit, and forecast accuracy. 
#    Analyse your results and select your final model.

checkresiduals(sn_f)             # seasonal naïve 
checkresiduals(fcast_de)         # random walk with drift after STL decomposition
checkresiduals(fcast_holtM)      # Holt-Winters' Multiplicative
checkresiduals(fcast_holtA)      # Holt-Winters' Additive
checkresiduals(fc_MAA)           # ETS(M,A,A)
checkresiduals(fc_ETS)           # auto ETS, ETS(M,A,M)
checkresiduals(fc_ets)           # auto ETS with log transformation, ETS(A,A,A) 
checkresiduals(fc_A1)            # ARIMA(0,1,0) with drift 
checkresiduals(fc_A2)            # ARIMA(1,0,0)(0,1,3)12 with drift
checkresiduals(fc_AUTO)          # auto ARIMA


accuracy(sn_f,test)[,c(2,3,5,6)]               # seasonal naïve 
accuracy(fcast_de,test)[,c(2,3,5,6)]           # random walk with drift after STL decomposition
accuracy(fcast_holtM, test)[,c(2,3,5,6)]       # Holt-Winters' Multiplicative
accuracy(fcast_holtA, test)[,c(2,3,5,6)]       # Holt-Winters' Additive
accuracy(fc_MAA, test)[,c(2,3,5,6)]            # ETS(M,A,A)
accuracy(fc_ETS, test)[,c(2,3,5,6)]            # auto ETS, ETS(M,A,M)
accuracy(fc_ets, test)[,c(2,3,5,6)]            # auto ETS with log transformation, ETS(A,A,A) 
accuracy(fc_A1, test)[,c(2,3,5,6)]             # ARIMA(0,1,0) with drift 
accuracy(fc_A2, test)[,c(2,3,5,6)]             # ARIMA(1,0,0)(0,1,3)12 with drift
accuracy(fc_AUTO, test)[,c(2,3,5,6)]           # auto ARIMA


#-------------------------------------------------- Question 9 ----------------------------------------------------#

# 9. Generate out of sample forecasts up to December 2020, based on the complete time series. Discuss your results.

ARIMA_9 <- Arima(turnover, lambda=0,order=c(1,0,0), seasonal=c(0,1,3), include.drift=TRUE)
fc_A9 <- forecast(ARIMA_9, h=11)
plot(fc_A9)

checkresiduals(fc_A9)

#      Ljung-Box test
# 
# data:  Residuals from ARIMA(1,0,0)(0,1,3)[12] with drift
# Q* = 116.78, df = 19, p-value = 4.441e-16
# 
# Model df: 5.   Total lags used: 24


#-------------------------------------------------- Question 10 ----------------------------------------------------#

# 10. In addition, generate the seasonally adjusted time series for the Turnover data.
#     Estimate an (auto-)ARIMA model and an (auto-)ETS model for this non-seasonal time series. 
#     Compare the forecast accuracy and residual diagnostics of both models, 
#     and select the final model for the seasonally adjusted series. Generate out of sample forecasts up to December 2020, 
#     based on the complete time series. Discuss your results.

turnover_ns <- seasadj(decompose(turnover, type="multiplicative"))
plot(turnover, col="grey", main="Seasonally adjusted", xlab='Year')
lines(trunover_ns,col="red")

train_ns <- seasadj(decompose(train, type="multiplicative"))

# Auto ARIMA
ns_ARIMA_auto <- auto.arima(train_ns, lambda = 0, allowdrift = TRUE)
fc_AA_ns <- forecast(ns_ARIMA_auto, h=h)

summary(ns_ARIMA_auto)

plot(fc_AA_ns)
lines(trunover_ns, col='red')

checkresiduals(fc_AA_ns)

accuracy(fc_AA_ns, test)[,c(2,3,5,6)]  
#                   RMSE       MAE      MAPE      MASE
# Training set  3.941302  3.087519  3.825479 0.5988828
# Test set     15.781383 13.056640 10.413300 2.5325829


# Auto ETS

fc_AE_ns <- forecast(ets(train_ns, lambda=0), h=h)  
plot(fc_AE_ns)
lines(trunover_ns, col='red')

summary(ets(train_ns, lambda=0))

checkresiduals(fc_AE_ns)

accuracy(fc_AE_ns, test)[,c(2,3,5,6)]
#                   RMSE       MAE      MAPE      MASE
# Training set  4.280711  3.388989  4.238444 0.6573587
# Test set     16.735907 13.640148 10.787890 2.6457653


# forecasts up to December 2020
ARIMA_10 <- auto.arima(turnover_ns, lambda = 0, allowdrift = TRUE)
fc_A10 <- forecast(ARIMA_10, h=11)
plot(fc_A10)

summary(ARIMA_10)

checkresiduals(ARIMA_10)

#------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------- Exercise 2 -----------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------#

# read in data
data2 <- read_excel("ALIBABA.xlsx", sheet="closePrice")

# change to time series data
baba <- ts(data2$Close, frequency=12, start = c(2014, 9), end = c(2020, 4))


# plot the time series
plot(baba, main = 'Alibaba Stock Close Price ', xlab ='Year')

seasonplot(baba,year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal plot on Alibaba Stock Close Price",
           col=rainbow(20), pch=18)

monthplot(baba, xaxt="n",
          main="Monthly plot on Alibaba Stock Close Price",
          type="l")
axis(1, at=1:12, labels=month.abb, cex=0.8)

# Box-Cox
trans_baba <- BoxCox(baba, lambda = 0)
plot(trans_baba, main = 'Box-Cox transformation on Alibaba stock')

tsdisplay(baba)
tsdisplay(trans_baba)


dif_baba <- diff(baba)

tsdisplay(dif_baba)

adf.test(dif_baba)

ddif_baba <- diff(diff(baba))

tsdisplay(ddif_baba)

adf.test(ddif_baba)

#split train and test
train <- window(baba, start = c(2014, 9), end = c(2019, 3))
test <- window(baba, start = c(2019, 4), end = c(2020, 4))
h <- length(test) 


# random walk with drift

baba_rwd <- rwf(train, drift=TRUE, h=h)
fc_baba_rwd <- forecast(baba_rwd)

plot(fc_baba_rwd)
lines(baba)

checkresiduals(fc_baba_rwd)

#          Ljung-Box test
# 
# data:  Residuals from Random walk with drift
# Q* = 6.6586, df = 10, p-value = 0.7572
# 
# Model df: 1.   Total lags used: 11


accuracy(fc_baba_rwd, test)[,c(2,3,5,6)]

#                  RMSE       MAE     MAPE      MASE
# Training set 12.37771  9.982345 8.443669 0.2693783
# Test set     16.58614 13.438719 7.763680 0.3626502


# auto ARIMA
baba_ARIMA_auto <- auto.arima(train, lambda = 0, allowdrift = TRUE)
fc_baba_aa <- forecast(baba_ARIMA_auto, h=h)

summary(baba_ARIMA_auto)

plot(fc_baba_aa)
lines(baba)

checkresiduals(fc_baba_aa)

#          Ljung-Box test
# 
# data:  Residuals from ARIMA(0,1,0)
# Q* = 10.687, df = 11, p-value = 0.4699
# 
# Model df: 0.   Total lags used: 11


accuracy(fc_baba_aa, test)[,c(2,3,5,6)]

#                  RMSE       MAE     MAPE      MASE
# Training set 12.38446  9.719959 8.121317 0.2622977
# Test set     18.96569 16.666924 8.982741 0.4497648



# auto ETS

baba_ets = ets(train, lambda=0)
fc_baba_ets <- forecast(baba_ets, h=h)

plot(fc_baba_ets)
lines(baba)

summary(baba_ets)

checkresiduals(fc_baba_ets)

#         Ljung-Box test
# 
# data:  Residuals from ETS(A,N,N)
# Q* = 10.907, df = 9, p-value = 0.2822
# 
# Model df: 2.   Total lags used: 11



accuracy(fc_baba_ets, test)[,c(2,3,5,6)]

#                  RMSE       MAE     MAPE      MASE
# Training set 12.36090  9.736325 8.144745 0.2627393
# Test set     18.96474 16.666544 8.982776 0.4497546



# make prediction with final model:random walk with drift

baba_fin <- rwf(baba, drift=TRUE, h=8)
fc_baba_fin <- forecast(baba_fin)

plot(fc_baba_fin)

checkresiduals(fc_baba_fin)
#             Ljung-Box test
# 
# data:  Residuals from Random walk with drift
# Q* = 15.78, df = 13, p-value = 0.2612
# 
# Model df: 1.   Total lags used: 14





