library(readxl)
library(fpp2)
library(fpp)
library(forecast)
library(portes)

setwd("C:/Users/cxie/Desktop/Forecasting")

data <- read_excel("DataSets2020.xlsx", sheet = "RetailSalesVolume")

rsv <- ts(data[, 2], frequency = 12, start = c(1998,1))

rsv1 <- window(rsv, end=c(2016,12))
rsv2 <- window(rsv, start=c(2017,1))

h=length(rsv2)

plot(rsv)
lines(rsv1, col='red')
lines(rsv2, col='blue')

seasonplot(rsv, col=rainbow(20), pch=19)
monthplot(rsv, type="l")

# seasonal naive
sn <- snaive(rsv1, h=h)
a_sn <- accuracy(sn, rsv2)[, c(2,3,5,6)]
a_sn
#                  RMSE      MAE     MAPE      MASE
# Training set 3.412852 2.775642 3.326379 1.0000000
# Test set     3.201457 2.739826 2.664475 0.9870963

plot(rsv)
lines(sn$mean, col=4)