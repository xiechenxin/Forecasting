library(readxl)
library(fpp2)
library(portes)

setwd("C:/Users/cxie/Desktop/Forecasting")

data <- read_excel("DataSets2020.xlsx", sheet = "Fatalities")

Fat <- ts(data[, 2], frequency = 1, start = 1965)

plot(Fat)
tsdisplay(Fat)

fat_train <- window(Fat, end = 2008)
fat_test <- window(Fat, start = 2009)

h <- length(fat_test) 

# naive
f1 <- rwf(fat_train, h=h)

# drift
f2 <- rwf(fat_train, drift = TRUE, h=h)


plot(Fat,main="Fatalities", ylab="",xlab="Day")
lines(f1$mean,col=4)
lines(f2$mean,col=2)

plot(f3)
lines(fat_test, col="red")

a1 <- accuracy(f1, fat_test)[,c(2,3,5,6)]
#                  RMSE      MAE      MAPE     MASE
# Training set 140.3844 107.2326  5.573924 1.000000
# Test set     239.7130 212.9000 30.502956 1.985404

a2 <- accuracy(f2, fat_test)[,c(2,3,5,6)]
#                   RMSE       MAE     MAPE      MASE
# Training set 136.42816 106.71282 5.347486 0.9951531
# Test set      42.53407  36.09302 5.085152 0.3365864

checkresiduals(f1)
res <- residuals(f1)
res <- na.omit(res)

LjungBox(res, lags=seq(1,12,3), order=0)
# lags statistic df   p-value
# 1    0.6465914  1 0.4213340
# 4    2.0700046  4 0.7228848
# 7    6.5571802  7 0.4763926
# 10   9.4440062 10 0.4905466

# make prediction on the whole data
f3 <- rwf(Fat, drift = TRUE, h=2)
plot(f3)
