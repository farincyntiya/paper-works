##### Perbandingan Metode SES, Holt’s Linear, Holt’s Winter #####
########### untuk Peramalan Jumlah Penumpang Pesawat ############
######## Penerbangan Domestik di Bandara Soekarno-Hatta #########


library(dplyr)
library(ggplot2)
library(forecast)
library(fpp2)

#===== Input Data
data=read.csv(file.choose(),sep=";",dec=",",header=T)
attach(data)
head(data)
str(data)

summary(data)

#===== Plot Data Time Series
y = ts(data,frequency=12,start=c(2006,1),end=c(2021,4));y
ts.plot(y,ylab="Jumlah Penumpang",main="Plot Jumlah Penumpang Pesawat Domestik Bandara Soekarno Hatta 2006-2021",col="lightcoral",lwd=2)
legend("bottomright",c("Penumpang"),cex=0.6,lty=2,text.font=1,col=c("lightcoral"))

## METODE SINGLE EXPONENTIAL SMOOTHING ##
# Estimate parameters
fc <- ses(y,h=8);fc
summary(fc)
# Accuracy of one step ahead training errors
round(accuracy(fc),2)

autoplot(y) +
autolayer(fc,series="Fitted") +
  ylab("Jumlah Penumpang") +
  ggtitle("Plot Jumlah Penumpang Pesawat Domestik Bandara Soekarno Hatta 2006-2021")+
  guides(colour=guide_legend(title="Forecast"))

## METODE HOLT'S LINEAR ##
library(tseries)
library(forecast)

penumpang1 <- window(y,start=2006)
fc1 <- holt(penumpang1,h=8)
fc2 <- holt(penumpang1,damped=TRUE,phi=0.9,h=5)
autoplot(penumpang1) +
  autolayer(fc1, series="Holt's method") +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Jumlah Penumpang Pesawat Domestik Bandara Soekarno Hatta 2006-2021") +
  guides(colour=guide_legend(title="Forecast"))
summary(fc1)
summary(fc2)
round(accuracy(fc1),2)
round(accuracy(fc2),2)

## METODE HOLT'S WINTER ADDITIVE DAN MULTIPLICATIVE ##
penumpang2 <- window(y,start=2006)
fit1 <- hw(penumpang2,seasonal="additive")
fit2 <- hw(penumpang2,seasonal="multiplicative")
summary(fit1)
summary(fit2)
round(accuracy(fit1),2)
round(accuracy(fit2),2)
autoplot(penumpang2) +
autolayer(fit1, series="HW additive forecasts",PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",PI=FALSE) +
  xlab("Bulan") +
  ylab("Jumlah Penumpang") +
  ggtitle("Plot Jumlah Penumpang Pesawat Domestik Bandara Soekarno Hatta 2006-2021")+
  guides(colour=guide_legend(title="Forecast"))