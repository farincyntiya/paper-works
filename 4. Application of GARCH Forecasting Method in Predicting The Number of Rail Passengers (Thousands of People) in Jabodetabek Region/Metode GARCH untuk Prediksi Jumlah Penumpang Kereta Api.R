##### Application of GARCH Forecasting Method in #####
###### Predicting The Number of Rail Passengers ######
##### (Thousands of People) in Jabodetabek Region ####


library(dplyr)
library(ggplot2)
library(forecast)
library(fpp2)
library(FitAR) 
library(lmtest) 
library(tseries) 
library(psych) 
library(readr) 
library(tsoutliers)

#===== Input Data
# Mengubah data ke time series
penumpang=read.csv(file.choose(),header=TRUE,sep=";")
penumpang
is.ts(penumpang)
typeof(penumpang)
penumpang[,-1]
t(penumpang[,-1])
as.numeric(t(penumpang[,-1]))
penumpangKAI=as.numeric(t(penumpang[,-1]))
summary(penumpangKAI)
penumpangTS=ts(penumpangKAI,start=c(2011,1),end=c(2020,12),frequency=12)
penumpangTS
is.ts(penumpangTS)
glimpse(penumpangTS)

#===== Plot Data
# Melihat Pola Sebaran Data
ts.plot(penumpangTS,ylab="Jumlah Penumpang (ribu orang)",xlab="Tahun",
        main="Data Jumlah Penumpang Kereta Api (Ribu Orang), 2011-2020",
        col="lightseagreen",lwd=2) 

#===== Plot ACF dan PACF
# Plotting ACF dan PACF
par(mfrow=c(2,1))
acf(penumpangTS,lag.max=71)
# Terlihat bahwa pada ACF terjadi dies down
pacf(penumpangTS,lag.max=71)
# Terlihat bahwa pada PACF terjadi cut off after lag 1
ggAcf(penumpangTS,lag.max=71) + ggtitle("Plot ACF sebelum differencing")
ggPacf(penumpangTS,lag.max=71) + ggtitle("Plot PACF sebelum differencing")
#Maka data tidak stasioner sehingga data perlu distasionerkan

#===== Uji Stasioneritas dalam Varians
# Transformasi Box Cox
lamda=BoxCox.lambda(penumpangTS);lamda
tf=((penumpangTS^lamda)-1)/lamda
BoxCox.lambda(tf)

par(mfrow=c(2,1))
# Data sebelum transformasi
ts.plot(penumpangTS,ylab="Banyak Penumpang (ribu orang)",xlab="Tahun",
        main="Data Jumlah Penumpang Kereta Api (Ribu Orang), 2011-2020 (Sebelum Transformasi)",
        col="darkviolet",lwd=2) 
# Data setelah transformasi
ts.plot(tf,ylab="Banyak Penumpang (ribu orang)",xlab="Tahun",
        main="Data Jumlah Penumpang Kereta Api (Ribu Orang), 2011-2020 (Sesudah Transformasi)",
        col="deeppink",lwd=2) 

#===== Uji Stasioneritas dalam Rata-rata
# ADF Test
adf.test(tf)
adf.test(tf,k=12)
# Melakukan differencing
df.series=diff(penumpangTS, 1)
adf.test(df.series)

par(mfrow=c(2,1))
# Data sebelum differencing
ts.plot(penumpangTS,ylab="Banyak Penumpang (ribu orang)",xlab="Tahun",
        main="Data Jumlah Penumpang Kereta Api (Ribu Orang), 2011-2020 (Sebelum Differencing)",
        col="darkviolet",lwd=2) 
# Data setelah differencing
ts.plot(df.series,ylab="Banyak Penumpang (ribu orang)",xlab="Tahun",
        main="Data Jumlah Penumpang Kereta Api (Ribu Orang), 2011-2020 (Sesudah Differencing)",
        col="deeppink",lwd=2) 

#===== Penentuan Orde ARIMA
par(mfrow=c(2,1))
acf(df.series,lag.max=71) 
pacf(df.series,lag.max=71) 
ggAcf(df.series,lag.max=71) + ggtitle("Plot ACF setelah differencing")
ggPacf(df.series,lag.max=71) + ggtitle("Plot PACF setelah differencing")

# Identifikasi Model dengan ACF dan PACF
ggAcf(df.series)+ ggtitle("Plot ACF setelah differencing")
ggPacf(df.series)+ ggtitle("Plot PACF setelah differencing")
# Maka model ARMA(p,q)

#===== Pembentukan Model ARIMA
model1=arima(penumpangTS,order=c(0,1,1)) ; model1
model2=arima(penumpangTS,order=c(0,1,2)) ; model2
model3=arima(penumpangTS,order=c(1,1,0)) ; model3
model4=arima(penumpangTS,order=c(1,1,1)) ; model4
model5=arima(penumpangTS,order=c(1,1,2)) ; model5
model6=arima(penumpangTS,order=c(2,1,0)) ; model6
model7=arima(penumpangTS,order=c(2,1,1)) ; model7

#===== Uji Signifikansi Model ARIMA
coeftest(model1)
coeftest(model2)
coeftest(model3)
coeftest(model4)
coeftest(model5)
coeftest(model6)
coeftest(model7)

### Memeriksa AIC Paling Kecil ###
# Model yang signifikan adalah model 4, 5, dan 7
list(AIC(model4),AIC(model5),AIC(model7))
# Ternyata model AIC paling kecil ada pada model 4
# Didapat model ARIMA terbaik
summary(model4)

###M emeriksa Kecocokan Model ###
# Diagnostik Model
# Residual
r=residuals(model4)

### ASUMSI PERTAMA ###
#===== Uji Normalitas Data
# H0: residual berdistribusi normal
# H1: residual tidak berdistribusi normal
n=length(r)
mean=mean(r)
sd=sd(r)
res=rnorm(n,mean,sd)
cek.normalitas=ks.test(r,res)
cek.normalitas

### ASUMSI KEDUA ###
#===== Uji White Noise-Autokorelasi
# H0: residual white noise
# H1: residual tidak white noise
cek.WNA=Box.test(r,lag=1,type=c("Ljung-Box"))
cek.WNA

### ASUMSI KETIGA ###
#===== Uji White Noise-Heteroskedastisitas
# Pengujian residual model 3
# H0: residual homogen (homoskedastisitas)
# H1: residual heterogen (heteroskedastisitas)
h=r^2
cek.heteros=Box.test(h,lag=1,type=c("Ljung-Box"))
cek.heteros

#===== Pembentukan Model ARCH-GARCH
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(rugarch)
require("rugarch")

#===== Uji Keberadaan Efek ARCH
library(dynlm)

# Step 1: Estimate mean equation r = beta + error
penumpang.mean <- dynlm(penumpangTS ~ 1, data = penumpangKAI)

# Step 2: Retrieve the residuals from the former model and square them
ehatsq <- ts(resid(penumpang.mean)^2)

# Step 3: regress squared residuals on one-lagged squared residuals
penumpang.arch <- dynlm(ehatsq ~ L(ehatsq), data = ehatsq)
summary(penumpang.arch)
library(FinTS)
penumpang.archTest <- ArchTest(penumpangTS, lags = 1, demean = TRUE)
penumpang.archTest
# H0 ditolak: terdapat efek ARCH

# Residual
res.arimamodel<-residuals(model4)
# Kuadrat Residual
squared.res.arimamodel=res.arimamodel^2
# Plot Kuadrat Residual
plot(squared.res.arimamodel,main="Plot Squared Residuals Model ARIMA(1,1,1)",
     ylab="Squared Residuals",xlab="Time",col="darkseagreen",lwd=2)
# Ada yang tinggi jadi terlihat ada efek ARCH
ggAcf(squared.res.arimamodel,lag.max=25) + ggtitle("Plot ACF squared residual ARIMA (1,1,1)")
# Terlihat bahwa pada ACF terjadi cut off after lag 1
ggPacf(squared.res.arimamodel,lag.max=25) + ggtitle("Plot PACF squared residual ARIMA (1,1,1)")
# Terlihat bahwa pada PACF terjadi cut off after lag 1

help(garch)

## PEMODELAN ARCH GARCH ##
model.garch1=ugarchspec(mean.model=list(armaOrder=c(1,1),include.mean=TRUE),
                        variance.model=list(model="sGARCH",garchOrder=c(0,1)),
                        distribution.model="norm")
model.garch.fit1=ugarchfit(data=penumpangTS,spec=model.garch1,out.sample=12)
model.garch.fit1

model.garch2=ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                        mean.model=list(armaOrder=c(1,1),include.mean=TRUE),
                        distribution.model="norm")
model.garch.fit2=ugarchfit(data=penumpangTS,spec=model.garch2,out.sample=12)
model.garch.fit2

#===== Forecasting Model GARCH(1,1)
ugarchforecast(model.garch.fit2,data=penumpangTS,n.ahead=24,n.roll=0)

#=====  Plot Peramalan
fc=ugarchforecast(model.garch.fit2,data=penumpangTS,n.ahead=24,n.roll=0)
fc
plot(fc)

#===== Perhitungan Akurasi Peramalan
# Model GARCH(1,1)
actual=read.csv(file.choose(),header=T,sep=";",dec=",")
predicted=read.csv(file.choose(),header=T,sep=";",dec=",")

library(MLmetrics)
mape=MAPE(predicted[,1],actual[,1])
mape