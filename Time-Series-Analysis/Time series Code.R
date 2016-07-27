##Time Series Analysis STAT 650 Submission by Vinay Vernekar


#####################################################################################
#                                   Mandatory Code                                  #                                   #
#####################################################################################
library(tseries)
library(forecast)
library(lmtest)
library(astsa)

setwd("C:\\Second Sem\\Time Series Analysis\\final_time_series")
D=read.csv("time_series_final.csv")
x=c(7,13,15)
mainData=D[,x]
head(mainData)


# Normalising the data
SNP = (mainData$SNP_volume - mean(mainData$SNP_volume))/sd(mainData$SNP_volume)
NIK = (mainData$Nikki_volume - mean(mainData$Nikki_volume))/sd(mainData$Nikki_volume)





#####################################################################################
#                         Code For Figure 1 and Figure 2                            #                                   #
#####################################################################################


###Plotting original- normalized data#####
plot(SNP,type="l",xlab="time", main="SNP") # Code for Figure 1
plot(NIK,type="l",xlab="time", main="Nikkei") # Code for Figure 2



#####################################################################################
#                         Code For Figure 3,Figure 4, Figure 5 and Figure 6         #                                   #
#####################################################################################
# plotting periodogram
hrper = spec.pgram(SNP,taper=0,log="no") # code for Figure 3
spec.pgram(SNP,spans=c(20,20),taper=0,log="no") # code for Figure 4

abline(v=1/250, lty="dotted",col="red")
abline(v=1/30, lty = "dotted",col="red")
abline(v=1/10, lty = "dotted", col="red")
text(1/250,9, "250")
text(1/30,2.5, "30")
text(1/10,2, "10")

hrper = spec.pgram(NIK,taper=0,log="no") # code for Figure 5
spec.pgram(NIK,spans=c(20,20),taper=0,log="no") # code for Figure 6

abline(v=1/280, lty="dotted",col="red")
abline(v=1/40, lty = "dotted",col="red")
text(1/290,26, "290")
text(1/38,4, "38")


#####################################################################################
#              Stationay Series Plot Figure 7 and Figure 8                          #                                   #
#####################################################################################
# taking first lag and plotting them
first_lag1=diff(SNP,1)
plot(first_lag1,type="l",xlab="time", main="First Lag for SNP")#code for Figure 7


first_lag1_Nik=diff(NIK,1)
plot(first_lag1_Nik,type="l",xlab="time", main="First Lag for Nikkei")#code for Figure 8



#####################################################################################
#            Code For Figure 9,Figure 10, Figure 11, Figure 12  and Figure 13           #                                   #
#####################################################################################



acf(first_lag1,50,main="SNP ACF for Stationary series")# Code For Figure 9

pacf(first_lag1,50,main="SNP PACF for Stationary series")#  Code For Figure 10


acf(first_lag1_Nik,50,main="Nikkei ACF for Stationary series") # Code For Figure 11

pacf(first_lag1_Nik,50,main="Nikkei PACF for Stationary series")# Code For Figure 12

ccf(SNP,NIK, main="Cross Correlation between SNP and Nikkei") # Code For Figure 13  

lag2.plot(SNP,NIK,10)# Code For Figure 14



#####################################################################################
#             Granger causality test  Figure 15                                     #                                   #
#####################################################################################

grangertest(SNP ~ NIK, order = 1, data = mainData)
grangertest(SNP ~ NIK, order = 2, data = mainData)
grangertest(SNP ~ NIK, order = 3, data = mainData)

grangertest(NIK ~ SNP, order = 1, data = mainData)
grangertest(NIK ~ SNP, order = 2, data = mainData)
grangertest(NIK ~ SNP, order = 3, data = mainData) # Code for Figure 15  



#####################################################################################
#################     ARIMAX Modelling                       ########################
B = c(0,1)
SNP_1 = filter(SNP,B,sides=1)
NIK_1 = filter(NIK,B,sides=1)
B = c(0,0,1)
SNP_2 = filter(SNP,B,sides=1)
NIK_2 = filter(NIK,B,sides=1)
B = c(0,0,0,1)
SNP_3 = filter(SNP,B,sides=1)

NIK_3= filter(NIK,B,sides=1)

N = 960
SNP = SNP[4:N]
SNP_1 = SNP_1[4:N]
NIK_1 = NIK_1[4:N]
SNP_2 = SNP_2[4:N]
SNP_3 = SNP_3[4:N]
NIK_2 = NIK_2[4:N]
J=cbind(SNP_1)
L=cbind(SNP_2)
M=cbind(SNP_3)


####Model1 with order=(1,1,2)###### This is the main model######


######With regresor as J

Model3 = arima(NIK[4:N], xreg=J, order=c(1,1,2)) # Here xreg can be changed to "L" or "M" as well
Model3
coef(Model3)
AIC(Model3)
BIC(Model3)
Modelres3= residuals(Model3)
Modelres3
plot(Modelres3, main="Model Residual Plot")
coeftest(Model3)

pv<-forecast(Model3,h = 25,xreg=J[c(426:450),])
plot(pv, main="Forecast for ARIMAX model order(1,1,2)")

hrper= spec.pgram(Modelres3,taper=0,log="no")
spec.pgram(Modelres3,spans=c(20,20),taper=0,log="no")

abline(v=1/4.8, lty="dotted",col="red")
text(1/4.8,0.5, "~5")

plot(Modelres3,main = "Model residual Plot")
qqnorm(Modelres3, main = "Model residual QQplot")
acf(Modelres3, 30, main="ACF for ARIMAX model (1,1,2)")
Box.test (Modelres3, lag = 16)


# with Regressor M

Model1 = arima(NIK[4:N], xreg=M, order=c(1,1,2)) # Here xreg can be changed to "L" or "M" as well
Model1
coef(Model1) # This part givs the Coefficient of the best model
AIC(Model1)
BIC(Model1)
Modelres= residuals(Model1)
Modelres
plot(Modelres, main="Model Residual Plot") # Code for figure 16
qqnorm(Modelres, main = "Model residual QQplot") # Code for figure 17
coeftest(Model1)

hrper= spec.pgram(Modelres,taper=0,log="no")
spec.pgram(Modelres,spans=c(20,20),taper=0,log="no")

abline(v=1/4.8, lty="dotted",col="red")
text(1/4.8,0.5, "~5")

plot(Modelres,main = "Model residual Plot")

acf(Modelres, 30, main="ACF for ARIMAX model (1,1,2)")
Box.test (Modelres, lag = 16)
pv<-forecast(Model1,h = 25,xreg=M[c(426:450),])
plot(pv, main="Forecast for ARIMAX model order(1,1,2)")


######With regresor as L

Model2 = arima(NIK[4:N], xreg=L, order=c(1,1,2)) # Here xreg can be changed to "L" or "M" as well
Model2
coef(Model2)
AIC(Model2)
BIC(Model2)
Modelres2= residuals(Model2)
Modelres2
plot(Modelres2, main="Model Residual Plot")
coeftest(Model2)

hrper= spec.pgram(Modelres2,taper=0,log="no")
spec.pgram(Modelres2,spans=c(20,20),taper=0,log="no")

abline(v=1/4.8, lty="dotted",col="red")
text(1/4.8,0.5, "~5")

plot(Modelres2,main = "Model residual Plot")
qqnorm(Modelres2, main = "Model residual QQplot")
acf(Modelres2, 30, main="ACF for ARIMAX model (1,1,2)")
Box.test (Modelres2, lag = 16)
pv<-forecast(Model2,h = 25,xreg=L[c(426:450),])
plot(pv, main="Forecast for ARIMAX model order(1,1,2)")


####Model2 with order=(1,0,1)###### with regressor as L
Model4 = arima(NIK[4:N], xreg=L, order=c(1,0,1)) # Here xreg can be changed to J as well
Model4
coef(Model4)
AIC(Model4)
BIC(Model4)
Modelres4= residuals(Model4)
Modelres4
coeftest(Model4)

plot(Modelres4)
qqnorm(Modelres4)
acf(Modelres4, 30, main="output")
Box.test (Modelres4, lag = 5)


pv<-forecast(Model4,h = 25,xreg=L[c(426:450),])
plot(pv)

accuracy(Model4)

####Model2 with order=(1,0,1)###### with regressor as J
Model5 = arima(NIK[4:N], xreg=J, order=c(1,0,1)) # Here xreg can be changed to L as well
Model5
coef(Model5)
AIC(Model5)
BIC(Model5)
Modelres5= residuals(Model5)
Modelres5
coeftest(Model5)

plot(Modelres5)
qqnorm(Modelres5)
acf(Modelres5, 30, main="output")
Box.test (Modelres5, lag = 15)


pv<-forecast(Model5,h = 25,xreg=J[c(426:450),])
plot(pv)

accuracy(Model5)


####### Model with order (2,1,2) and with regressor as J
Model6 = arima(NIK[4:N], xreg=J, order=c(2,1,2))# Here xreg can be changed to J as well
Model6
coef(Model6)
AIC(Model6)
BIC(Model6)
Modelres6= residuals(Model6)
Modelres6

plot(Modelres6)
qqnorm(Modelres6)
acf(Modelres6, 30, main="output")
Box.test (Modelres6, lag = 16)

Fcast<-forecast(Model6,h = 25,xreg=J[c(426:450),]) # Forecasting the series
plot(Fcast)
accuracy(Model6)


####### Model with order (2,1,2) and with regressor as L
Model7 = arima(NIK[4:N], xreg=L, order=c(2,1,2))# Here xreg can be changed to J as well
Model7
coef(Model7)
AIC(Model7)
BIC(Model7)
Modelres7= residuals(Model7)

plot(Modelres7)
qqnorm(Modelres7)
acf(Modelres7, 30, main="output")
Box.test (Modelres7, lag = 5)

Fcast<-forecast(Model7,h = 25,xreg=L[c(426:450),]) # Forecasting the series
plot(Fcast)
accuracy(Model7)