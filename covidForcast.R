# Load packages

library(forecast)
library(caret)
library(WaveletArima)

#----------------------ARIMA analysis----------------------------------######
dt1<-francovid19[2]
dt1$fran_cases=as.numeric(dt1$fran_cases)
typeof(dt1$fran_cases)
mymodel=auto.arima(dt1,trace = TRUE)


library(fUnitRoots)
df=ts(francovid19)
components.ts = decompose(df)
plot(components.ts)


urkpssTest(serie2, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(serie2, differences=1)
plot(tsstationary)


timeseriesseasonallyadjusted <- serie- timeseriescomponents$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)

acf(tsstationary, lag.max=34)
pacf(tsstationary, lag.max=34)

#-----------------------------ARIMA for france---------------------------------------------------#
serie=ts(dt1)
mymodel=auto.arima(serie,trace = TRUE)
plot(mymodel)
#### 10-step ahead forecast from the fitted ARIMA model
fit=forecast(mymodel,h=10)
#accuracy
RMSE(fit$fitted,serie)
MAE(fit$fitted,serie)

#####plot actual and predict
par(mfrow = c(1,1))
plot(serie) # plot original sim
lines(fitted(fit), col = "red") # plot fitted values
legend("topleft", legend = c("original","fitted"), col = c("black","red"),lty = 1)

res=fit$residuals
plot(res,col=4,size=1)
#### Fit a WBF model to the residuals obtained from the previous step 
#### Simultaneously 10-ste-ahead forecast using the fitted WBF model
WaveletForecast<-WaveletFittingarma(res,Waveletlevels=floor(log(length(res))),boundary='periodic',FastFlag=TRUE,MaxARParam=5,MaxMAParam=5,NForecast=10)
plot(WaveletForecast$Finalforecast)

#### Add the fitted ARIMA outouts to the fitted WBF model outputs
hybrid_fit=WaveletForecast$FinalPrediction+mymodel$fitted
hybri=WaveletForecast$Finalforecast+fit$mean

#####plot actual and predict
par(mfrow = c(1,1))
plot(serie) # plot original sim
lines(hybrid_fit, col = "red") # plot fitted values
legend("topleft", legend = c("original","fitted"), col = c("black","red"),lty = 1)


counts <- data.frame(Arima=fit$mean,hybrid=hybri)
rownames(counts)<- c("7 june","8 june","9 june","10 june","11 june","12 june","13 june","14 june","15 june","16 june")
counts <- as.matrix(counts)
counts1 <- t(counts)

barplot(counts1,beside=TRUE,
        col=c("green","black"))

legend("topleft", legend =colnames(counts),col=c("green","black"),lty = 1)

## check the accuracy
RMSE(hybrid_fit, serie)
MAE(hybrid_fit, serie)
      

saveRDS(counts, file = "counts.RDS")

#------------------arima for cameroon-------------------#

serie2=ts(camercov)
mymodel2=auto.arima(serie2,trace = TRUE)
plot(mymodel)
#### 10-step ahead forecast from the fitted ARIMA model
fit2=forecast(mymodel2,h=10)
plot(fit2)
## accuracy
RMSE(fit2$fitted,serie2)
MAE(fit2$fitted,serie2)

#####plot actual and predict
par(mfrow = c(1,1))
plot(serie2) # plot original sim
lines(fitted(fit2), col = "red") # plot fitted values
legend("topleft", legend = c("original","fitted"), col = c("black","red"),lty = 1)



#### Assign residuals to a variable
res2=fit2$residuals
plot(res2,col=4,size=1)
#### Fit a WBF model to the residuals obtained from the previous step 
#### Simultaneously 10-ste-ahead forecast using the fitted WBF model
WaveletForecast2<-WaveletFittingarma(res2,Waveletlevels=floor(log(length(res2))),boundary='periodic',FastFlag=TRUE,MaxARParam=5,MaxMAParam=5,NForecast=10)
plot(WaveletForecast$FinalPrediction)

#### Add the fitted ARIMA outouts to the fitted WBF model outputs
hybrid_fit2=WaveletForecast2$FinalPrediction+mymodel2$fitted
plot(hybrid_fit2)

par(mfrow = c(1,1))
plot(serie2) # plot original sim
lines(hybrid_fit2, col = "red") # plot fitted values
legend("topleft", legend = c("original","fitted"), col = c("black","red"),lty = 1)

hybri2=WaveletForecast2$Finalforecast+fit2$mean
## plot the hybrid and arima forecast

counts2 <- data.frame(Arima=fit2$mean, hybrid=hybri2)
rownames(counts2)<- c("4 May","5 May","6 May","7 May","8 May","9 May","10 May","11 May","12 May","13 May")
counts2 <- as.matrix(counts2)
counts_t <- t(counts2)

barplot(counts_t,beside=TRUE,
        col=c("green","black"))

legend("topleft", legend =colnames(counts2),col=c("green","black"),lty = 1)

## check the accuracy
RMSE(hybrid_fit2, serie2)
MAE(hybrid_fit2, serie2)






