
#*************************  MODELOS ARIMA  ******************************
#
#*:::::::::::::: metodologia de Box-Jenkins
#
#*para estimar AR y MA se requiere estacionariedad
#
#Cargamos las librerias que usaremos
library(tseries)
library(lubridate)
library(tidyverse)
library(car)
library(astsa)
library(foreign)
library(timsac)
library(vars)
library(lmtest)
library(mFilter)
library(dynlm)
library(nlme)
library(lmtest)
library(broom)
library(kableExtra)
library(knitr)
library(MASS)
library(parallel)
library(car)
library(mlogit)
library(dplyr)
library(tidyr)
library(forecast)
library(fpp2)
library(stats)
library(quantmod)
library(readxl)
library(TSA)
library(fGarch)
library(xts)
library(rugarch)
library(rmgarch)
library(FinTS)

#cargamos el database
usuarios <- read_excel("C:/Users/Abdias Figueredo V/OneDrive - Universidad Mayor de San Simón/Documentos/ECONOMETRIA AVANZADA/SERIES TEMPORALES/DB/ITNETUSERP2BOL.xls")

usuarios

View(usuarios)

attach(usuarios)

names(usuarios)


#Convertir a objeto de Serie de Tiempo en R
usuarios_ts=ts(usuarios,
           start = c(1995),
           frequency = 1)

class(usuarios_ts)

start(usuarios_ts);end(usuarios_ts)

summary(usuarios_ts)

usuarios_ts



#*::::::::::::::1 PASO -> ANALIZAR LA ESTACIONARIEDAD :::::::::::::::::::::

plot(usuarios_ts, 
     ylab="Porcentaje",
     col="blue",
     main="PORCENTAJE DE INTERNAUTAS")
tsplot(usuarios_ts,
       ylab="Porcentaje",
       col="blue",
       main="PORCENTAJE DE INTERNAUTAS")

seasonplot(usuarios_ts, 
           col=rainbow(12), 
           year.labels = TRUE)

descom<- decompose(usuarios_ts)
#pruebas graficas: correlogramas de la serie en niveles
par(mfrow=c(2,1), 
    mar=c(4,4,4,1)+.1)
acf(usuarios_ts)
pacf(usuarios_ts)

acf2(usuarios_ts,
     max.lag = 10)


#pruebas formales Pruebas de Raiz Unitaria
#H0: raiz unitarria por lo tanto la serie es no Estacionaria
#Dickey Fuller
adf.test(usuarios_ts, 
         alternative = "stationary")

ur.test=ur.df(usuarios_ts,
              type="drift",
              selectlags = "AIC")


#Dickey Fuller Aumentado
ur.test=ur.df(usuarios_ts,
              type="none",
              selectlags = "AIC")

summary(ur.test)



#*::::::::::::::2 PASO -> TRANSFORMACION DE LA SERIE :::::::::::::::::::::

#*si yt no es estacionario-->> diferenciamos

#Para conocer el número de diferencias que se requieren para lograr que la serie 
#sea estacionaria
ndiffs(usuarios_ts)


#1ra diferencia
usuarios_dif=diff(usuarios_ts)

summary(usuarios_dif)

usuarios_dif

tsplot(usuarios_dif,
     ylab="Porcentaje",
     col="red",
     main="PORCENTAJE DE INTERNAUTAS 1RA DIFERENCIA")

par(mfrow=c(2,1), 
    mar=c(4,4,4,1)+.1)
acf(usuarios_dif)
pacf(usuarios_dif)

acf2(usuarios_dif,
     max.lag = 10)

ndiffs(usuarios_dif)

adf.test(usuarios_dif, 
         alternative = "stationary")

ur.test=ur.df(usuarios_dif,
              type="drift",
              selectlags = "AIC")

summary(ur.test)


#2da diferencia
usuarios_dif2=diff(usuarios_ts, 
               differences = 2)

plot(usuarios_dif2,
     ylab="Porcentaje",
     col="red",
     main="PORCENTAJE DE INTERNAUTAS 2da DIFERENCIA")

par(mfrow=c(2,1), 
    mar=c(4,4,4,1)+.1)
acf(usuarios_dif2)
pacf(usuarios_dif2)

acf2(usuarios_dif2,
     max.lag = 10)

ndiffs(usuarios_dif2)

adf.test(usuarios_dif2, 
         alternative = "stationary")

ur.test=ur.df(usuarios_dif2,
              type="drift",
              selectlags = "AIC")
summary(ur.test)


#Analisis grafico de la diferecia selecciona
par(mfrow=c(2,2), 
    mar=c(4,4,4,1)+.1)

plot(usuarios_ts, 
     ylab="Precio")

acf(usuarios_ts, 
    main="Serie No Estacionaria")

plot(usuarios_dif1)

acf(usuarios_dif, 
    main="Serie Estacionaria")

plot(usuarios_dif, 
     type="o", 
     lty="dashed",
     main="Serie de Tiempo",
     col="red")



#*::::::::::::::3 PASO -> IDENTIFICAR EL MODELO :::::::::::::::::::::

#funcion de autorrelacion simple y parcial  
par(mfrow=c(2,1), 
    mar=c(4,4,4,1)+.1)
acf(usuarios_dif)
pacf(usuarios_dif)

acf(ts(usuarios_dif, frequency=1))
pacf(ts(usuarios_dif, frequency=1))


acf2(usuarios_dif,
     max.lag = 10)
#*AC -> orden de los MA(2)
#*PAC-> orden de los AR(1)


#modelos ARIMA
modelo1<-arima(usuarios_ts,
              order=c(1,1,1))

modelo2<-arima(usuarios_ts,
              order=c(1,2,1))

modelo3<-arima(usuarios_ts,
              order=c(1,1,0))

summary(modelo1)

summary(modelo2)

summary(modelo3)

modelo.auto<-auto.arima(usuarios_ts,
                        d=1,
                        D=1,stepwise = FALSE,
                        approximation = FALSE,
                        trace = TRUE)

summary(modelo.auto)

#modelos SARIMA
modelo1.1<-sarima(usuarios_ts,1,1,1)

modelo2.1<-sarima(usuarios_ts,1,2,1)

modelo3.1<-sarima(usuarios_ts,1,1,0)

modelo1.1$ttable

modelo2.1$ttable

modelo3.1$ttable




#*:::::::::::::: 4 PASO -> VALIDACION DEL MODELO :::::::::::::::::::::

#*mejor modelo es el mas parsimonioso 

#*test de portmanteau
#*H0: el proceso el ruido blanco

#Modelo1: ARIMA(1,1,1)
tsdiag(modelo1) #grafico de estandarizacion de los errores

#test de Ljung-box: HO -> ruido blanco 
Box.test(residuals(modelo1),
         lag=10,
         type="Ljung-Box")

error=residuals(modelo1)

#correlograma de los residuos
par(mfrow=c(2,1), 
    mar=c(4,4,4,1)+.1)
acf(error)
pacf(error)
acf2(error,
     max.lag=10)

#test de periodograma acumulativo de residuos
periodograma <- periodogram(error)
plot(periodograma, 
     main="Periodograma de Residuos")

summary(error) #*la media del error debe ser cero
plot(error)
plot(error, type = "l", ylab = "Residuos")
abline(h = mean(error), col = "red")

summary(modelo1)


#Modelo2: ARIMA(1,2,1)
tsdiag(modelo2) #grafico de estandarizacion de los errores

#test de Ljung-box: HO -> ruido blanco 
Box.test(residuals(modelo2),
         lag=10,
         type="Ljung-Box")

error2=residuals(modelo2)

#test de periodograma acumulativo de residuos
periodograma <- periodogram(error2)
plot(periodograma, 
     main="Periodograma de Residuos")

summary(error2) #*la media del error debe ser cero
plot(error2)
plot(error2, type = "l", ylab = "Residuos")
abline(h = mean(error2), col = "red")

summary(modelo2)


#Modelo3: ARIMA(1,1,0)
tsdiag(modelo3) #grafico de estandarizacion de los errores

#test de Ljung-box: HO -> ruido blanco 
Box.test(residuals(modelo3),
         lag=10,
         type="Ljung-Box")

error3=residuals(modelo3)

#test de periodograma acumulativo de residuos
periodograma <- periodogram(error3)
plot(periodograma, 
     main="Periodograma de Residuos")

summary(error3) #*la media del error debe ser cero
plot(error3)
plot(error3, type = "l", ylab = "Residuos")
abline(h = mean(error3), col = "red")

summary(modelo3)


#*:::::::::::::: 4 PASO -> PREDICCION :::::::::::::::::::::

#Modelo1: ARIMA(1,1,1)
pronostico1<-forecast::forecast(modelo1,
                               h=10)
pronostico1
plot(pronostico1)

pronostico1<-forecast(modelo1,
               h=10,
               level=c(95))

autoplot(pronostico1)+
  ggtitle("PRONOSTICO PARA LOS PROXIMOS 10 MESES")+
  ylab("Expresado en Bs")

print(summary(pronostico1))

pronostico<-as.data.frame(pronostico1)


#Modelo2: ARIMA(1,2,1)
pronostico2=forecast::forecast(modelo2,
                               h=10)
pronostico2
plot(pronostico2)

pronostico1<-forecast(modelo2,
                      h=10,
                      level=c(95))

autoplot(pronostico2)+
  ggtitle("PRONOSTICO PARA LOS PROXIMOS 10 MESES")+
  ylab("Expresado en Bs")

print(summary(pronostico2))

pronostico<-as.data.frame(pronostico2)


#Modelo3: ARIMA(1,1,0)
pronostico3=forecast::forecast(modelo3,
                               h=10)
pronostico3
plot(pronostico3)

pronostico1<-forecast(modelo3,
                      h=10,
                      level=c(95))

autoplot(pronostico3)+
  ggtitle("PRONOSTICO PARA LOS PROXIMOS 10 MESES")+
  ylab("Expresado en Bs")

print(summary(pronostico3))

pronostico<-as.data.frame(pronostico3)


#Grafico de predicciones
time_index <- seq(from = as.Date("2021-01-01"), 
                  by = "month", 
                  length.out = length(pronostico1))

plot(time_index, pronostico1, 
     type = "l", 
     col = "blue",
     ylab = "Valor",
     xlab = "Tiempo")
lines(time_index, preciopma.ts,
      col = "red")
legend("topright", 
       legend = c("pronostico1", "preciopma.ts"), 
       col = c("blue", "red"),
       lty = 1)