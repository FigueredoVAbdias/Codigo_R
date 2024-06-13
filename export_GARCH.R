
#*************************  MODELO GARCH  ******************************
#
#*:::::::::::::: metodologia de Box-Jenkins
#
#*para estimar AR y MA se requiere estacionariedad
#
#Cargamos las librerias que usaremos

install.packages(c(
  "tseries", "lubridate", "tidyverse", "car", "astsa", "foreign",
  "timsac", "vars", "lmtest", "mFilter", "dynlm", "nlme", "broom",
  "kableExtra", "knitr", "MASS", "parallel", "mlogit", "dplyr",
  "tidyr", "forecast", "fpp2", "stats", "quantmod", "readxl", "TSA"
))

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
install.packages("fGarch") 
library(fGarch)
library(xts)
install.packages("rugarch")
library(rugarch)
install.packages("rmgarch")
library(rmgarch)
install.packages("FinTS")
library(FinTS)

#cargamos el database
library(readxl)
export_usa <- read_excel("ABDIAS/IMP3350_1992.xls")
View(export_usa)
export_usa

View(export_usa)

attach(export_usa)

names(export_usa)


#Convertir a objeto de Serie de Tiempo en R
export.usa_ts=ts(export_usa,
               start = c(1992),
               frequency = 12)

class(export.usa_ts)

start(export.usa_ts);end(export.usa_ts)

summary(export.usa_ts)

export.usa_ts



#*::::::::::::::1 PASO -> ANALIZAR LA ESTACIONARIEDAD :::::::::::::::::::::

plot(export.usa_ts, 
     ylab="Precio",
     col="blue",
     main="PORCENTAJE DE INTERNAUTAS EN BOLIVIA 
1995-2021")

seasonplot(export.usa_ts, 
           col=rainbow(12), 
           year.labels = TRUE)


#pruebas graficas
par(mfrow=c(2,1), 
    mar=c(4,4,4,1)+.1)
acf(export.usa_ts)
pacf(export.usa_ts)


#pruebas formales Pruebas de Raiz Unitaria
#H0: raiz unitarria por lo tanto la serie es no Estacionaria
#Dickey Fuller
adf.test(export.usa_ts, 
         alternative = "stationary")

ur.test=ur.df(export.usa_ts,
              type="drift",
              selectlags = "AIC")


#Dickey Fuller Aumentado
ur.test=ur.df(export.usa_ts,
              type="none",
              selectlags = "AIC")

summary(ur.test)



#*::::::::::::::2 PASO -> TRANSFORMACION DE LA SERIE :::::::::::::::::::::

#*si yt no es estacionario-->> diferenciamos

#Para conocer el número de diferencias que se requieren para lograr que la serie 
#sea estacionaria
ndiffs(export.usa_ts)


#1ra diferencia
export.usa_dif=diff(export.usa_ts)

summary(export.usa_dif)

export.usa_dif

plot(export.usa_dif)

par(mfrow=c(2,1), 
    mar=c(4,4,4,1)+.1)
acf(export.usa_dif)
pacf(export.usa_dif)
acf(ts(export.usa_dif, frequency=1))
pacf(ts(export.usa_dif, frequency=1))

ndiffs(export.usa_dif)

adf.test(export.usa_dif, 
         alternative = "stationary")

ur.test=ur.df(export.usa_dif,
              type="drift",
              selectlags = "AIC")

summary(ur.test)


#2da diferencia
usuarios_dif2=diff(export.usa_dif, 
                   differences = 2)

plot(usuarios_dif2)

par(mfrow=c(2,1), 
    mar=c(4,4,4,1)+.1)
acf(usuarios_dif2)
pacf(usuarios_dif2)

ndiffs(usuarios_dif2)

adf.test(usuarios_dif2, 
         alternative = "stationary")

ur.test=ur.df(usuarios_dif2,
              type="drift",
              selectlags = "AIC")
summary(ur.test)


#3da diferencia
fbkf_dif3=diff(fbkf_ts, 
               differences = 3)

plot(fbkf_dif3)

par(mfrow=c(2,1), 
    mar=c(4,4,4,1)+.1)
acf(fbkf_dif3)
pacf(fbkf_dif3)


ndiffs(fbkf_dif3)

adf.test(fbkf_dif3, 
         alternative = "stationary")

ur.test=ur.df(fbkf_dif3,
              type="drift",
              selectlags = "AIC")
summary(ur.test)


#Analisis grafico de la diferecia selecciona
par(mfrow=c(2,2), 
    mar=c(4,4,4,1)+.1)

plot(export.usa_ts, 
     ylab="Precio")

acf(export.usa_ts, 
    main="Serie No Estacionaria")

plot(export.usa_dif)

acf(export.usa_dif, 
    main="Serie Estacionaria")

plot(export.usa_dif, 
     type="o", 
     lty="dashed",
     main="Serie de Tiempo",
     col="red")



#*::::::::::::::3 PASO -> IDENTIFICAR EL MODELO :::::::::::::::::::::

#funcion de autorrelacion simple y parcial  
par(mfrow=c(2,1), 
    mar=c(4,4,4,1)+.1)
acf(export.usa_dif)
pacf(export.usa_dif)

acf(ts(export.usa_dif, frequency=1))

pacf(ts(export.usa_dif, frequency=1))
#*AC -> orden de los MA(2)
#*PAC-> orden de los AR(1)


#modelos ARIMA
modelo1=arima(export.usa_ts,
              order=c(1,1,1))

modelo2=arima(export.usa_ts,
              order=c(1,1,2))


summary(modelo1)

summary(modelo2)

#modelos GARCH

#calculo de los residuales al cuadrado
rescuad<-resid(modelo1)^2
rescuad
chartSeries(rescuad)
#regresion con los residuales al cuadrado rezagados en 1 periodo 
# HO: No hay efectos ARCH
export.usa.arch<-dynlm(rescuad~L(rescuad), data=export_usa)
summary(export.usa.arch)

acf.res2=acf(rescuad,
             main='ACF RESIDUALES AL CUADRADO',
             lag.max = 100,
             ylim=c(-0.5,1))

pacf.res2=pacf(rescuad,
             main='PACF RESIDUALES AL CUADRADO',
             lag.max = 100,
             ylim=c(-0.5,1))

#test ARCH
export.usa_arch_test<-ArchTest(export.usa_ts,
                               lags=1,
                               demean=TRUE)
export.usa_arch_test

#estimar el Modelo GARCH
uf_spec1=ugarchspec(mean.model = list(armaOrder=c(1,1)))
uf_spec1

ugfit<-ugarchfit(spec =uf_spec1,
                 data=export.usa_ts)
ugfit

#los coeficientes del modelo ARMA + GARCH
ugfit@fit$coef

#imprimir la varianza
ug_var=ugfit@fit$var
ug_var

#residuales
ug_res2=(ugfit@fit$residuals)^2
plot(ug_res2,
     type="l")
line(ug_var,
     col="green")

#pronostico del modelo ARMA + GARCH
ugfore=ugarchforecast(ugfit,
                      n.ahead = 10)
ugfore

par(mfrow=c(2,1), 
    mar=c(4,4,4,1)+.1)
plot(export.usa_ts)
plot(ugfit)


#*:::::::::::::: 4 PASO -> VALIDACION DEL MODELO :::::::::::::::::::::

#*mejor modelo es el mas parsimonioso 

#*test de portmanteau
#*H0: el proceso el ruido blanco

#Modelo1: ARIMA(1,1,1)
tsdiag(ugfit) #grafico de estandarizacion de los errores

#test de Ljung-box: HO -> ruido blanco 
Box.test(residuals(ugfit),
         lag=10,
         type="Ljung-Box")

error=residuals(ugfit)

#correlograma de los residuos
par(mfrow=c(2,1), 
    mar=c(4,4,4,1)+.1)
acf(error)
pacf(error)

#test de periodograma acumulativo de residuos
periodograma <- periodogram(error)
plot(periodograma, 
     main="Periodograma de Residuos")

summary(error) #*la media del error debe ser cero
plot(error)
plot(error, type = "l", ylab = "Residuos")
abline(h = mean(error), col = "red")

summary(ugfit)


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

#modelo_arima<-auto.arima(
#  fbkf_ts,
#  d=1,
#  D=1,
#  stepwise = FALSE,
#  approximation = FALSE,
#  trace = TRUE
#)

#fcst<-forecast(modelo1,
#               h=10,
#               level=c(95)
#)

#Modelo2: ARIMA(1,1,2)
pronostico2=forecast::forecast(modelo2,
                               h=10)
pronostico2
plot(time_index, pronostico2)
lines(time_index, preciopma.ts,
      col = "red")


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