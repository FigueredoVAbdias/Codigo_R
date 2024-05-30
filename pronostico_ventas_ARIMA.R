######################### ESTIMACION DE VENTAS #################################

# LIMPIEZA DE R
rm(list=ls()) 
options(scipen=999)

# LIBRERIAS
library(fpp2)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)

# CARGA DE LOS DATOS
data<- read_excel("C:/Users/Abdias Figueredo V/OneDrive - Universidad Mayor de San Simón/Documentos/YPFB/Consultas en excel/VENTA.xlsx")
data
nrow(data)
dim(data)

#USAR VARIABLES POR SUS NOMBRES
attach(data)

# CAMBIAR TIPO A DATE
data$fecha<- date(data$fecha)

#DATAFRAME FILTRADO POR VENTAS CONFIRMADAS
ventas_confirmadas<-filter(data, 
                           estado=="confirmado")
dim(ventas_confirmadas)

#DATAFRAME FILTRADO POR VENTAS DIARIAS
ventas_diarias<-select(data,
                       total, fecha)
colnames(ventas_diarias)[1]<-"ventas"
ventas_diarias

#DATAFRAME FILTRADO POR VENTAS MENSUALES
ventas_mensuales<-ventas_diarias%>%
  mutate(
    month=format(fecha,"%m"), 
    year= format(fecha, "%Y")
  )%>%
  group_by(month, year)%>%
  summarise(ventas=sum(ventas))

ventas_mensuales<-ventas_mensuales[
  with(ventas_mensuales,
       order(ventas_mensuales$year)),
  ]

ventas_mensuales

#DECLARACION DE LOS DATOS COMO SERIE TEMPORAL
Y<-ts(
      ventas_mensuales[,3],
      start=c(2022,1), 
      end=c(2023,11),
      frequency = 12
      )
Y

#EXPLORACION DE LOS DATOS
autoplot(Y)+
  ggtitle("GRAFICO DE VENTAS UGLP")+
  ylab("expresado en bolivianos")

#OBSERVAR LA ESTACIONALIDAD DE LA SERIE
descom=decompose(Y)
autoplot(descom)
acf(Y)
pacf(Y)

#REVISAR LAS DIFERENCIAS EN LAS SERIES
DY<-diff(Y)

#OBSERVAR LAS DIFERENCIAS
autoplot(DY)+
  ggtitle("CAMBIOS ANUALES EN VENTAS UGLP") + 
  ylab("Expresado en Bs")

#ELABORACION MODELO ARIMA
modelo_arima<-auto.arima(
  Y,
  d=1,
  D=1,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)

print(modelo_arima)

#REALIZAMOS UNA REVISION DE LOS RESIDUOS DEL MODELO
checkresiduals(modelo_arima)
                         
#CALCULAMOS EL FORECAST DE VENTAS PARA LOS PROXIMOS 6 MESES
fcst<-forecast(modelo_arima,
               h=13,
               level=c(95)
               )
autoplot(fcst)+
  ggtitle("PRONOSTICODE VENTAS PARA LOS PROXIMOS 6 MESES")+
  ylab("Expresado en Bs")

print(summary(fcst))

pronostico<-as.data.frame(fcst)
