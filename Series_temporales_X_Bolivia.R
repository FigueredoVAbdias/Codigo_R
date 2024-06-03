################ SERIES TEMPORALES DATOS MACRO BOLIVIA ###################

# IMPORTAMOS DATOS MACRO DE LA FRED
fredkey <- "dcd2a47e8017386f6f5fcd4259eb0390"

library(quantmod)
library(readxl)
library(xts)
library(ggplot2)
library(fpp2)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)

choose.files()
getSymbols("IMP3350", 
           src = "FRED")

IMP3350<-read_xls("C:/Users/Abdias Figueredo V/Downloads/IMP3350.xls")

# RESUMEN DEL DATAFRAME
summary(IMP3350)

# RENOMBRAR EL DATAFRAME
IMP3350$exports_USA <- IMP3350$IMP3350

#CONVERTIR LAS FECHAS DE CADENA A FECHAS NUMERICAS MES
IMP3350$montly <- as.Date(IMP3350$observation_date,                    , 
                                  format = "%Y-%m")


IMP3350$montly <- format(IMP3350$observation_date, "%Y-%m")

IMP3350$IMP3350 <- NULL

#Especificar la estructura de la serie de tiempo
st_IMP3350 <- xts(IMP3350, 
                  order.by = IMP3350$observation_date)

ts_IMP3350<-ts(
  IMP3350[,2],
  start=c(1992,1), 
  end=c(2024,3),
  frequency = 12
)

ts_IMP3350

#Filtrar datos para un período específico:
filtrado_st_IMP3350 <- ts_IMP3350["1992-01/1992-06"]

rango_anios <- window(ts_IMP3350, 
                      start = c(1992,1), 
                      end = c(1992, 13)
                      )


#Graficar las series de tiempo
ggplot(data = rango_anios, 
       aes(x = index(rango_anios))) +
  geom_line(aes(y = exports_USA), color = "blue") +
  labs(title = "Exportaciones mensuales de mercancías a EEUU",
       subtitle = "1992m1-2024m2",
       y = "Millones de $us",
       x = "periodo",
       caption = "Fuente: FRED, U.S. Census Bureau; U.S. Bureau of Economic Analysis")

ggplot(data = rango_anios, 
       aes(x = index(rango_anios))) +
  geom_line(aes(y = exports_USA), color = "red") +
  labs(title = "Exportaciones mensuales de mercancías a EEUU (primera diferencia)",
       subtitle = "1992m1-2024m2",
       y = "",
       x = "",
       caption = "Fuente: U.S. Census Bureau; U.S. Bureau of Economic Analysis")

#EXPLORACION DE LOS DATOS
autoplot(rango_anios)

#OBSERVAR LA ESTACIONALIDAD DE LA SERIE
descom=decompose(ts_IMP3350)
autoplot(descom)
#nivel de residuos
acf(ts_IMP3350) 
#nivel de residuos
pacf(ts_IMP3350)

#REVISAR LAS DIFERENCIAS EN LAS SERIES
DY<-diff(ts_IMP3350)

#OBSERVAR LAS DIFERENCIAS
autoplot(DY)+
  ggtitle("CAMBIOS ANUALES EN VENTAS UGLP") + 
  ylab("Expresado en Bs")

