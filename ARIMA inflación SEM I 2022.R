

# Modelo ARIMA inflaci?n para Bolivia
# Este dise?o  marzo, 2022


######################################################################

# 1. Importamos datos y configuamos variables de series de tiempo


######################
library(readxl)
datos <- read_excel("C:/Users/Abdias Figueredo V/OneDrive - Universidad Mayor de San Simón/Documentos/ECONOMETRIA AVANZADA/SERIES TEMPORALES/Modelos Cap. ARIMA (Simul + Inflación)-20240602T034912Z-001/Modelos Cap. ARIMA (Simul + Inflación)/inflation_bol.xlsx")
View(datos)
attach(datos)
names(datos)

#################################3

# 1.2. Configurando una serie mensual


install.packages("tseries")
library(tseries)

#configuramos las variables en series de tiempo
inf <- ts(infl, 
          start=c(2015, 01), 
          end=c(2022,02), 
          freq=12)

#Estad?sticos descriptivos
install.packages("psych")
library(psych)

describe(inf)
#test de distribucion normal: si p > 0.1 no se rechaza la HO de distribution normal  
jarque.bera.test(inf)

#Coeficiente de variabilidad 
sd(inf)/mean(inf)*100

# Percentiles, cuantiles y umbrales
quantile(inf, 1:9/10)
quantile(inf, c(0.25, 0.50, 0.75))

length(inf[inf < 5]) / length(inf) * 100 #porcentaje de la inflacion >5%
length(inf[inf > 5]) / length(inf) * 100#porcentaje de la inflacion <5%


# Graficando la variable

# http://www.sthda.com/english/articles/32-r-graphics-essentials/128-plot-time-series-data-using-ggplot/

install.packages("ggplot2")
install.packages("colorspace")
library(colorspace)
library(ggplot2)

# Graficando una serie individual
t <- seq(as.Date('2015-01-01'),
         as.Date('2022-02-28'),
         by = "month")
t
length(t)
muestra<-as.data.frame(inf)

a<-ggplot(data = muestra, 
          aes(x = t, 
              y = inf))+
  geom_line(color = "#00AFBB", 
            size = 2) +   
  stat_smooth( color = "#FC4E07", 
               fill = "#FC4E07",
               method = "loess", 
               show.legend = TRUE)+ 
  ggtitle("Tasa de inflacion en Bolivia,\ periodo 2015-2022")+
  xlab("tiempo") + 
  ylab("En porcentaje, %")
a


a + theme(
  plot.title = element_text(color="blue",
                            size=14, face="bold"),
  axis.title.x = element_text(color="blue", 
                              size=14, face="bold"),
  axis.title.y = element_text(color="blue", 
                              size=14, 
                              face="bold")
  )

#################################################################3

###### 

#2 Desestacionalizacion de series (si corresponde)

# install and load seasonal package 
install.packages("seasonal")
library(seasonal)


# seasonal adjust time series using X11. 
inf_sa <- seas(inf,
               x11 = "")
inf_sa


inf_sa$data
#descomposicion de la serie de tiempo
inf_componen<-inf_sa$data
plot(inf_componen)
inf_componen2<-as.data.frame(inf_componen)
inf_componen2
inf_ajus<-ts(inf_componen2$seasonaladj, 
             start=c(2015, 1), 
             freq=12)
plot(inf_ajus)

install.packages("seasonalview")
library(seasonalview)
view(inf_sa)
plot(inf)

# plot original and seasonally adjusted series.
plot(inf, main="Tasa de inflacion, 2015-2022",
     lwd=2, 
     lty=1)
lines(final(inf_sa),
      col=2, 
      lwd=2, 
      lty=2)

plot(inf, main="Inflacion obserada y desestacionalizada",
     sub="Estimacion por ARIMA CENSUS X11",
     lwd=3, 
     ylim=c(-1.5,6),
     xlim=c(2015,2022), 
     ylab="En porcentaje", 
     xlab="Per?odo", lty=1)
lines(inf_ajus,  
      col="red", 
      lwd=3, 
      lty=2)
legend("topright", c("Serie original observada",
                     "serie desestacionalizada"), lwd=3, lty=1:2 )
abline(h=0, lty=2)
abline(h=2, lty=2)
abline(h=4, lty=2)
abline(h=6, lty=2)


######################################################33

# 3. Aplicando la metodologia BOX-JENKINS

# 3.1 Identificacion

# Is the series stationary?
# Plot autocorrelation and partial autocorrelation
par(mfrow=c(2,1))
acf(inf); pacf(inf)


#En primera diferencia
acf(diff(inf)); pacf(diff(inf))
par(mfrow=c(1,1))


# Posibles modelos (1 1 0)(0 0 0), variables de impulso?

install.packages("forecast")
library(forecast)


inf_f<-window(inf, 
              end=c(2021,08))  # Seis meses fuera de muestra

arima1<-Arima(inf_f,
              order=c(1,1,0),
              seasonal = c(0,0,0), 
              xreg = NULL, 
              include.mean = TRUE)
arima1


library(tseries)

jarque.bera.test(arima1$residuals)
shapiro.test(arima1$residuals)
Box.test(arima1$residuals, type="Ljung-Box")
accuracy(arima1)
plot(arima1$residuals)


# Probability is greater than 0.05. Note that the null hypothesis is: residuals are normal. We cannot reject the null.
# OK

# Identificando Outlier
myboxplot1 <- boxplot(arima1$residuals)
names(myboxplot1)
myboxplot1$out  # dos valores at?picos
3*sd(arima1$residuals)#las max y min no deben superar la ds para que los residuos tengan distribucion normal
max(arima1$residuals) 
min(arima1$residuals)
plot(arima1$residuals)
arima1$residuals  


# Creamos variables de impulso en el archivo excel
d19_12<- ts(d19_12, start=c(2015, 01), freq=12)
d20_12<- ts(d20_12, start=c(2015, 01), freq=12) 

exogen<-ts.intersect(d19_12, d20_12)
plot(exogen)

exogen1<-window(exogen, start=c(2015,01), end=c(2021, 08))
exogen2<-window(exogen, start=c(2021,09), end=c(2022,02))
exogen2

##### Modelo 2, corrigiendo problemas de normalidad #####

arima2<-Arima(inf_f, 
              order=c(1,1,0), 
              seasonal = c(0,0,0), 
              xreg = exogen1,
              include.mean = TRUE)
arima2
summary(arima2)
jarque.bera.test(arima2$residuals)
shapiro.test(arima2$residuals)
Box.test(arima2$residuals, type="Ljung-Box")
plot(arima2$residuals)
arima2$residuals


# Fanchart o grafico de abanico

# Pronosticos fuera de muestra


pronosticos<-forecast(arima2,
                      h=6,
                      xreg = exogen2,
                      level = c(10, 25,50, 90))
pronosticos


inf_obs<-window(inf, start=c(2019,1), end=c(2022,2))
inf_obs

plot(forecast(arima2, h=6, 
              xreg=exogen2,
              level = c(10, 25,50,75,90)), 
     xlim= c(2019, 2022), ylim= c(-3,4), 
     main= "Pron?stico de la inflaci?n, seis meses fuera de muestra",
     col = 1)
lines(inf_obs,  col="red", lwd=3, lty=2)
abline(h = -2, lty=2)  #cpi target
abline(h = 0, lty=2)  #cpi target
abline(h = 2, lty=2)  #cpi target
abline(h = 4, lty=2)  #cpi target
abline(h = 6, lty=2)  #cpi target
abline(h = 8, lty=2)  #cpi target
axis(2, at = -2:4, las = 2, tcl = 0.5, labels = FALSE)
axis(4, at = -2:4, las = 2, tcl = 0.5)
axis(1, at = seq(2019, 2022, 0.25), labels = FALSE, tcl = 0.2)
abline(v = 2021 + 0.6, lty = 2)  #2 year line
legend("bottomleft", c("Pron?stico",
                     "Serie observada"), lwd=3, lty=1:2 )


# Estimaci?n con muestra completa

inf_comp<-window(inf, end=c(2022,02))  #  Con muestra completa

exogen<-ts.intersect(d19_12, d20_12)
plot(exogen)

exogen1f<-window(exogen, start=c(2015,01), end=c(2022, 02))
exogen2f<-window(exogen, start=c(2022,03), end=c(2023,06))
exogen2f


arima3<-Arima(inf_comp, order=c(1,1,0), seasonal = c(0,0,0), 
              xreg = exogen1f, include.mean = TRUE)
arima3


jarque.bera.test(arima3$residuals)
shapiro.test(arima3$residuals)
Box.test(arima3$residuals, type="Ljung-Box")
plot(arima3$residuals)


plot(forecast(arima3, h=16, 
              xreg=exogen2f,
              level = c(10, 25,50,90)), 
     xlim= c(2019, 2023.5), ylim= c(-4,6), 
     main= "Pron?stico de la inflaci?n, a Jun/2023",
     col = 1)
abline(h = -2, lty=2)  #cpi target
abline(h = 0, lty=2)  #cpi target
abline(h = 2, lty=2)  #cpi target
abline(h = 4, lty=2)  #cpi target
abline(h = 6, lty=2)  #cpi target
axis(2, at = -4:6, las = 2, tcl = 0.5, labels = FALSE)
axis(4, at = -2:4, las = 2, tcl = 0.5)
axis(1, at = seq(2019, 2023, 0.25), labels = FALSE, tcl = 0.2)
abline(v = 2022 + 0.1, lty = 2)  #2 year line


forecast(arima3, h=16, xreg=exogen2f, level = c(10,25,50,75,90))%>%
  autoplot(main="Pron?stico de la inflaci?n en Bolivia (Jun/2023)")+
  ylab("En porcentaje") + xlab("A?o")+
  xlim(2020, 2023)+ ylim(-4, 6)








