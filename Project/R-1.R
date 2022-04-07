
library(readxl)
library(car)
library(carData)
library(ridge)

#Northern Hemisphere
data1<-read_excel("sunrisedata.xlsx", sheet=1)
plot(data1$la, log((data1$adjsun)-340))

fit5<-lm(log(data1$adjsun)~data1$la+data1$altitude+data1$Humidity+data1$meantemp,data=data1)
fit5
summary(fit5)
vif(fit5)

#Ridge Regreesion#
mod<-linearRidge(log(data1$adjsun)~data1$la+data1$altitude+data1$Humidity
                 +data1$meantemp,data=data1)
mod
summary(mod)
vif(mod)

#Regression with the reduced model#
mod1<-linearRidge(log(data1$adjsun)~data1$la
                 +data1$meantemp,data=data1)
mod1
summary(mod1)


#Southern Hemisphere
data2<-read_excel("sunrisedata.xlsx", sheet=2)
plot(data2$la, log(data2$adjsun))

fit7<-lm(log(data2$adjsun)~data2$meantemp+data2$la+data2$Humidity+data2$altitude,data=data2)
summary(fit7)
vif(fit7)

#The reduced model
fit8<-lm(log(data2$adjsun)~data2$la+data2$Humidity,data=data2)
summary(fit8)
vif(fit8)

