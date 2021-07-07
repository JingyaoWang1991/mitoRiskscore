##library(foreign)
rm(list=ls())
library(survival)
library(rms)
setwd('.08_nomogram')
mydata <- read.table('indepInput.txt',header = T)
mydata <- as.data.frame(mydata)
head(mydata)

#f<-coxph(Surv(futime,fustat==1)~age+gender+stage+riskScore,data=mydata)
#f
#summary(f)

dd<-datadist(mydata)
options(datadist='dd')

coxm1 <- cph(Surv(futime,fustat==1)~age+stage+riskScore,x=T,y=T,data=mydata,surv=T)
coxm1
summary(coxm1)

surv <- Survival(coxm1)
surv1 <- function(x)surv(1*3,lp=x)
surv2 <- function(x)surv(1*5,lp=x)
surv3 <- function(x)surv(1*10,lp=x)

nom1<-nomogram(coxm1,fun=list(surv1,surv2,surv3),lp = F,
               funlabel=c('3-year Survival probability',
                          '5-year survival probability',
                          '10-year survival probability'),
               maxscale=100,
               fun.at=c('0.9','0.8','0.7','0.5','0.3','0.1')
               
               )
plot(nom1,xfrac=.45)

##C-index calculation 
library(survival)
f<-coxph(Surv(futime,fustat==1)~age+stage+riskScore,data=mydata) # coxph函数拟合cox回归模型
summary(f)
sum.surv<-summary(f)
c_index<-sum.surv$concordance
c_index

##calibration curve
cal <- calibrate(coxm1, cmethod='KM', method='boot', u=2.5, m=100, B=100)
plot(cal,lwd=2,lty=1,errbar.col=c(rgb(0,118,192,maxColorValue=255)),xlim=c(0,1),ylim=c(0,1),xlab="Nomogram-Predicted Probability of 5-year OS",ylab="Actual 5-year OS (proportion)",col=c(rgb(192,98,83,maxColorValue=255)))
lines(cal[,c("mean.predicted","KM")],type="b",lwd=2,col=c(rgb(192,98,83,maxColorValue=255)),pch=16)
abline(0,1,lty=3,lwd=2,col=c(rgb(0,118,192,maxColorValue=255)))

