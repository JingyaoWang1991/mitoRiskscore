rm(list=ls())
setwd('10_ex-validition/GSE72094') 
library(timeROC)
library(survival)

bioROC=function(inputFile=null,rocFile=null){
  
  rt=read.table(inputFile,header=T,sep="\t")
  #rt$futime=rt$futime/365
  ROC_rt=timeROC(T=rt$futime,delta=rt$fustat,
                 marker=rt$riskscore,cause=1,
                 weighting='aalen',
                 times=c(1,3,5),ROC=TRUE)
  pdf(file=rocFile,width=5,height=5)
  plot(ROC_rt,time=5,col='#FFFF33',title=FALSE,lwd=2)
  plot(ROC_rt,time=3,col='#00CCCC',add=TRUE,title=FALSE,lwd=2)
  plot(ROC_rt,time=1,col='#FF6666',add=TRUE,title=FALSE,lwd=2)
  legend('bottomright',
         c(paste0('AUC at 1 years: ',sprintf("%.03f",ROC_rt$AUC[1])),
           paste0('AUC at 3 years: ',sprintf("%.03f",ROC_rt$AUC[2])),
           paste0('AUC at 5 years: ',sprintf("%.03f",ROC_rt$AUC[3]))),
         col=c("#FF6666",'#00CCCC','#FFFF33'),lwd=2,bty = 'n')
  dev.off()
}

bioROC(inputFile="GSE72094-risksurv.txt",rocFile="timeROC.pdf")
