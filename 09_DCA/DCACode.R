rm(list=ls())
library(survival)

  setwd('.09_DCA')
  #Source file to use dca command
  source("dca.R")
  data.set = read.delim("indepInput.txt", header=TRUE, sep="\t")
  attach(data.set)

###################################################
## Decision Curve Analysis for Survival Outcomes ##
###################################################

# Basic Data Set-up
  #Source file to use stdca command
  source("stdca.R")
  #Creates a survival object with time to event variable as ttcancer and the event is 
  #cancer. 
  Srv = Surv(data.set$futime, data.set$fustat)


  #Load survival library
  #Run the cox model
  coxmod = coxph(Srv ~ stage+riskScore, data=data.set)
  #the probability of failure is calculated by subtracting the probability of 
  #survival from 1. 
  data.set$pr_failure18 = c(1- (summary(survfit(coxmod,
  newdata=data.set), times=5)$surv))
  #Run the decision curve analysis (with a smoother)
  stdca(data=data.set, outcome="fustat", ttoutcome="futime", timepoint=5, 
        predictors="pr_failure18", #probability=T,
        xstop=0.6, smooth=F)
  km1 = stdca(data=data.set, outcome="fustat", ttoutcome="futime", timepoint=5, 
             predictors="pr_failure18", xstop=0.9, smooth=F)


  #Run the cox model
  coxmod2 = coxph(Srv ~ stage, data=data.set)
  #the probability of failure is calculated by subtracting the probability of 
  #survival from 1. 
  data.set$pr_failure18_2 = c(1- (summary(survfit(coxmod2,
  newdata=data.set), times=5)$surv))
  #Run the decision curve analysis (with a smoother)
  stdca(data=data.set, outcome="fustat", ttoutcome="futime", timepoint=5, 
        predictors="pr_failure18_2", #probability=T,
        xstop=0.6, smooth=F)
  km2 = stdca(data=data.set, outcome="fustat", ttoutcome="futime", timepoint=5, 
              predictors="pr_failure18_2", xstop=0.9, smooth=F)
  

    plot(km1$net.benefit.threshold, km1$net.benefit.none, type = "l", lwd=2, 
    xlim=c(0.15,0.95), ylim=c(-.05, 0.5), xlab = "Threshold Probability", 
    ylab = "Net Benefit")
    lines(km1$net.benefit$threshold, km1$net.benefit$all, type="l", col="grey", lwd=1)
    lines(km1$net.benefit$threshold, km1$net.benefit$pr_failure18, type="l", col="blue",lwd=2)
    abline(h=0, type="l", lty=3, col="black", lwd=2)
  
     #plot(km2$net.benefit.threshold, km2$net.benefit.none, type = "l", lwd=2, 
   #      xlim=c(-,.50), ylim=c(-.05, .20), xlab = "Threshold Probability", 
   #      ylab = "Net Benefit")
   #lines(km2$net.benefit$threshold, km2$net.benefit$all, type="l", col=8, lwd=2)
    
    lines(km2$net.benefit$threshold, km2$net.benefit$pr_failure18, type="l", col="green",lwd=2)
    legend("topright", cex=0.8, legend=c("All", "Nomogram", "Clinic features"), 
    col=c("grey", "blue","green"), lwd=c(2, 2, 2), lty=c(1, 1, 1))
    
    
