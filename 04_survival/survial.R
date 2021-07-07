
#install.packages("survival")
#install.packages("survminer")
rm(list=ls())
setwd('.04_survival')     
library(survival)
library("survminer")
rt=read.table("lassoRisk.txt",header=T,sep="\t")
diff=survdiff(Surv(futime, fustat) ~risk,data = rt)
pValue=1-pchisq(diff$chisq,df=1)
pValue=signif(pValue,4)
pValue=format(pValue, scientific = TRUE)

fit <- survfit(Surv(futime, fustat) ~ risk, data = rt)

#KM plot
pdf(file="survival.pdf",onefile = FALSE,
       width = 5.5,    
       height =5)      
ggsurvplot(fit, 
           data=rt,
           conf.int=F, #ÊÇ·ñ»­95%CI
           pval=paste0("p = ",pValue),
           pval.size=4,
           risk.table=TRUE,
           legend.labs=c("High risk", "Low risk"),
           legend.title="Risk",
           xlab="Time(years)",
           break.time.by = 1,
           risk.table.title="",
           #palette=c("red", "blue"),
           risk.table.height=.25)
dev.off()

summary(fit)   
