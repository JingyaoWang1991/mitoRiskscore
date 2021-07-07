#install.packages("survival")
#install.packages("survminer")
rm(list=ls())
setwd('10_ex-validition/GSE31210')
library(survival)
library("survminer")
rt=read.table("GSE31210-risksurv.txt",header=T,sep="\t")
rt$futime=rt$futime/365
diff=survdiff(Surv(futime, fustat) ~risk,data = rt)
pValue=1-pchisq(diff$chisq,df=1)
pValue=signif(pValue,4)
pValue=format(pValue, scientific = F)

fit <- survfit(Surv(futime, fustat) ~ risk, data = rt)

#»æÖÆÉú´æÇúÏß
pdf(file="survival.pdf",onefile = FALSE,
       width = 5.5,        
       height =5)          
ggsurvplot(fit, 
           data=rt,
           conf.int=F, 
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

#summary(fit) 
