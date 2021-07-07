#install.packages("glmnet")
#install.packages("survival")

rm(list=ls())

library("glmnet")
library("survival")

setwd('.03_lasso')

rt=read.table("uniSigExp.txt",header=T,sep="\t",row.names=1,check.names=F)       #¶ÁÈ¡ÎÄ¼þ
rt$futime=rt$futime/365
rt[,3:ncol(rt)]=log2(rt[,3:ncol(rt)]+1)
x=as.matrix(rt[,c(3:ncol(rt))])
y=data.matrix(Surv(rt$futime,rt$fustat))

fit <- glmnet(x, y, family = "cox", maxit = 500)
pdf("lambda.pdf")
plot(fit, xvar = "lambda", label = TRUE)
dev.off()

cvfit <- cv.glmnet(x, y, family="cox", maxit = 1000)
pdf("cvfit.pdf")
plot(cvfit)
abline(v=log(c(cvfit$lambda.min,cvfit$lambda.1se)),lty="dashed")
dev.off()

#write table
coef <- coef(fit, s = cvfit$lambda.min)
index <- which(coef != 0)
actCoef <- coef[index]
lassoGene=row.names(coef)[index]
geneCoef=cbind(Gene=lassoGene,Coef=actCoef)
write.table(geneCoef,file="geneCoef.txt",sep="\t",quote=F,row.names=F)

riskScore=predict(cvfit, newx = x, s = "lambda.min",type="response")
outCol=c("futime","fustat",lassoGene)
risk=as.vector(ifelse(riskScore>median(riskScore),"high","low"))
outTab=cbind(rt[,outCol],riskScore=as.vector(riskScore),risk)
write.table(cbind(id=rownames(outTab),outTab),
    file="lassoRisk.txt",
    sep="\t",
    quote=F,
    row.names=F)

###

