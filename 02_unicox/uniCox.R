
#install.packages('survival')
rm(list=ls()) 

library(survival)
pFilter=0.05
setwd('.02_unicox')

rt=read.table("TCGA-LUAD-Matrix.txt",header=T,sep="\t",check.names=F,row.names=1)     #读取输入文件

outTab=data.frame()
sigGenes=c("futime","fustat")
rt[,3:ncol(rt)]=log2(rt[,3:ncol(rt)]+1)
for(i in colnames(rt[,3:ncol(rt)])){
  cox <- coxph(Surv(futime, fustat) ~ rt[,i], data = rt)
  coxSummary = summary(cox)
  coxP=coxSummary$coefficients[,"Pr(>|z|)"]
  outTab=rbind(outTab,
                 cbind(id=i,
                       z=coxSummary$coefficients[,"z"],
                       HR=coxSummary$conf.int[,"exp(coef)"],
                       HR.95L=coxSummary$conf.int[,"lower .95"],
                       HR.95H=coxSummary$conf.int[,"upper .95"],
                       pvalue=coxSummary$coefficients[,"Pr(>|z|)"])
    )
  }
  #result
  outTab = outTab[is.na(outTab$pvalue)==FALSE,]
  outTab=outTab[order(as.numeric(as.vector(outTab$pvalue))),]
  write.table(outTab,file="totalCoxResult.txt",sep="\t",row.names=F,quote=F)
  #write table
  sigTab=outTab[as.numeric(as.vector(outTab$pvalue))<pFilter,]
  write.table(sigTab,file="sigcoxresult.txt",sep="\t",row.names=F,quote=F)
  sigGenes=c("futime","fustat")
  sigGenes=c(sigGenes,as.vector(sigTab[,1]))
  uniSigExp=rt[,sigGenes]
  uniSigExp=cbind(id=row.names(uniSigExp),uniSigExp)
  write.table(uniSigExp,file="uniSigExp.txt",sep="\t",row.names=F,quote=F)
  
  
  
######Forest plot######
rt <- read.table("sigcoxresult.txt",header=T,sep="\t",row.names=1,check.names=F)
gene <- rownames(rt)
hr <- sprintf("%.3f",rt$"HR")
hrLow  <- sprintf("%.3f",rt$"HR.95L")
hrHigh <- sprintf("%.3f",rt$"HR.95H")
Hazard.ratio <- paste0(hr,"(",hrLow,"-",hrHigh,")")
pVal <- ifelse(rt$pvalue<0.001, "<0.001", sprintf("%.3f", rt$pvalue))

pdf(file="forest.pdf", width = 6,height = 4.5)
n <- nrow(rt)
nRow <- n+1
ylim <- c(1,nRow)
layout(matrix(c(1,2),nc=2),width=c(3,2))
xlim = c(0,3)
par(mar=c(4,2.5,2,1))
plot(1,xlim=xlim,ylim=ylim,type="n",axes=F,xlab="",ylab="")
text.cex=0.8
text(0,n:1,gene,adj=0,cex=text.cex)
text(1.5-0.5*0.2,n:1,pVal,adj=1,cex=text.cex);text(1.5-0.5*0.2,n+1,'pvalue',cex=text.cex,font=2,adj=1)
text(3,n:1,Hazard.ratio,adj=1,cex=text.cex);text(3,n+1,'Hazard ratio',cex=text.cex,font=2,adj=1,)



#Forest plot
par(mar=c(4,1,2,1),mgp=c(3,0.5,0))
xlim = c(0,max(as.numeric(hrLow),as.numeric(hrHigh)))
plot(1,xlim=xlim,ylim=ylim,type="n",axes=F,ylab="",xaxs="i",xlab="Hazard ratio")
arrows(as.numeric(hrLow),n:1,as.numeric(hrHigh),n:1,angle=90,code=0,length=0.05,col="black",lwd=1.5)
abline(v=1,col="black",lty=3,lwd=1)
boxcolor = ifelse(as.numeric(hr) > 1, '#f8766c', '#00bec4')
points(as.numeric(hr), n:1, pch = 18, col = boxcolor, cex=1.3)
axis(1)
dev.off()
