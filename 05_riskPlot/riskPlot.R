
#install.packages("pheatmap")
rm(list=ls())
library(pheatmap)
library(ggrisk)
library(tidyverse)

setwd('.05_riskPlot')    
rt=read.table("lassoRisk.txt",sep="\t",header=T,row.names=1,check.names=F) 
rt=rt[order(rt$riskScore),] 

###riskscore
riskClass=rt[,"risk"]
lowLength=length(riskClass[riskClass=="low"])
highLength=length(riskClass[riskClass=="high"])
line=rt[,"riskScore"]
line[line>10]=10
pdf(file="riskScore.pdf",width = 10,height = 4)
plot(line,
     type="p",
     pch=20,
     xlab="Patients (increasing risk socre)",
     ylab="Risk score",
     col=c(rep("#00bec4",lowLength),
     rep("#f8766c",highLength)))
abline(h=median(rt$riskScore),v=lowLength,lty=2)
legend("topleft", c("High risk", "low Risk"),bty="n",pch=19,col=c("#f8766c","#00bec4"),cex=1.2)
dev.off()

###survival state
color=as.vector(rt$fustat)
color[color==1]="#f8766c"
color[color==0]="#00bec4"
pdf(file="survStat.pdf",width = 10,height = 4)
plot(rt$futime,
     pch=19,
     xlab="Patients (increasing risk socre)",
     ylab="Survival time (years)",
     col=color)
legend("topleft", c("Dead", "Alive"),bty="n",pch=19,col=c("#f8766c","#00bec4"),cex=1.2)
abline(v=lowLength,lty=2)
dev.off()

#heat map
rt1=read.table("heat-input.txt",sep="\t",header=T,row.names=1,check.names=F)
colgroup=read.table("heat-group.txt",sep="\t",header=T,row.names=1,check.names=F)
colnames(colgroup)=c("Risk","Stage")
rownames(annotation)=rownames(rt)
pdf(file="heatmap.pdf",width = 10,height = 4)
pheatmap(rt1, 
         annotation_col = colgroup, 
                  cluster_cols = FALSE,
         fontsize_row=11,
         show_colnames = F,
         fontsize_col=3,
         color = colorRampPalette(c("#00bec4", "white", "#f8766c"))(50) )
dev.off()