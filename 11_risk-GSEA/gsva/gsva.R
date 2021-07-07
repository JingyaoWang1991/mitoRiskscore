suppressMessages(library(GSVA))
suppressMessages(library(GSVAdata))
suppressMessages(library(GSEABase))
suppressMessages(library(limma))
library(data.table)

rm(list=ls())
setwd('11_risk-GSEA/gsva') 

#read gmt
gmt_file="yd01.gmt"
geneset <- getGmt(gmt_file)  

#read expression
exp=read.table("LUAD-fpkm-gene.txt", header=T, sep="\t", check.names=F, row.names=1)
gsva_data  <- gsva(as.matrix(exp), geneset,
           verbose=TRUE,method = "ssgsea")
gsva_data  <- as.matrix(gsva_data)

write.table(gsva_data,file="gsva_data.txt",sep="\t",row.names=T,quote=F)

