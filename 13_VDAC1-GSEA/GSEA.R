rm(list = ls())
library(clusterProfiler)
library(ggplot2)
setwd('.13_VDAC1-GSEA')

cor_data_df =read.table("high-vs-Low.edgeR.txt",header=T,sep="\t",check.names=F)  

geneList <- cor_data_df$logFC
names(geneList) = cor_data_df$names
geneList = sort(geneList, decreasing = TRUE)
hallmarks <- read.gmt("c2.cp.kegg.v7.2.symbols.gmt")
y <- GSEA(geneList,TERM2GENE =hallmarks,pvalueCutoff = 3)

yd <- data.frame(y)
library(enrichplot)
dotplot(y,showCategory=10,split=".sign")+facet_grid(~.sign)
