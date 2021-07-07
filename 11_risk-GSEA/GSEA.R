
rm(list = ls())
library(clusterProfiler)
library(ggplot2)

setwd('.11_risk-GSEA')

cor_data_df =read.table("high-vs-low.edgeR.txt",header=T,sep="\t",check.names=F)  

geneList <- cor_data_df$logFC
names(geneList) = cor_data_df$names
geneList = sort(geneList, decreasing = TRUE)

hallmarks <- read.gmt("c2.cp.kegg.v7.2.symbols.gmt")
y <- GSEA(geneList,TERM2GENE =hallmarks,pvalueCutoff = 3)
#dotplot(y,showCategory=5,split=".sign")+facet_grid(~.sign)

yd <- data.frame(y)
library(enrichplot)

write.table(yd,file="kegg.txt",sep="\t",row.names=T,quote=F)
gseaplot2(y, geneSetID = 1:5,pvalue_table = T)