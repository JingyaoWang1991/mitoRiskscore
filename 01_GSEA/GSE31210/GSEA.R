rm(list = ls())
library(clusterProfiler)
library(ggplot2)

setwd('.01_GSEA/GSE31210')
cor_data_df =read.table("tumor-vs-normal.limma.txt",header=T,sep="\t",check.names=F) 


geneList <- cor_data_df$logFC
names(geneList) = cor_data_df$names
geneList = sort(geneList, decreasing = TRUE)

hallmarks <- read.gmt("hallmark-mito.gmt")
y <- GSEA(geneList,TERM2GENE =hallmarks,pvalueCutoff = 3)
dotplot(y,showCategory=5,split=".sign")+facet_grid(~.sign)

yd <- data.frame(y)
library(enrichplot)
gseaplot2(y, geneSetID = 1:5,pvalue_table = T)

library(gridExtra)
ln=row.names(yd)
ln=ln[1:5]
ydd=yd[1:5,c("p.adjust","NES")]
ydd=round(ydd, 3)
ydd=cbind(ln,ydd)
ydd=data.frame(ydd)

gseaplot2(y,geneSetID = 1:5,pvalue_table = T,subplots = 1:2)+
  annotation_custom(tableGrob(ydd, rows=NULL), 
                    xmin=0,xmax = 1, ymin=0)+annotate("text",0.1, 0.97, label = "PPAR-gene CNA vs. None", hjust=0, vjust=0,size = 5)
#6*12#