library(pheatmap)      
geneFile="gsva_data.txt"    
clusterFile="geneCluster.txt"       
setwd('11_risk-GSEA/gsva/heatmap') 


gene=read.table(geneFile, header=T, sep="\t", check.names=F, row.names=1)
cluster=read.table(clusterFile, header=T, sep="\t", check.names=F, row.names=1)

#combine data
gene=t(gene)
data=cbind(gene, cluster)
data=data[order(data$mitoRiskscore),]

#separate data
Type=data[,(35:ncol(data))]
data=t(data[,1:34])
row_type=read.table("rowtype.txt", header=F, sep="\t", check.names=F, row.names=1)
annotation_row=data.frame(row_type)
bk = unique(c(seq(-5,5, length=100)))
pdf("heatmap.pdf", height=5, width=8)
pheatmap(log2(data+1),
         breaks = bk,
         annotation=Type,
         #annotation_colors = ann_colors,
         color = colorRampPalette(c(rep("#33CCFF",5), "white", rep("#FF3333",5)))(100),
         cluster_cols =F,
         cluster_rows =F,
         clustering_method = "ward.D",
         scale="row",
         show_colnames=F,
         fontsize=6,
         fontsize_row=6,
         fontsize_col=6,
         annotation_row = annotation_row,
         gaps_col=c(251,502)
         ,gaps_row = c(8)
         )
dev.off()

