#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install(c("car", "ridge", "preprocessCore", "genefilter", "sva"))
#install.packages("ggpubr")

#####total drugs######
#A.443654, A.770041, ABT.263, ABT.888, AG.014699, AICAR, AKT.inhibitor.VIII, AMG.706, AP.24534, 
#AS601245, ATRA, AUY922, Axitinib, AZ628, AZD.0530, AZD.2281, AZD6244, AZD6482, AZD7762, AZD8055, 
#BAY.61.3606, Bexarotene, BI.2536, BIBW2992, Bicalutamide, BI.D1870, BIRB.0796, Bleomycin, 
#BMS.509744, BMS.536924, BMS.708163, BMS.754807, Bortezomib, Bosutinib, Bryostatin.1, BX.795, 
#Camptothecin, CCT007093, CCT018159, CEP.701, CGP.082996, CGP.60474, CHIR.99021, CI.1040, Cisplatin, 
#CMK, Cyclopamine, Cytarabine, Dasatinib, DMOG, Docetaxel, Doxorubicin, EHT.1864, Elesclomol, 
#Embelin, Epothilone.B, Erlotinib, Etoposide, FH535, FTI.277, GDC.0449, GDC0941, Gefitinib, 
#Gemcitabine, GNF.2, GSK269962A, GSK.650394, GW.441756, GW843682X, Imatinib, IPA.3, 
#JNJ.26854165, JNK.9L, JNK.Inhibitor.VIII, JW.7.52.1, KIN001.135, KU.55933, Lapatinib, 
#Lenalidomide, LFM.A13, Metformin, Methotrexate, MG.132, Midostaurin, Mitomycin.C, MK.2206, 
#MS.275, Nilotinib, NSC.87877, NU.7441, Nutlin.3a, NVP.BEZ235, NVP.TAE684, Obatoclax.Mesylate, 
#OSI.906, PAC.1, Paclitaxel, Parthenolide, Pazopanib, PD.0325901, PD.0332991, PD.173074, PF.02341066, 
#PF.4708671, PF.562271, PHA.665752, PLX4720, Pyrimethamine, QS11, Rapamycin, RDEA119, RO.3306, 
#Roscovitine, Salubrinal, SB.216763, SB590885, Shikonin, SL.0101.1, Sorafenib, S.Trityl.L.cysteine, 
#Sunitinib, Temsirolimus, Thapsigargin, Tipifarnib, TW.37, Vinblastine, Vinorelbine, Vorinostat, 
#VX.680, VX.702, WH.4.023, WO2009093972, WZ.1.84, X17.AAG, X681640, XMD8.85, Z.LLNle.CHO, ZM.447439


rm(list=ls())
library(limma)
library(ggpubr)
library(pRRophetic)
library(ggplot2)
set.seed(12345)
setwd('.12_pRRophetic') 
expFile="LUAD-fpkm-gene.txt" 
riskFile="lassoRisk.txt"
drug="Etoposide"         #drug name

rt = read.table(expFile, header=T, sep="\t", check.names=F)
rt=as.matrix(rt)
rownames(rt)=rt[,1]
exp=rt[,2:ncol(rt)]
dimnames=list(rownames(exp),colnames(exp))
data=matrix(as.numeric(as.matrix(exp)),nrow=nrow(exp),dimnames=dimnames)
data=avereps(data)
data=data[rowMeans(data)>0.5,]

#add dug senstivity
senstivity=pRRopheticPredict(data, drug, selection=1)
senstivity=senstivity[senstivity!="NaN"]
#senstivity[senstivity>quantile(senstivity,0.99)]=quantile(senstivity,0.99)


risk=read.table(riskFile, header=T, sep="\t", check.names=F, row.names=1)
sameSample=intersect(row.names(risk), names(senstivity))
riskscore=risk[sameSample, "riskScore",drop=F]
risk=risk[sameSample, "risk",drop=F]
senstivity=senstivity[sameSample]
rt=cbind(risk, senstivity,riskscore)

#set compare group
rt$risk=factor(rt$risk, levels=c("control","low", "high"))
type=levels(factor(rt[,"risk"]))
options(scipen = 200)
comp=combn(type, 2)
my_comparisons=list()
for(i in 1:ncol(comp)){my_comparisons[[i]]<-comp[,i]}

#box plot
rt$risk <- factor(rt$risk, levels = c("control","low", "high"))
b=ggboxplot(rt, x ="risk",
           y="senstivity", fill="risk",
            xlab="mitoRiskscore",width=0.5,size=0.8,
			      ylab=paste0(drug, " senstivity (IC50)"),
			      legend.title="Group",
			      add = "jitter", 
			      palette = "Dark2",font.x = c(15, "blue"),
			      font.y = c(15, "#993333"),
			      add.params = list(size=2,color=1,alpha=0.2)
			     )
b
b1=b+stat_compare_means(comparisons=my_comparisons,label="p.signif")
b1#5*3

#scatter plot
p <- ggscatter(rt,x="riskScore", y="senstivity", color=1,size=2,
               xlab="mitoRiskscore",
               ylab=paste0(drug, " senstivity (IC50)"))
p
options(scipen = 200)
p1=p+geom_smooth(size=2,alpha=0.3,color=4,se=T,method="lm")+stat_cor(data=rt, method = "pearson",p.accuracy=0.0001)
p1
