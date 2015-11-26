library(reshape2)
library(car)
library(qpcR)
library(xtable)
library(Hmisc)
library(plyr)
library(foreign)
setwd("/Users/sergej/Documents/Master/Thesis/June_Empirical_Analysis/Send_Liza/Results")
results<-read.dta("results_02_11_2015.dta")
IND.70.74<-results[, c("COU", "IND", "sector")]
IND.70.74<-IND.70.74[IND.70.74$IND=="70T74",]
IND.70.74<-IND.70.74[order(-IND.70.74[,3]),]

IND.70.74.exgr<-results[, c("COU", "IND", "sector_exgr")]
IND.70.74.exgr<-IND.70.74.exgr[IND.70.74.exgr$IND=="70T74",]
IND.70.74.exgr<-IND.70.74.exgr[order(-IND.70.74.exgr[,3]),]

IND.70.74.both<-c(IND.70.74.exgr,IND.70.74) 
IND.70.74.both[[2]]<-NULL
IND.70.74.both[[4]]<-NULL
xtable(as.data.frame(IND.70.74.both))

bench.industry<-"01-05"
benchmark<-"ROW"
results$IND<-gsub("T","-", results$IND)
prod.2005<-results[, c("COU", "IND", "revealed_productivity_2005")]
prod.2005.exgr<-results[, c("COU", "IND","revealed_productivity_exgr_2005")]
prod.1995<-results[, c("COU", "IND","revealed_productivity_1995" )]
in.centrality.va.2005<-results[, c("COU", "IND","in_eigen_va_2005")]
in.centrality.va.1995<-results[, c("COU", "IND","in_eigen_va_1995")]

prod.2005.wide<-dcast(out.centrality.va.2005, COU ~ IND, value.var="revealed_productivity_2005")
prod.2005.exgr.wide<-dcast(prod.2005.exgr, COU ~ IND, value.var="revealed_productivity_exgr_2005")
prod.1995.wide<-dcast(prod.1995 , COU ~ IND, value.var="revealed_productivity_1995" )

in.centrality.va.2005.wide<-dcast(in.centrality.va.2005,COU ~ IND, value.var="in_eigen_va_2005")
in.centrality.va.1995.wide<-dcast(in.centrality.va.1995, COU ~ IND, value.var="in_eigen_va_1995")

results<-subset(results,  results$IND %nin% bench.industry) 
results<-subset(results,  results$COU %nin% benchmark) 

in.norm.centrality.va.2005<-results[, c("COU", "IND","in_centrality_normalized_2005")]
in.norm.centrality.va.1995<-results[, c("COU", "IND","in_centrality_normalized_1995")]

norm.prod.va.2005<-results[, c("COU", "IND","sector")]
norm.prod.va.1995<-results[, c("COU", "IND","sector_VA_95")]
norm.prod.exgr.2005<-results[, c("COU", "IND","sector_exgr")]
norm.prod.exgr.1995<-results[, c("COU", "IND","sector_exgr_1995")]
in.norm.centrality.va.2005.wide<-dcast(in.norm.centrality.va.2005, COU ~ IND, value.var="in_centrality_normalized_2005")
in.norm.centrality.va.1995.wide<-dcast(in.norm.centrality.va.1995, COU ~ IND, value.var="in_centrality_normalized_1995" )

norm.prod.va.2005.wide<-dcast(norm.prod.va.2005, COU ~IND, value.var="sector")
norm.prod.va.1995.wide<-dcast(norm.prod.va.1995, COU ~IND, value.var="sector_VA_95")
norm.prod.exgr.2005.wide<-dcast(norm.prod.exgr.2005, COU ~IND, value.var="sector_exgr")
norm.prod.exgr.1995.wide<-dcast(norm.prod.exgr.1995, COU ~IND, value.var="sector_exgr_1995")



#correlation matrixes
in.centrality.va.cor<-(in.centrality.va.2005.wide[,2:ncol(in.centrality.va.2005.wide)])
centrality.va.cor<-(in.norm.centrality.va.2005.wide[,2:ncol(in.norm.centrality.va.2005.wide)])
ricardo.va.2005.cor<- (norm.prod.va.2005.wide[,2:ncol(norm.prod.va.2005.wide)])
ricardo.va.1995.cor<- (norm.prod.va.1995.wide[,2:ncol(norm.prod.va.1995.wide)])
ricardo.exgr.2005.cor<-(norm.prod.exgr.2005.wide[,2:ncol(norm.prod.exgr.2005.wide)])
ricardo.exgr.1995.cor<-(norm.prod.exgr.1995.wide[,2:ncol(norm.prod.exgr.1995.wide)])
centrality.1995.va.cor<-(in.norm.centrality.va.1995.wide[,2:ncol(in.norm.centrality.va.1995.wide)])

#correlation Structural RCA and relative network centrality 
pearson<-diag(cor(ricardo.va.2005.cor,centrality.va.cor,method="pearson"))
spearman<-diag(cor(ricardo.va.2005.cor,centrality.va.cor,method="spearman"))
Cor.pearson.spear<-cbind(pearson,spearman)

Cor.test.pear.centr.ricardo<-matrix(0, nrow = 21, ncol = 1)
Cor.test.spear.centr.ricardo<-matrix(0, nrow = 21, ncol = 1)

for (i in 1: ncol(centrality.va.cor)) {
  Cor.test.pear.centr.ricardo[i,1]<-as.vector(cor.test(as.vector(ricardo.va.2005.cor[,i]), as.vector(centrality.va.cor[,i]),method = c("pearson"))[[3]])
}
for (i in 1: ncol(centrality.va.cor)) {
  Cor.test.spear.centr.ricardo[i,1]<-as.vector(cor.test(as.vector(ricardo.va.2005.cor[,i]), as.vector(centrality.va.cor[,i]),method = c("spearman"))[[3]])
}

colnames(Cor.test.pear.centr.ricardo)<-c('p-val pearson')

colnames(Cor.test.spear.centr.ricardo)<-c('p-val spearman')

null<-c(0)
Cor.test.pear.centr.ricardo<-rbind(Cor.test.pear.centr.ricardo,null)
Cor.test.pear.centr.ricardo<-rbind(Cor.test.pear.centr.ricardo,null)

Cor.test.spear.centr.ricardo<-rbind(Cor.test.spear.centr.ricardo,null)


avg.pear<-mean(diag(cor(ricardo.va.2005.cor,centrality.va.cor,method="pearson")))
avg.spear<-mean(diag(cor(ricardo.va.2005.cor,centrality.va.cor,method="spearman")))
med.pear<-median(diag(cor(ricardo.va.2005.cor,centrality.va.cor,method="pearson")))
med.spear<-median(diag(cor(ricardo.va.2005.cor,centrality.va.cor,method="spearman")))
median<-cbind(med.pear,med.spear)
rownames(median)<-c("median")
avg<-cbind(avg.pear,avg.spear)
rownames(avg)<-c("AVG")
Cor.pearson.spear<-rbind(Cor.pearson.spear,avg) 
Cor.pearson.spear<-rbind(Cor.pearson.spear,median)
Cor.pearson.spear.w.test<-Cor.pearson.spear
Cor.pearson.spear<-cbind(Cor.pearson.spear,Cor.test.spear.centr.ricardo)
Cor.pearson.spear<-cbind(Cor.pearson.spear,Cor.test.spear.centr.ricardo)

#Correlation Structural RCA VA 2005 EXGR2005
pearson.exgr.va<-diag(cor(ricardo.va.2005.cor,ricardo.exgr.2005.cor,method="pearson"))
spearman.exgr.va<-diag(cor(ricardo.va.2005.cor,ricardo.exgr.2005.cor,method="spearman"))
Cor.pearson.spear.exgr.va<-cbind(pearson.exgr.va,spearman.exgr.va)
Cor.test.pear.exgr.va.ricardo<-matrix(0, nrow = 20, ncol = 1)
Cor.test.spear.exgr.va.ricardo<-matrix(0, nrow = 20, ncol = 1)

for (i in 1: ncol(centrality.va.cor)) {
  Cor.test.pear.exgr.va.ricardo[i,1]<-as.vector(cor.test(as.vector(ricardo.va.2005.cor[,i]), as.vector(ricardo.exgr.2005.cor[,i]),method = c("pearson"))[[3]])
}
for (i in 1: ncol(centrality.va.cor)) {
  Cor.test.spear.exgr.va.ricardo[i,1]<-as.vector(cor.test(as.vector(ricardo.va.2005.cor[,i]), as.vector(ricardo.exgr.2005.cor[,i]),method = c("spearman"))[[3]])
}

colnames(Cor.test.pear.exgr.va.ricardo)<-c('p-val pearson')

colnames(Cor.test.spear.exgr.va.ricardo)<-c('p-val spearman')


Cor.test.pear.exgr.va.ricardo<-rbind(Cor.test.pear.exgr.va.ricardo,null)
Cor.test.spear.exgr.va.ricardo<-rbind(Cor.test.spear.exgr.va.ricardo,null)


avg.pear.exgr.va<-mean(diag(cor(ricardo.va.2005.cor,ricardo.exgr.2005.cor,method="pearson")))
avg.spear.exgr.va<-mean(diag(cor(ricardo.va.2005.cor,ricardo.exgr.2005.cor,method="spearman")))
avg.exgr.va<-cbind(avg.pear.exgr.va,avg.spear.exgr.va)
rownames(avg.exgr.va)<-c("AVG")
Cor.pearson.spear.exgr.va<-rbind(Cor.pearson.spear.exgr.va,avg.exgr.va)
Cor.pearson.spear.exgr.va.w.test<-Cor.pearson.spear.exgr.va
Cor.pearson.spear.exgr.va<-cbind(Cor.pearson.spear.exgr.va,Cor.test.pear.exgr.va.ricardo)
Cor.pearson.spear.exgr.va<-cbind(Cor.pearson.spear.exgr.va,Cor.test.spear.exgr.va.ricardo)



#Correlation Structural RCA EXGR VA 1995 
pearson.va.1995.2005<-diag(cor(ricardo.exgr.1995.cor,ricardo.va.1995.cor,method="pearson"))

#significance 

#empty matrix
Cor.test.pear<-matrix(0, nrow = 20, ncol = 1)
Cor.test.spear<-matrix(0, nrow = 20, ncol = 1)

for (i in 1: ncol(centrality.va.cor)) {
  Cor.test.pear[i,1]<-as.vector(cor.test(as.vector(ricardo.exgr.1995.cor[,i]), as.vector(ricardo.va.1995.cor[,i]),method = c("pearson"))[[3]])
}
for (i in 1: ncol(centrality.va.cor)) {
  Cor.test.spear[i,1]<-as.vector(cor.test(as.vector(ricardo.exgr.1995.cor[,i]), as.vector(ricardo.va.1995.cor[,i]),method = c("spearman"))[[3]])
}

colnames(Cor.test.pear)<-c('p-val pearson')

colnames(Cor.test.spear)<-c('p-val spearman')

Cor.test.pear<-rbind(Cor.test.pear,null)
Cor.test.spear<-rbind(Cor.test.spear,null)

#avg and median pairwise pearson/spearman correlation
spearman.va.1995.2005<-diag(cor(ricardo.exgr.1995.cor,ricardo.va.1995.cor,method="spearman"))
Cor.pearson.spear.1995.2005<-cbind(pearson.va.1995.2005,spearman.va.1995.2005)

avg.pear.1995.2005<-mean(diag(cor(ricardo.exgr.1995.cor,ricardo.va.1995.cor,method="pearson")))
avg.spear.1995.2005<-mean(diag(cor(ricardo.exgr.1995.cor,ricardo.va.1995.cor,method="spearman")))
avg.1995.2005<-cbind(avg.pear.1995.2005,avg.spear.1995.2005)
rownames(avg.1995.2005)<-c("AVG")
Cor.pearson.spear.1995.2005<-rbind(Cor.pearson.spear.1995.2005,avg.1995.2005) 
Cor.pearson.spear.1995.2005.w.test<-Cor.pearson.spear.1995.2005
Cor.pearson.spear.1995.2005<-cbind(Cor.pearson.spear.1995.2005,Cor.test.pear)
Cor.pearson.spear.1995.2005<-cbind(Cor.pearson.spear.1995.2005,Cor.test.spear)



#correlation network centrality and structural RCA 1995
pearson.centr<-diag(cor(centrality.1995.va.cor,ricardo.va.1995.cor,method="pearson"))
spearman.centr<-diag(cor(centrality.1995.va.cor,ricardo.va.1995.cor,method="spearman"))
Cor.pearson.spear.centr<-cbind(pearson.centr,spearman.centr)

#significance 

#empty matrix
Cor.test.pear.centr<-matrix(0, nrow = 20, ncol = 1)
Cor.test.spear.centr<-matrix(0, nrow = 20, ncol = 1)

#write results of test for association in matrix
for (i in 1: ncol(ricardo.va.1995.cor)) {
  Cor.test.pear.centr[i,1]<-as.vector(cor.test(as.vector(centrality.1995.va.cor[,i]), as.vector(ricardo.va.1995.cor[,i]),method = c("pearson"))[[3]])
}
for (i in 1: ncol(ricardo.va.1995.cor)) {
  Cor.test.spear.centr[i,1]<-as.vector(cor.test(as.vector(centrality.1995.va.cor[,i]), as.vector(ricardo.va.1995.cor[,i]),method = c("spearman"))[[3]])
}

colnames(Cor.test.pear.centr)<-c('p-val pearson')

colnames(Cor.test.spear.centr)<-c('p-val spearman')

#prepare for latex output
Cor.test.pear.centr<-rbind(Cor.test.pear.centr,null)
Cor.test.spear.centr<-rbind(Cor.test.spear.centr,null)

Cor.test.pear.centr<-rbind(Cor.test.pear.centr,null)
Cor.test.spear.centr<-rbind(Cor.test.spear.centr,null)

#avg and median pairwise pearson/spearman correlation
avg.pear.centr<-mean(diag(cor(centrality.1995.va.cor,ricardo.va.1995.cor)))
avg.spear.centr<-mean(diag(cor(centrality.1995.va.cor,ricardo.va.1995.cor,method="spearman")))
med.pear.centr<-median(diag(cor(centrality.1995.va.cor,ricardo.va.1995.cor)))
med.spear.centr<-median(diag(cor(centrality.1995.va.cor,ricardo.va.1995.cor,method="spearman")))

median.centr<-cbind(med.pear.centr,med.spear.centr)
rownames(median.centr)<-c("median")
avg.centr<-cbind(avg.pear.centr,avg.spear.centr)
rownames(avg.centr)<-c("AVG")

Cor.pearson.spear.centr<-rbind(Cor.pearson.spear.centr,avg.centr) 
Cor.pearson.spear.centr<-rbind(Cor.pearson.spear.centr,median.centr)

Cor.pearson.spear.centr.w.test<-Cor.pearson.spear.centr

Cor.pearson.spear.centr<-cbind(Cor.pearson.spear.centr,Cor.test.pear.centr)
Cor.pearson.spear.centr<-cbind(Cor.pearson.spear.centr,Cor.test.spear.centr)

#Print Tables in Latex

xtable(Cor.pearson.spear) # 2005 centrality and RCA
xtable(Cor.pearson.spear.exgr.va) # RCA for EXGR and EXGR DVA
xtable(Cor.pearson.spear.1995.2005)# RCA for EXGR and EXGR DVA for 1995
xtable(Cor.pearson.spear.centr) # 1995 centrality and RCA

#repeat procedure for sample without rest of the world 

setwd("/Users/sergej/Documents/Master/Thesis/June_Empirical_Analysis/Send_liza/Results_without_row")
results.w.row<-read.dta("results_31_07_2015_without_row.dta")
results.w.row$IND<-gsub("T","-", results.w.row$IND) 
benchmark2<-"USA"
prod.2005.w.row<-results.w.row[, c("COU", "IND", "revealed_productivity_2005")]
prod.2005.w.row.exgr<-results.w.row[, c("COU", "IND","revealed_productivity_exgr_2005")]
prod.1995.w.row<-results.w.row[, c("COU", "IND","revealed_productivity_1995" )]
in.centrality.va.2005.w.row<-results.w.row[, c("COU", "IND","in_centrality_VA_2005")]
in.centrality.va.1995.w.row<-results.w.row[, c("COU", "IND","in_centrality_VA_1995")]

prod.2005.w.row.wide<-dcast(prod.2005.w.row, COU ~ IND, value.var="revealed_productivity_2005")
prod.2005.w.row.exgr.wide<-dcast(prod.2005.w.row.exgr, COU ~ IND, value.var="revealed_productivity_exgr_2005")
prod.1995.w.row.wide<-dcast(prod.1995.w.row , COU ~ IND, value.var="revealed_productivity_1995" )

in.centrality.va.2005.w.row.wide<-dcast(in.centrality.va.2005.w.row,COU ~ IND, value.var="in_centrality_VA_2005")
in.centrality.va.1995.w.row.wide<-dcast(in.centrality.va.1995.w.row, COU ~ IND, value.var="in_centrality_VA_1995")

results.w.row<-subset(results.w.row,  results.w.row$IND %nin% bench.industry) 
results.w.row<-subset(results.w.row,  results.w.row$COU %nin% benchmark2) 

in.norm.centrality.va.w.row.2005<-results.w.row[, c("COU", "IND","in_centrality_normalized_2005")]
in.norm.centrality.va.w.row.1995<-results.w.row[, c("COU", "IND","in_centrality_normalized_1995")]

norm.prod.va.w.row.2005<-results.w.row[, c("COU", "IND","sector")]
norm.prod.va.w.row.1995<-results.w.row[, c("COU", "IND","sector_VA_1995")]
norm.prod.exgr.w.row.2005<-results.w.row[, c("COU", "IND","sector_exgr")]
norm.prod.exgr.w.row.1995<-results.w.row[, c("COU", "IND","sector_exgr_1995")]

in.norm.centrality.va.w.row.2005.wide<-dcast(in.norm.centrality.va.w.row.2005, COU ~ IND, value.var="in_centrality_normalized_2005")
in.norm.centrality.va.w.row.1995.wide<-dcast(in.norm.centrality.va.w.row.1995, COU ~ IND, value.var="in_centrality_normalized_1995" )

norm.prod.va.w.row.2005.wide<-dcast(norm.prod.va.w.row.2005, COU ~IND, value.var="sector")
norm.prod.va.w.row.1995.wide<-dcast(norm.prod.va.w.row.1995, COU ~IND, value.var="sector_VA_1995")
norm.prod.exgr.w.row.2005.wide<-dcast(norm.prod.exgr.w.row.2005, COU ~IND, value.var="sector_exgr")
norm.prod.exgr.w.row.1995.wide<-dcast(norm.prod.exgr.w.row.1995, COU ~IND, value.var="sector_exgr_1995")




#correlation matrixes
in.centrality.va.w.row.cor<-(in.centrality.va.2005.w.row.wide[,2:ncol(in.centrality.va.2005.w.row.wide)])
centrality.va.w.row.cor<-(in.norm.centrality.va.w.row.2005.wide[,2:ncol(in.norm.centrality.va.w.row.2005.wide)])
ricardo.va.2005.w.row.cor<- (norm.prod.va.w.row.2005.wide[,2:ncol(norm.prod.va.w.row.2005.wide)])
ricardo.va.1995.cor<- (norm.prod.va.w.row.1995.wide[,2:ncol(norm.prod.va.w.row.1995.wide)])
ricardo.exgr.2005.w.row.cor<-(norm.prod.exgr.w.row.2005.wide[,2:ncol(norm.prod.exgr.w.row.2005.wide)])
ricardo.exgr.1995.w.row.cor<-(norm.prod.exgr.w.row.1995.wide[,2:ncol(norm.prod.exgr.w.row.1995.wide)])
centrality.1995.va.w.row.cor<-(in.norm.centrality.va.w.row.1995.wide[,2:ncol(in.norm.centrality.va.w.row.1995.wide)])

#correlation
pearson.w.row<-diag(cor(ricardo.va.2005.w.row.cor,centrality.va.w.row.cor,method="pearson"))
spearman.w.row<-diag(cor(ricardo.va.2005.w.row.cor,centrality.va.w.row.cor,method="spearman"))
Cor.pearson.w.row.spear<-cbind(pearson.w.row,spearman.w.row)

Cor.test.pear.centr.w.row.ricardo<-matrix(0, nrow = 21, ncol = 1)
Cor.test.spear.centr.w.row.ricardo<-matrix(0, nrow = 21, ncol = 1)

for (i in 1: ncol(centrality.va.w.row.cor)) {
  Cor.test.pear.centr.w.row.ricardo[i,1]<-as.vector(cor.test(as.vector(ricardo.va.2005.w.row.cor[,i]), as.vector(centrality.va.w.row.cor[,i]),method = c("pearson"))[[3]])
}
for (i in 1: ncol(centrality.va.w.row.cor)) {
  Cor.test.spear.centr.w.row.ricardo[i,1]<-as.vector(cor.test(as.vector(ricardo.va.2005.w.row.cor[,i]), as.vector(centrality.va.w.row.cor[,i]),method = c("spearman"))[[3]])
}

colnames(Cor.test.pear.centr.w.row.ricardo)<-c('p-val pearson')

colnames(Cor.test.spear.centr.w.row.ricardo)<-c('p-val spearman')

null<-c(0)
Cor.test.pear.centr.w.row.ricardo<-rbind(Cor.test.pear.centr.w.row.ricardo,null)

Cor.test.spear.centr.w.row.ricardo<-rbind(Cor.test.spear.centr.w.row.ricardo,null)


avg.pear.w.row<-mean(diag(cor(ricardo.va.2005.w.row.cor,centrality.va.w.row.cor,method="pearson")))
avg.spear.w.row<-mean(diag(cor(ricardo.va.2005.w.row.cor,centrality.va.w.row.cor,method="spearman")))
med.pear.w.row<-median(diag(cor(ricardo.va.2005.w.row.cor,centrality.va.w.row.cor,method="pearson")))
med.spear.w.row<-median(diag(cor(ricardo.va.2005.w.row.cor,centrality.va.w.row.cor,method="spearman")))
median.w.row<-cbind(med.pear.w.row,med.spear.w.row)
rownames(median.w.row)<-c("median")
avg.w.row<-cbind(avg.pear.w.row,avg.spear.w.row)
rownames(avg.w.row)<-c("AVG")
Cor.pearson.w.row.spear<-rbind(Cor.pearson.w.row.spear,avg.w.row) 
Cor.pearson.w.row.spear<-rbind(Cor.pearson.w.row.spear,median.w.row)
Cor.pearson.w.row.spear.w.test<-Cor.pearson.w.row.spear
Cor.pearson.w.row.spear<-cbind(Cor.pearson.w.row.spear,Cor.test.pear.centr.w.row.ricardo)
Cor.pearson.w.row.spear<-cbind(Cor.pearson.w.row.spear,Cor.test.spear.centr.w.row.ricardo)

#VA 2005 EXGR2005
pearson.w.row.exgr.va<-diag(cor(ricardo.va.2005.w.row.cor,ricardo.exgr.2005.w.row.cor,method="pearson"))
spearman.exgr.w.row.va<-diag(cor(ricardo.va.2005.w.row.cor,ricardo.exgr.2005.w.row.cor,method="spearman"))
Cor.pearson.w.row.spear.exgr.va<-cbind(pearson.w.row.exgr.va,spearman.exgr.w.row.va)
Cor.test.pear.exgr.w.row.va.ricardo<-matrix(0, nrow = 20, ncol = 1)
Cor.test.spear.exgr.w.row.va.ricardo<-matrix(0, nrow = 20, ncol = 1)

for (i in 1: ncol(centrality.va.w.row.cor)) {
  Cor.test.pear.exgr.w.row.va.ricardo[i,1]<-as.vector(cor.test(as.vector(ricardo.va.2005.w.row.cor[,i]), as.vector(ricardo.exgr.2005.w.row.cor[,i]),method = c("pearson"))[[3]])
}
for (i in 1: ncol(centrality.va.w.row.cor)) {
  Cor.test.spear.exgr.w.row.va.ricardo[i,1]<-as.vector(cor.test(as.vector(ricardo.va.2005.w.row.cor[,i]), as.vector(ricardo.exgr.2005.w.row.cor[,i]),method = c("spearman"))[[3]])
}

colnames(Cor.test.pear.exgr.w.row.va.ricardo)<-c('p-val pearson')

colnames(Cor.test.spear.exgr.w.row.va.ricardo)<-c('p-val spearman')


Cor.test.pear.exgr.w.row.va.ricardo<-rbind(Cor.test.pear.exgr.w.row.va.ricardo,null)
Cor.test.spear.exgr.w.row.va.ricardo<-rbind(Cor.test.spear.exgr.w.row.va.ricardo,null)


avg.pear.w.row.exgr.va<-mean(diag(cor(ricardo.va.2005.w.row.cor,ricardo.exgr.2005.w.row.cor,method="pearson")))
avg.spear.w.row.exgr.va<-mean(diag(cor(ricardo.va.2005.w.row.cor,ricardo.exgr.2005.w.row.cor,method="spearman")))
avg.w.row.exgr.va<-cbind(avg.pear.w.row.exgr.va,avg.spear.w.row.exgr.va)
rownames(avg.w.row.exgr.va)<-c("AVG")
Cor.pearson.w.row.spear.exgr.va<-rbind(Cor.pearson.w.row.spear.exgr.va,avg.w.row.exgr.va)
Cor.pearson.w.row.spear.exgr.va.w.test<-Cor.pearson.w.row.spear.exgr.va
Cor.pearson.w.row.spear.exgr.va<-cbind(Cor.pearson.w.row.spear.exgr.va,Cor.test.pear.exgr.w.row.va.ricardo)
Cor.pearson.w.row.spear.exgr.va<-cbind(Cor.pearson.w.row.spear.exgr.va,Cor.test.spear.exgr.w.row.va.ricardo)



#1995 Ricardo VA EXGR
pearson.w.row.va.1995.2005<-diag(cor(ricardo.exgr.1995.w.row.cor,ricardo.va.1995.cor,method="pearson"))


Cor.test.pear.w.row<-matrix(0, nrow = 20, ncol = 1)
Cor.test.spear.w.row<-matrix(0, nrow = 20, ncol = 1)

for (i in 1: ncol(centrality.va.w.row.cor)) {
  Cor.test.pear.w.row[i,1]<-as.vector(cor.test(as.vector(ricardo.exgr.1995.w.row.cor[,i]), as.vector(ricardo.va.1995.cor[,i]),method = c("pearson"))[[3]])
}
for (i in 1: ncol(centrality.va.w.row.cor)) {
  Cor.test.spear.w.row[i,1]<-as.vector(cor.test(as.vector(ricardo.exgr.1995.w.row.cor[,i]), as.vector(ricardo.va.1995.cor[,i]),method = c("spearman"))[[3]])
}

colnames(Cor.test.pear.w.row)<-c('p-val pearson')

colnames(Cor.test.spear.w.row)<-c('p-val spearman')

Cor.test.pear.w.row<-rbind(Cor.test.pear.w.row,null)
Cor.test.spear.w.row<-rbind(Cor.test.spear.w.row,null)

spearman.va.1995.2005<-diag(cor(ricardo.exgr.1995.w.row.cor,ricardo.va.1995.cor,method="spearman"))
Cor.pearson.w.row.spear.1995.2005<-cbind(pearson.w.row.va.1995.2005,spearman.va.1995.2005)

avg.pear.w.row.1995.2005<-mean(diag(cor(ricardo.exgr.1995.w.row.cor,ricardo.va.1995.cor,method="pearson")))
avg.spear.w.row.1995.2005<-mean(diag(cor(ricardo.exgr.1995.w.row.cor,ricardo.va.1995.cor,method="spearman")))
avg.w.row.1995.2005<-cbind(avg.pear.w.row.1995.2005,avg.spear.w.row.1995.2005)
rownames(avg.w.row.1995.2005)<-c("AVG")
Cor.pearson.w.row.spear.1995.2005<-rbind(Cor.pearson.w.row.spear.1995.2005,avg.w.row.1995.2005) 
Cor.pearson.w.row.spear.1995.2005.w.test<-Cor.pearson.w.row.spear.1995.2005
Cor.pearson.w.row.spear.1995.2005<-cbind(Cor.pearson.w.row.spear.1995.2005,Cor.test.pear.w.row)
Cor.pearson.w.row.spear.1995.2005<-cbind(Cor.pearson.w.row.spear.1995.2005,Cor.test.spear.w.row)



#1995 centrality RCA
pearson.w.row.centr<-diag(cor(centrality.1995.va.w.row.cor,ricardo.va.1995.cor,method="pearson"))
spearman.centr<-diag(cor(centrality.1995.va.w.row.cor,ricardo.va.1995.cor,method="spearman"))
Cor.pearson.w.row.spear.centr<-cbind(pearson.w.row.centr,spearman.centr)

Cor.test.pear.centr.w.row<-matrix(0, nrow = 20, ncol = 1)
Cor.test.spear.centr.w.row<-matrix(0, nrow = 20, ncol = 1)

for (i in 1: ncol(ricardo.va.1995.cor)) {
  Cor.test.pear.centr.w.row[i,1]<-as.vector(cor.test(as.vector(centrality.1995.va.w.row.cor[,i]), as.vector(ricardo.va.1995.cor[,i]),method = c("pearson"))[[3]])
}
for (i in 1: ncol(ricardo.va.1995.cor)) {
  Cor.test.spear.centr.w.row[i,1]<-as.vector(cor.test(as.vector(centrality.1995.va.w.row.cor[,i]), as.vector(ricardo.va.1995.cor[,i]),method = c("spearman"))[[3]])
}

colnames(Cor.test.pear.centr.w.row)<-c('p-val pearson')

colnames(Cor.test.spear.centr.w.row)<-c('p-val spearman')
Cor.test.pear.centr.w.row<-rbind(Cor.test.pear.centr.w.row,null)
Cor.test.spear.centr.w.row<-rbind(Cor.test.spear.centr.w.row,null)

Cor.test.pear.centr.w.row<-rbind(Cor.test.pear.centr.w.row,null)
Cor.test.spear.centr.w.row<-rbind(Cor.test.spear.centr.w.row,null)
avg.pear.w.row.centr<-mean(diag(cor(centrality.1995.va.w.row.cor,ricardo.va.1995.cor)))
avg.spear.w.row.centr<-mean(diag(cor(centrality.1995.va.w.row.cor,ricardo.va.1995.cor,method="spearman")))
med.pear.centr<-median(diag(cor(centrality.1995.va.w.row.cor,ricardo.va.1995.cor)))
med.spear.centr<-median(diag(cor(centrality.1995.va.w.row.cor,ricardo.va.1995.cor,method="spearman")))
median.w.row.centr<-cbind(med.pear.centr,med.spear.centr)
rownames(median.w.row.centr)<-c("median")
avg.w.row.centr<-cbind(avg.pear.w.row.centr,avg.spear.w.row.centr)
rownames(avg.w.row.centr)<-c("AVG")
Cor.pearson.w.row.spear.centr<-rbind(Cor.pearson.w.row.spear.centr,avg.w.row.centr) 
Cor.pearson.w.row.spear.centr<-rbind(Cor.pearson.w.row.spear.centr,median.w.row.centr)
Cor.pearson.w.row.spear.centr.w.test<-Cor.pearson.w.row.spear.centr
Cor.pearson.w.row.spear.centr<-cbind(Cor.pearson.w.row.spear.centr,Cor.test.pear.centr.w.row)
Cor.pearson.w.row.spear.centr<-cbind(Cor.pearson.w.row.spear.centr,Cor.test.spear.centr.w.row)

xtable(Cor.pearson.w.row.spear)
row.w.row<-cbind(Cor.pearson.spear.w.test,Cor.pearson.spear.centr.w.test)
row.w.row<-cbind(row.w.row,Cor.pearson.w.row.spear.w.test )
colnames(row.w.row)<-c("2005.Pearson","2005.Spearman","1995.Pearson","1995.Spearman","w.row.Pearson","w.row.Spearman")
xtable(as.data.frame(row.w.row))

xtable(Cor.pearson.w.row.spear.exgr.va)
exgr.va.row.w.row<-cbind(Cor.pearson.spear.exgr.va.w.test,Cor.pearson.spear.1995.2005.w.test)
exgr.va.row.w.row<-cbind(exgr.va.row.w.row,Cor.pearson.w.row.spear.exgr.va.w.test)
xtable(as.data.frame(exgr.va.row.w.row))
#xtable(Cor.pearson.w.row.spear.1995.2005)

#xtable(Cor.pearson.w.row.spear.centr)

#repeat procedure for estimation sample and construct correlation table with estimation and full sample next to each other

setwd("/Users/sergej/Documents/Master/Thesis/June_Empirical_Analysis/Send_liza/03_08_small_sample")
results.small<-read.dta("results_31_07_2015_small.dta")

results.small$IND<-gsub("T","-", results.small$IND) 

prod.2005.small<-results.small[, c("COU", "IND", "revealed_productivity_2005")]
prod.2005.small.exgr<-results.small[, c("COU", "IND","revealed_productivity_exgr_2005")]
prod.1995.small<-results.small[, c("COU", "IND","revealed_productivity_1995" )]

in.centrality.va.2005.small<-results.small[, c("COU", "IND","in_centrality_VA_2005")]
in.centrality.va.1995.small<-results.small[, c("COU", "IND","in_centrality_VA_1995")]

prod.2005.small.wide<-dcast(prod.2005.small, COU ~ IND, value.var="revealed_productivity_2005")
prod.2005.small.exgr.wide<-dcast(prod.2005.small.exgr, COU ~ IND, value.var="revealed_productivity_exgr_2005")
prod.1995.small.wide<-dcast(prod.1995.small , COU ~ IND, value.var="revealed_productivity_1995" )

in.centrality.va.2005.small.wide<-dcast(in.centrality.va.2005.small,COU ~ IND, value.var="in_centrality_VA_2005")
in.centrality.va.1995.small.wide<-dcast(in.centrality.va.1995.small, COU ~ IND, value.var="in_centrality_VA_1995")

results.small<-subset(results.small,  results.small$IND %nin% bench.industry) 
results.small<-subset(results.small,  results.small$COU %nin% benchmark2) 

in.norm.centrality.va.small.2005<-results.small[, c("COU", "IND","in_centrality_normalized_2005")]
in.norm.centrality.va.small.1995<-results.small[, c("COU", "IND","in_centrality_normalized_1995")]

norm.prod.va.small.2005<-results.small[, c("COU", "IND","sector")]
norm.prod.va.small.1995<-results.small[, c("COU", "IND","sector_VA_1995")]
norm.prod.exgr.small.2005<-results.small[, c("COU", "IND","sector_exgr")]
norm.prod.exgr.small.1995<-results.small[, c("COU", "IND","sector_exgr_1995")]

in.norm.centrality.va.small.2005.wide<-dcast(in.norm.centrality.va.small.2005, COU ~ IND, value.var="in_centrality_normalized_2005")
in.norm.centrality.va.small.1995.wide<-dcast(in.norm.centrality.va.small.1995, COU ~ IND, value.var="in_centrality_normalized_1995" )

norm.prod.va.small.2005.wide<-dcast(norm.prod.va.small.2005, COU ~IND, value.var="sector")
norm.prod.va.small.1995.wide<-dcast(norm.prod.va.small.1995, COU ~IND, value.var="sector_VA_1995")
norm.prod.exgr.small.2005.wide<-dcast(norm.prod.exgr.small.2005, COU ~IND, value.var="sector_exgr")




#correlation matrixes
in.centrality.va.small.cor<-(in.centrality.va.2005.small.wide[,2:ncol(in.centrality.va.2005.small.wide)])
centrality.va.small.cor<-(in.norm.centrality.va.small.2005.wide[,2:ncol(in.norm.centrality.va.small.2005.wide)])
ricardo.va.2005.small.cor<- (norm.prod.va.small.2005.wide[,2:ncol(norm.prod.va.small.2005.wide)])
ricardo.va.1995.cor<- (norm.prod.va.small.1995.wide[,2:ncol(norm.prod.va.small.1995.wide)])
ricardo.exgr.2005.small.cor<-(norm.prod.exgr.small.2005.wide[,2:ncol(norm.prod.exgr.small.2005.wide)])
ricardo.exgr.1995.small.cor<-(norm.prod.exgr.small.1995.wide[,2:ncol(norm.prod.exgr.small.1995.wide)])
centrality.1995.va.small.cor<-(in.norm.centrality.va.small.1995.wide[,2:ncol(in.norm.centrality.va.small.1995.wide)])

#correlation
pearson.small<-diag(cor(ricardo.va.2005.small.cor,centrality.va.small.cor,method="pearson"))
spearman.small<-diag(cor(ricardo.va.2005.small.cor,centrality.va.small.cor,method="spearman"))
Cor.pearson.small.spear<-cbind(pearson.small,spearman.small)

Cor.test.pear.centr.small.ricardo<-matrix(0, nrow = 21, ncol = 1)
Cor.test.spear.centr.small.ricardo<-matrix(0, nrow = 21, ncol = 1)

for (i in 1: ncol(centrality.va.small.cor)) {
  Cor.test.pear.centr.small.ricardo[i,1]<-as.vector(cor.test(as.vector(ricardo.va.2005.small.cor[,i]), as.vector(centrality.va.small.cor[,i]),method = c("pearson"))[[3]])
}
for (i in 1: ncol(centrality.va.small.cor)) {
  Cor.test.spear.centr.small.ricardo[i,1]<-as.vector(cor.test(as.vector(ricardo.va.2005.small.cor[,i]), as.vector(centrality.va.small.cor[,i]),method = c("spearman"))[[3]])
}

colnames(Cor.test.pear.centr.small.ricardo)<-c('p-val pearson')

colnames(Cor.test.spear.centr.small.ricardo)<-c('p-val spearman')

null<-c(0)
Cor.test.pear.centr.small.ricardo<-rbind(Cor.test.pear.centr.small.ricardo,null)

Cor.test.spear.centr.small.ricardo<-rbind(Cor.test.spear.centr.small.ricardo,null)


avg.pear.small<-mean(diag(cor(ricardo.va.2005.small.cor,centrality.va.small.cor,method="pearson")))
avg.spear.small<-mean(diag(cor(ricardo.va.2005.small.cor,centrality.va.small.cor,method="spearman")))
med.pear.small<-median(diag(cor(ricardo.va.2005.small.cor,centrality.va.small.cor,method="pearson")))
med.spear.small<-median(diag(cor(ricardo.va.2005.small.cor,centrality.va.small.cor,method="spearman")))
median.small<-cbind(med.pear.small,med.spear.small)
rownames(median.small)<-c("median")
avg.small<-cbind(avg.pear.small,avg.spear.small)
rownames(avg.small)<-c("AVG")
Cor.pearson.small.spear<-rbind(Cor.pearson.small.spear,avg.small) 
Cor.pearson.small.spear<-rbind(Cor.pearson.small.spear,median.small)
Cor.pearson.small.spear.w.test<-Cor.pearson.small.spear
Cor.pearson.small.spear<-cbind(Cor.pearson.small.spear,Cor.test.pear.centr.small.ricardo)
Cor.pearson.small.spear<-cbind(Cor.pearson.small.spear,Cor.test.spear.centr.small.ricardo)

#VA 2005 EXGR2005
pearson.small.exgr.va<-diag(cor(ricardo.va.2005.small.cor,ricardo.exgr.2005.small.cor,method="pearson"))
spearman.exgr.small.va<-diag(cor(ricardo.va.2005.small.cor,ricardo.exgr.2005.small.cor,method="spearman"))
Cor.pearson.small.spear.exgr.va<-cbind(pearson.small.exgr.va,spearman.exgr.small.va)
Cor.test.pear.exgr.small.va.ricardo<-matrix(0, nrow = 20, ncol = 1)
Cor.test.spear.exgr.small.va.ricardo<-matrix(0, nrow = 20, ncol = 1)

for (i in 1: ncol(centrality.va.small.cor)) {
  Cor.test.pear.exgr.small.va.ricardo[i,1]<-as.vector(cor.test(as.vector(ricardo.va.2005.small.cor[,i]), as.vector(ricardo.exgr.2005.small.cor[,i]),method = c("pearson"))[[3]])
}
for (i in 1: ncol(centrality.va.small.cor)) {
  Cor.test.spear.exgr.small.va.ricardo[i,1]<-as.vector(cor.test(as.vector(ricardo.va.2005.small.cor[,i]), as.vector(ricardo.exgr.2005.small.cor[,i]),method = c("spearman"))[[3]])
}

colnames(Cor.test.pear.exgr.small.va.ricardo)<-c('p-val pearson')

colnames(Cor.test.spear.exgr.small.va.ricardo)<-c('p-val spearman')


Cor.test.pear.exgr.small.va.ricardo<-rbind(Cor.test.pear.exgr.small.va.ricardo,null)
Cor.test.spear.exgr.small.va.ricardo<-rbind(Cor.test.spear.exgr.small.va.ricardo,null)


avg.pear.small.exgr.va<-mean(diag(cor(ricardo.va.2005.small.cor,ricardo.exgr.2005.small.cor,method="pearson")))
avg.spear.small.exgr.va<-mean(diag(cor(ricardo.va.2005.small.cor,ricardo.exgr.2005.small.cor,method="spearman")))
avg.small.exgr.va<-cbind(avg.pear.small.exgr.va,avg.spear.small.exgr.va)
rownames(avg.small.exgr.va)<-c("AVG")
Cor.pearson.small.spear.exgr.va<-rbind(Cor.pearson.small.spear.exgr.va,avg.small.exgr.va)
Cor.pearson.small.spear.exgr.va.w.test<-Cor.pearson.small.spear.exgr.va
Cor.pearson.small.spear.exgr.va<-cbind(Cor.pearson.small.spear.exgr.va,Cor.test.pear.exgr.small.va.ricardo)
Cor.pearson.small.spear.exgr.va<-cbind(Cor.pearson.small.spear.exgr.va,Cor.test.spear.exgr.small.va.ricardo)



#1995 2005 Ricardo
pearson.small.va.1995.2005<-diag(cor(ricardo.va.2005.small.cor,ricardo.va.1995.cor,method="pearson"))


Cor.test.pear.small<-matrix(0, nrow = 20, ncol = 1)
Cor.test.spear.small<-matrix(0, nrow = 20, ncol = 1)

for (i in 1: ncol(centrality.va.small.cor)) {
  Cor.test.pear.small[i,1]<-as.vector(cor.test(as.vector(ricardo.va.2005.small.cor[,i]), as.vector(ricardo.va.1995.cor[,i]),method = c("pearson"))[[3]])
}
for (i in 1: ncol(centrality.va.small.cor)) {
  Cor.test.spear.small[i,1]<-as.vector(cor.test(as.vector(ricardo.va.2005.small.cor[,i]), as.vector(ricardo.va.1995.cor[,i]),method = c("spearman"))[[3]])
}

colnames(Cor.test.pear.small)<-c('p-val pearson')

colnames(Cor.test.spear.small)<-c('p-val spearman')

Cor.test.pear.small<-rbind(Cor.test.pear.small,null)
Cor.test.spear.small<-rbind(Cor.test.spear.small,null)

spearman.va.1995.2005<-diag(cor(ricardo.va.2005.small.cor,ricardo.va.1995.cor,method="spearman"))
Cor.pearson.small.spear.1995.2005<-cbind(pearson.small.va.1995.2005,spearman.va.1995.2005)

avg.pear.small.1995.2005<-mean(diag(cor(ricardo.va.2005.small.cor,ricardo.va.1995.cor,method="pearson")))
avg.spear.small.1995.2005<-mean(diag(cor(ricardo.va.2005.small.cor,ricardo.va.1995.cor,method="spearman")))
avg.small.1995.2005<-cbind(avg.pear.small.1995.2005,avg.spear.small.1995.2005)
rownames(avg.small.1995.2005)<-c("AVG")
Cor.pearson.small.spear.1995.2005<-rbind(Cor.pearson.small.spear.1995.2005,avg.small.1995.2005) 

Cor.pearson.small.spear.1995.2005<-cbind(Cor.pearson.small.spear.1995.2005,Cor.test.pear.small)
Cor.pearson.small.spear.1995.2005<-cbind(Cor.pearson.small.spear.1995.2005,Cor.test.spear.small)



#1995 2005 centrality 
pearson.small.centr<-diag(cor(centrality.1995.va.small.cor,centrality.va.small.cor,method="pearson"))
spearman.centr<-diag(cor(centrality.1995.va.small.cor,centrality.va.small.cor,method="spearman"))
Cor.pearson.small.spear.centr<-cbind(pearson.small.centr,spearman.centr)

Cor.test.pear.centr.small<-matrix(0, nrow = 20, ncol = 1)
Cor.test.spear.centr.small<-matrix(0, nrow = 20, ncol = 1)

for (i in 1: ncol(centrality.va.small.cor)) {
  Cor.test.pear.centr.small[i,1]<-as.vector(cor.test(as.vector(centrality.1995.va.small.cor[,i]), as.vector(centrality.va.small.cor[,i]),method = c("pearson"))[[3]])
}
for (i in 1: ncol(centrality.va.small.cor)) {
  Cor.test.spear.centr.small[i,1]<-as.vector(cor.test(as.vector(centrality.1995.va.small.cor[,i]), as.vector(centrality.va.small.cor[,i]),method = c("spearman"))[[3]])
}

colnames(Cor.test.pear.centr.small)<-c('p-val pearson')

colnames(Cor.test.spear.centr.small)<-c('p-val spearman')
Cor.test.pear.centr.small<-rbind(Cor.test.pear.centr.small,null)
Cor.test.spear.centr.small<-rbind(Cor.test.spear.centr.small,null)

Cor.test.pear.centr.small<-rbind(Cor.test.pear.centr.small,null)
Cor.test.spear.centr.small<-rbind(Cor.test.spear.centr.small,null)
avg.pear.small.centr<-mean(diag(cor(centrality.1995.va.small.cor,centrality.va.small.cor)))
avg.spear.small.centr<-mean(diag(cor(centrality.1995.va.small.cor,centrality.va.small.cor,method="spearman")))
med.pear.centr<-median(diag(cor(centrality.1995.va.small.cor,centrality.va.small.cor)))
med.spear.centr<-median(diag(cor(centrality.1995.va.small.cor,centrality.va.small.cor,method="spearman")))
median.small.centr<-cbind(med.pear.centr,med.spear.centr)
rownames(median.small.centr)<-c("median")
avg.small.centr<-cbind(avg.pear.small.centr,avg.spear.small.centr)
rownames(avg.small.centr)<-c("AVG")
Cor.pearson.small.spear.centr<-rbind(Cor.pearson.small.spear.centr,avg.small.centr) 
Cor.pearson.small.spear.centr<-rbind(Cor.pearson.small.spear.centr,median.small.centr)

Cor.pearson.small.spear.centr<-cbind(Cor.pearson.small.spear.centr,Cor.test.pear.centr.small)
Cor.pearson.small.spear.centr<-cbind(Cor.pearson.small.spear.centr,Cor.test.spear.centr.small)

xtable(Cor.pearson.small.spear)
row.small<-cbind(Cor.pearson.spear.w.test,Cor.pearson.small.spear.w.test)
row.all<-cbind(row.w.row,Cor.pearson.small.spear.w.test)
xtable(as.data.frame(row.all))

xtable(Cor.pearson.small.spear.exgr.va)
exgr.va.row.small<-cbind(Cor.pearson.spear.exgr.va.w.test,Cor.pearson.small.spear.exgr.va.w.test)
exgr.va.row.all<-cbind(exgr.va.row.w.row, Cor.pearson.small.spear.exgr.va.w.test)
xtable(as.data.frame(exgr.va.row.all))
#xtable(Cor.pearson.small.spear.1995.2005)

#xtable(Cor.pearson.small.spear.centr)
