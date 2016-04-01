#Creating parwise graph of RCA for GER and BEL
library(reshape2)
library(car)
library(qpcR)
library(xtable)
library(Hmisc)
library(plyr)
library(foreign)
library(tikzDevice)
library(dplyr)
setwd("/Users/sergej/Google Drive/Send_liza/results")
#different theta estimates for EXGR and FDDVA 
results.raw<-read.dta("results_march_raw.dta")
results.raw$IND<-gsub("T","-", results.raw$IND)
#exclude<-cbind("01-05","10-14","45", "55","70-74" ,"75-95")


#results.raw.2<- subset(results.raw,IND %nin% exclude)
results.raw.dva<-subset(results.raw, select=c("z_dva_std","COU","IND"))
results.raw.fddva<-subset(results.raw, select=c("z_fddva_std","COU","IND"))
results.raw.exgr<-subset(results.raw, select=c("z_exgr_std","COU","IND"))

z.wide.dva<-dcast(results.raw.dva,IND ~ COU, value.var=c("z_dva_std"))

z.wide.fd_dva<-dcast(results.raw.fddva,IND ~ COU, value.var=c("z_fddva_std"))
z.wide.exgr<-dcast(results.raw.exgr,IND ~ COU, value.var=c("z_exgr_std"))
colnames(z.wide.dva) <- paste("DVA_std", colnames(z.wide.dva), sep = "_")
colnames(z.wide.fd_dva) <- paste("FDDVA_std", colnames(z.wide.fd_dva), sep = "_")
colnames(z.wide.exgr) <- paste("EXGR_std", colnames(z.wide.exgr), sep = "_")
names(z.wide.exgr)[1]<-"IND"
names(z.wide.fd_dva)[1]<-"IND"
names(z.wide.dva)[1]<-"IND"
z.wide.exgr.fddva.dva<-merge(z.wide.fd_dva,z.wide.dva, by=("IND"))
z.wide.exgr.fddva.dva<-merge(z.wide.exgr.fddva.dva,z.wide.exgr, by=("IND"))
dat.m <- melt(z.wide.exgr.fddva.dva,id.vars='IND', measure.vars=c('FDDVA_std_BEL','FDDVA_std_DEU','DVA_std_BEL','DVA_std_DEU'))

dat.m.wide<-melt(dat.m.wide,id.vars='IND')
dat.m$variable<-revalue(dat.m$variable, c('FDDVA_std_DEU'="Forw. VAX Germany", "FDDVA_std_BEL"="Forw. VAX Belgium", "DVA_std_DEU"="Back. VAX Germany", "DVA_std_BEL"="Back. VAX Belgium"))
dat.m.2 <- melt(z.wide.exgr.fddva.dva,id.vars='IND', measure.vars=c('FDDVA_std_BEL','FDDVA_std_DEU','EXGR_std_BEL','EXGR_std_DEU'))
dat.m.2$variable<-revalue(dat.m.2$variable, c('FDDVA_std_BEL'="Forw. VAX Belgium", "FDDVA_std_DEU"="Forw. VAX DEU", "EXGR_std_BEL"="EXGR Belgium", "EXGR_std_DEU"="EXGR DEU"))
dat.m.1 <- melt(z.wide.exgr.fddva.dva,id.vars='IND', measure.vars=c('DVA_std_BEL','DVA_std_DEU','EXGR_std_BEL','EXGR_std_DEU'))
dat.m.1$variable<-revalue(dat.m.1$variable, c("DVA_std_BEL"="Back. VAX Belgium", "DVA_std_DEU"="Back. VAX Germany", "EXGR_std_BEL"="EXGR Belgium", "EXGR_std_DEU"="EXGR Germany"))
library(ggplot2)


colors.vis<-("#50AC69","#B02985","#78A619","#C50138","#1C7FA7", "#BD922F")
exclude.industry<-c("45",    "50-52", "55"  ,  "60-64", "65-67", "70-74", "75-95")
dat.plot<-subset(dat.m,dat.m$IND %nin% exclude.industry)

dat.plot$shapes_values<-revalue(dat.plot$variable, c("Forw. VAX Belgium"="Forw.","Forw. VAX Germany"="Forw.", "Back. VAX Belgium"="Backw.","Back. VAX Germany"="Backw.") )
dat.plot$variable<- factor(dat.plot$variable,levels=c("Forw. VAX Belgium", "Back. VAX Belgium","Forw. VAX Germany",  "Back. VAX Germany"))
shapes_values<-c(1,2,1,2)

p.1 <- ggplot(dat.plot, aes(x=IND, y=value, group=variable,shape = factor(variable),color=factor(variable))) +
  geom_point( size = 2)  +
  geom_line()+
  coord_cartesian(ylim = c(0.8, 1.2))+
  scale_shape_manual(name = "",labels =c("Forw. VAX BEL","Backw. VAX BEL","Forw. VAX DEU","Backw. VAX DEU"),  values=shapes_values) +
  scale_color_manual(name = "",labels =c("Forw. VAX BEL","Backw. VAX BEL","Forw. VAX DEU","Backw. VAX DEU"), values=brewer.1)+ 
  ggtitle(expression(atop("Structural RCA across industries for Belgium and Germany III")))+
  labs(x="Industry") +
  scale_y_continuous("Norm. RCA",limits=c(0.8,1.2))+
  coord_cartesian( )+
  theme_bw() + 
  theme( legend.title=element_blank(),legend.position = "bottom", # legend location in graph
         panel.grid.minor = element_blank(),
         axis.title=element_text( size="10"),
         axis.text.x=element_text(angle = 0, size=10))
brewer<-c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33')
 #forw. bel, backw. bel, forw. vx deu, backw. deu
brewer.1<-c('#2b6391','#4daf4a','#e41a1c','#ff7f00' )
#"Forw. VAX Belgium","EXGR Belgium","Forw. VAX Germany","EXGR Germany"
brewer.2<-c('#2b6391', "#cdcd00", '#e41a1c','#984ea3')


brewer.3<-c('#4daf4a', "#cdcd00", '#ff7f00', '#984ea3')
dat.plot<-subset(dat.m,dat.m$IND %nin% exclude.industry)

dat.plot$shapes_values<-revalue(dat.plot$variable, c("Forw. VAX Belgium"="Forw.","Forw. VAX Germany"="Forw.", "Back. VAX Belgium"="Backw.","Back. VAX Germany"="Backw.") )
dat.plot$variable<- factor(dat.plot$variable,levels=c("Forw. VAX Belgium", "Back. VAX Belgium","Forw. VAX Germany",  "Back. VAX Germany"))
shapes_values<-c(1,2,1,2)

plot.2.sub<-subset(dat.m.2,dat.m$IND %nin% c("45","50-52","55","60-64","65-67","70-74","75-95"))
plot.3.sub<-subset(dat.m.1,dat.m$IND %nin% c("45","50-52","55","60-64","65-67","70-74","75-95"))
tableau.2.color<-c("#1F77B4", "#B25C9A",  "#D62728", "#DDCB00")
plot.2.sub$variable<- factor(plot.2.sub$variable,levels=c("Forw. VAX Belgium", "EXGR Belgium","Forw. VAX DEU",  "EXGR DEU"))
p.2 <- ggplot(plot.2.sub, aes(x=IND, y=value)) +
  geom_line(aes(colour=factor(variable), group=factor(variable),shape=factor(variable))) +
  geom_point(aes(colour=factor(variable), group=factor(variable), shape=factor(variable)))  +
  coord_cartesian(ylim = c(0.8, 1.2))+
  scale_shape_manual(name = "",labels =c("Forw. VAX Belgium","EXGR Belgium","Forw. VAX Germany","EXGR Germany"),  values=shapes_values) +
  scale_color_manual(name = "",labels =c("Forw. VAX Belgium","EXGR Belgium","Forw. VAX Germany","EXGR Germany"), values=c('#4daf4a','#2b6391','#ff7f00','#7F2627' ))+ 
  ggtitle(expression(atop("Structural RCA across industries for Belgium and Germany I")))+
  labs(x="IND",y="RCA") +
  theme_bw() + 
  theme(legend.title=element_blank(), legend.position = "bottom", # legend location in graph
         panel.grid.minor = element_blank(),
         axis.title=element_text( size="10"),
         axis.text.x=element_text(angle = 0, size=10))
plot.3.sub$variable<-factor(plot.3.sub$variable,levels=c("Back. VAX Belgium","EXGR Belgium", "Back. VAX Germany","EXGR Germany"))
p.3 <- ggplot(plot.3.sub, aes(x=IND, y=value)) +
  geom_line(aes(colour=factor(variable), group=factor(variable),shape=factor(variable))) +
  geom_point(aes(colour=factor(variable), group=factor(variable), shape=factor(variable)))  +
  ggtitle(expression(atop("Structural RCA across industries for Beglium and Germany II")))+
  labs(x="IND",y="RCA") +
  theme_bw() + 
  scale_shape_manual(name = "",labels =c("Backw. VAX Belgium","EXGR Belgium","Backw. VAX Germany","EXGR Germany"),  values=shapes_values) +
  scale_color_manual(name = "",labels =c("Backw. VAX Belgium","EXGR Belgium","Backw. VAX Germany","EXGR Germany"), values=c('#B26D00','#2b6391','#C5CC14','#7F2627'))+ 
  coord_cartesian(ylim = c(0.8, 1.2))+
  theme( legend.title=element_blank(),
legend.position = "bottom", # legend location in graph
         panel.grid.minor = element_blank(),
         axis.title=element_text( size="10"),
         axis.text.x=element_text(angle = 0, size=10))

tikz(file = "/Users/sergej/Documents/Thesis/fig/forw_back_DEU_BEL_tiva.tex")
p.1
dev.off()

tikz(file = "/Users/sergej/Documents/Thesis/fig/forw_exgr_DEU_BEL_tiva.tex")
p.2
dev.off()

tikz(file = "/Users/sergej/Documents/Thesis/fig/back_exgr_DEU_BEL_tiva.tex")
p.3
dev.off()
