# Figure3.R
# T. Eerola, 22 May 2020

## SET PATH AND LIBRARIES ------------------------
suppressMessages(library(dplyr))
library(ggplot2)
library(Rmisc)
#library(tidyverse)
library(ggrepel)

## LOAD RAW DATA ------------------------
intervalspriming3<-read.csv('FULL_Data_munged.csv')

## CALCULATE PRIMING INDEX AND ADD LABEL ------------------------
RT_per_condition<-dplyr::summarise(group_by(intervalspriming3,Intervals,Part,PrimeValence,TargetValence),RT=mean((RT)),.groups = 'drop') # was log
RT_per_condition$multiplier<-ifelse(RT_per_condition$PrimeValence==RT_per_condition$TargetValence,-1,1)
cortable<-dplyr::summarise(group_by(RT_per_condition,Intervals,Part),index=sum(RT*multiplier),.groups = 'drop')
deltarough<-as.data.frame(c(6.05,4.05,3.28,0.45,0.55,0.71,0.82,7.05,2.40,1.71))
deltarough$pair<-c("d2P5","m2P5","m2tt","m3M3","m6M6","M7P5","ttP5","s2S5","m2M3","M2P5")
df<-merge(cortable,deltarough,by.x="Intervals",by.y="pair")
colnames(df)<-c("Intervals","Participant","Index","deltarough")

df$deltarough<-factor(df$deltarough)
df$deltaroughC = as.numeric(as.character(df$deltarough))
df$Intervals<-factor(df$Intervals)

## SUMMARISE FOR PAIRS ------------------------
S0 <- summarySE(df, measurevar = "Index",groupvars = "deltaroughC",conf.interval = 0.90)

## ADD LABELS ------------------------
S0$Intervals<-''
S0$Intervals<-c('m3M3','m6M6','M7P5','ttP5','M2P5','m2M3','m2tt','m2P5','d2P5','s2S5')
S0$graphcolour<-c(1,1,1,1,1,2,2,2,3,3)
S0$graphcolour<-factor(S0$graphcolour)

meanpoints<-dplyr::summarise(group_by(S0,graphcolour),M=median(deltaroughC),.groups = 'drop')
meanpoints$M[1]<-S0$deltaroughC[1]+(S0$deltaroughC[5]-S0$deltaroughC[1])/2

## SET GRAPHICS PARAMS ------------------------
plsize <- 0.40 # errorbar line width
theme_fs <- function(fs=18){
  tt <- theme(axis.text = element_text(size=fs-1, colour=NULL)) + 
    theme(legend.text = element_text(size=fs, colour=NULL)) + 
    theme(legend.title = element_text(size=fs, colour=NULL)) + 
    theme(axis.title = element_text(size=fs, colour=NULL)) + 
    theme(legend.text = element_text(size=fs, colour=NULL))
  return <- tt
}
custom_theme_size <- theme_fs(14)
pd <- position_dodge(.6) 

## PLOT ------------------------
g3<-ggplot(S0,aes(x=deltaroughC,y=Index,group=Intervals,colour=graphcolour,linetype=graphcolour,shape=graphcolour))+
  geom_point(show.legend = FALSE,size=3)+
  geom_errorbar(S0, mapping=aes(x=deltaroughC, ymin=Index-ci, ymax=Index+ci), width=0.1, size=plsize,show.legend = FALSE,alpha=1.0)+
  ylab('Priming Index (ms) ± 90% CI')+
  xlab('Delta Roughness')+
  scale_y_continuous(limits = c(-40,80),breaks = seq(-40,80,by=20))+
  scale_x_continuous(limits = c(0,7.5),breaks = seq(0,7.5,by=1),expand = c(0.001,0.001))+
  scale_color_manual(values = c('red4','mediumblue','darkgreen'))+
  scale_shape_manual(values = c(15,16,17))+
  annotate('segment',x = 0,xend=7.5,y=0,yend=0,colour='gray40')+  
  geom_label_repel(x = as.numeric(S0$deltaroughC),y=S0$Index,label=S0$Intervals,size=3.5,segment.size = 0.5,box.padding = 0.75,family="Arial")+  
  theme_bw()+
  annotate("segment",x = S0$deltaroughC[1],xend = S0$deltaroughC[5],y=50,yend = 50,size=1.25,colour='gray50')+
  annotate("segment",x = S0$deltaroughC[6],xend = S0$deltaroughC[8],y=50,yend = 50,size=1.25,colour='gray50')+
  annotate("segment",x = S0$deltaroughC[9],xend = S0$deltaroughC[10],y=50,yend = 50,size=1.25,colour='gray50')+
  annotate("text",x = meanpoints$M[1],y=53.5,label="Low",size=3,family="Arial")+
  annotate("text",x = meanpoints$M[2],y=53.5,label="High",size=3,family="Arial")+
  annotate("text",x = meanpoints$M[3],y=53.5,label="Extreme",size=3,family="Arial")+
  annotate("segment",x = meanpoints$M[1],xend = meanpoints$M[2],y=58+8,yend = 58+8,size=0.25)+
  annotate("segment",x = meanpoints$M[1],xend = meanpoints$M[1],y=58+8,yend = 56+8,size=0.25)+
  annotate("segment",x = meanpoints$M[2],xend = meanpoints$M[2],y=58+8,yend = 56+8,size=0.25)+
  annotate("text",x = (meanpoints$M[1]+meanpoints$M[2])/2,y=60+8,label="italic(p)<.05",parse = TRUE,size=2.5,family="Arial")+
  annotate("segment",x = meanpoints$M[2],xend = meanpoints$M[3],y=62+8,yend = 62+8,size=0.25)+
  annotate("segment",x = meanpoints$M[2],xend = meanpoints$M[2],y=62+8,yend = 60+8,size=0.25)+
  annotate("segment",x = meanpoints$M[3],xend = meanpoints$M[3],y=62+8,yend = 60+8,size=0.25)+
  annotate("text",x = (meanpoints$M[2]+meanpoints$M[3])/2,y=64+8,label="italic(ns)",parse = TRUE,size=2.5,family="Arial")+
  annotate("segment",x = meanpoints$M[1],xend = meanpoints$M[3],y=66+8,yend = 66+8,size=0.25)+
  annotate("segment",x = meanpoints$M[1],xend = meanpoints$M[1],y=66+8,yend = 64+8,size=0.25)+
  annotate("segment",x = meanpoints$M[3],xend = meanpoints$M[3],y=66+8,yend = 64+8,size=0.25)+
  annotate("text",x = (meanpoints$M[1]+meanpoints$M[3])/2,y=68+8,label="italic(p)<.05",parse = TRUE,size=2.5,family="Arial")+
  theme(legend.position="none")+
  theme(text = element_text(family = "Arial"))
g3

## SAVE ------------------------
#ggsave(filename = 'Figure3.pdf',g1,device = 'pdf',height = 4,width = 6)
print(g3)