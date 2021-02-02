# Simple Bar charts to illustrate some contrasts
library(ggplot2)
#library(grid)
#library(gridExtra)
suppressMessages(library(dplyr))

#intervalspriming2<-as.data.frame(read.csv("intervalspriming2.csv"))

X<-as.data.frame(read.csv("FULL_Data_munged.csv"))
X$Congruence<-ifelse(X$PrimeValence==X$TargetValence,"C","I")
Delta_R<-c("L","L","L","L","H","H","H","H","E","E")
Interval<-c("m3M3","m6M6","ttP5","M7P5","m2P5","m2M3","M2P5","m2tt","d2P5","s2S5")
Y<-cbind(Delta_R,Interval)
Z<-merge(X,Y,by.x="Intervals",by.y="Interval")
Z$Delta_R<-as.factor(Z$Delta_R)

W<-summarise(group_by(Z,Delta_R,Congruence),SE=sd(RT)/sqrt(length(RT)),meanRT=mean(RT),.groups = 'drop')

W$Delta_R<-factor(W$Delta_R, levels=c("L","H","E"),labels=c("Low","High","Extreme"))
limits=aes(ymax=meanRT+SE,ymin=meanRT-SE)
grays<-c(I="red",C=hsv(h=206/360,s=0.88,v=0.34,alpha=1))
  g2<-ggplot(W,aes(x=Delta_R,y=meanRT,fill=Congruence))+geom_bar(stat="summary",position="dodge")
g2<-g2+coord_cartesian(ylim=c(660,695))+
  geom_errorbar(limits,width=0.25,size=0.5,position=position_dodge(0.9))+
  scale_fill_manual(values=grays,labels=c("Congruent","Incongruent"))+theme(legend.position ="bottom")+
  ylab("Mean Reaction Time \n ± SE (ms)") +  xlab("Delta Roughness")+theme_bw()
  
Delta_H<-c("L","L","H","H","H","L","L","H")
Interval<-c("m3M3","m6M6","ttP5","M7P5","m2P5","m2M3","M2P5","m2tt")
P<-cbind(Delta_H,Interval)
Q<-merge(X,P,by.x="Intervals",by.y="Interval")
U<-summarise(group_by(Q,Delta_H,Congruence),SE=sd(RT)/sqrt(length(RT)),meanRT=mean(RT),.groups = 'drop')

U$Delta_H<-factor(U$Delta_H, levels=c("L","H"),labels=c("Low","High"))
limits2=aes(ymax=meanRT+SE,ymin=meanRT-SE)
g3<-ggplot(U,aes(x=Delta_H,y=meanRT,fill=Congruence))+geom_bar(stat="summary",position="dodge")
g3<-g3+coord_cartesian(ylim=c(660,695))+
  geom_errorbar(limits2,width=0.25,size=0.5,position=position_dodge(0.9))+
  scale_fill_manual(values=grays,labels=c("Congruent","Incongruent"))+theme(legend.position = "bottom")+
  ylab("Mean Reaction Time \n ± SE (ms)") +  xlab("Delta Harmonicity")+theme_bw()

## save
#ggsave("Figure2.pdf",arrangeGrob(g2,g3,ncol=1,nrow=2))
print(g2)