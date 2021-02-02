
suppressMessages(library(lme4))
suppressMessages(library(dplyr))
suppressMessages(library(MASS))
library(lmerTest)
library(car)
X<-as.data.frame(read.csv("FULL_Data_munged.csv"))
X$Congruence<-as.factor(ifelse(X$PrimeValence==X$TargetValence,"C","I")) #we'll look simply at congruent vs not congruent. no point in full factorial
Delta_R<-c("L","L","L","L","H","H","H","H") 
Delta_H<-c("L","L","H","H","H","L","H","L")
Interval<-c("m3M3","m6M6","ttP5","M7P5","m2P5","m2M3","M2P5","m2tt")
Y<-cbind(Delta_R,Delta_H,Interval)
X<-merge(X,Y,by.x="Intervals",by.y="Interval")

contrasts<-c("contr.sum","contr.poly")

X$RTsec<-X$RT/1000
#GLM models (NB - switch to seconds as lme4 complains about big eigenvalues or determinants or something)
model1<-glmer(data=X,RTsec~Delta_R*Congruence+Delta_H*Congruence+(1|Part),family=Gamma(link="identity"))
model2<-glmer(data=X,RTsec~Delta_R*Congruence+Delta_H*Congruence+(1|Part),family=Gamma(link="log"))
model3<-glmer(data=X,RTsec~Delta_R*Congruence+Delta_H*Congruence+(1|Part),family=Gamma(link="inverse"))

Anova(model3,type="III")
