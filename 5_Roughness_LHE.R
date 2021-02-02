suppressMessages(library(lme4))
suppressMessages(library(lmerTest))
suppressMessages(library(emmeans))

X<-as.data.frame(read.csv("FULL_Data_munged.csv"))

X$Congruence<-as.factor(ifelse(X$PrimeValence==X$TargetValence,"C","I")) #we'll look simply at congruent vs not congruent. no point in full factorial
Delta_R<-c("L","L","L","L","H","H","H","H","E","E") 
 Interval<-c("m3M3","m6M6","ttP5","M7P5","m2P5","m2M3","M2P5","m2tt","d2P5","s2S5")
  Y<-cbind(Delta_R,Interval)
  X<-merge(X,Y,by.x="Intervals",by.y="Interval")
  X$RTsec<-0.001*X$RT
  
#############################
#First check for congruency effetcs in L,H and E roughness conditions
  
  model<-glmer(data=X,RTsec~ Delta_R*Congruence+(1|Part),family=Gamma(link="inverse"))
  E<-c(-1,0,0,1,0,0)
  H<-c(0,-1,0,0,1,0)
  L<-c(0,0,-1,0,0,1)
  dX<-contrast(emmeans(model,~Delta_R*Congruence),adjust="bonferroni",list(E,H,L))
dX
 
################################

#Now test (E + H) vs L


contrast1<-c(1,1,-2)
contrast2<-c(1,-1,0)

RT_per_condition<-dplyr::summarise(dplyr::group_by(X,Intervals,Part,Congruence),RT=mean(RT),.groups = 'drop')
RT_per_condition$multiplier<-ifelse(RT_per_condition$Congruence=="C",-1,1)
cortable<-dplyr::summarise(dplyr::group_by(RT_per_condition,Intervals,Part),index=sum(RT*multiplier),.groups = 'drop')
deltarough<-as.data.frame(c(0.830,0.536,0.475,0.065,0.080,0.103,0.111,0.923,0.348,0.247))
deltarough$pair<-c("d2P5","m2P5","m2tt","m3M3","m6M6","M7P5","ttP5","s2S5","m2M3","M2P5")
deltarough$size<-c("E","H","H","L","L","L","L","E","H","H")
df<-merge(cortable,deltarough,by.x="Intervals",by.y="pair")
colnames(df)<-c("Intervals","Participant","Index","deltarough","Delta_R")
df$Delta_R<-factor(df$Delta_R)

C<-lm(data=df,Index~Delta_R)

contrast(emmeans(C,~Delta_R),adjust="bonferroni",list(contrast1,contrast2))
print(knitr::kable(dplyr::summarise(dplyr::group_by(df,Delta_R),I=mean(Index),SDI=sd(Index),.groups = 'drop')))



