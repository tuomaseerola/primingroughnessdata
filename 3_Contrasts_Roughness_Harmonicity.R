#Planned contrasts

#The interaction Delta_R*Congruence was significant: let's probe further
#This script assumes GLMapproach_v2_1.R has been run and model1 and model2 are still in the active environment

library(emmeans)

A<-contrast(emmeans(model1,~Congruence*Delta_R|Delta_H),list(HighDeltaR=c(-1,1,0,0),LowDeltaR=c(0,0,-1,1)),adjust="bonferroni")
B<-contrast(emmeans(model1,~Congruence*Delta_H|Delta_R),list(HighDeltaH=c(-1,1,0,0),LowDeltaH=c(0,0,-1,1)),adjust="bonferroni")
