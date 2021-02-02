#fit gamma function to each participant's RT distribution
#KS test to assess goodness of fit

print("Fit gamma function to each participant's RT distribution")
print("and use Kolmogorov-Smirnow test to assess the goodness of fit")

suppressMessages(library(dplyr))
suppressMessages(library(fitdistrplus))

RTData<-as.data.frame(read.csv("FULL_Data_Raw.csv"))
RTData<-filter(RTData,Correct==1)
A<-function(x){
  Ind_Dist<-filter(RTData,Part==x)
  fit<-fitdist(Ind_Dist$RT,distr="gamma",method="mle")
  KS<-ks.test(Ind_Dist$RT,rgamma(n=500,shape=fit$estimate[[1]],rate=fit$estimate[[2]]))
  B<-as.data.frame(matrix(nrow=1,ncol=2))
  B[1,1]<-paste(Ind_Dist$Part[[1]])
  B[1,2]<-KS[[2]]
  return(B)
}
participants<-unique(RTData$Part)
C<-lapply(participants,A)

D<-as.data.frame(NULL)
for(i in 1:length(C)){D[i,1]<-C[[i]][[1]]}
for(i in 1:length(C)){D[i,2]<-C[[i]][[2]]}
colnames(D)<-c("Participant","p_value")

failures<-sum(D$p_value<0.05)
NOTGAMMA<-filter(D,p_value<0.05)
print(binom.test(failures,n=379,alternative="greater",p=0.05))



