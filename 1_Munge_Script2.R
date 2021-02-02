suppressMessages(library(dplyr))
suppressMessages(library(retimes))
library(fitdistrplus)
intervalspriming2<-read.csv("RT_Data_Raw.csv")
intervalspriming2<-dplyr::filter(intervalspriming2,!Correct==3)
participants<-as.list(unique(intervalspriming2$Part)) #create list of participants (useful for applying munge function to list)
#we're fitting ExGaussian Distribution to each participants data, then excluding outliers 
step_1<-function(x){
step_2<-function(y){
  Z<-filter(intervalspriming2,Part==y)
  
  tdist<-fitdist(Z$RT,distr="gamma",method="mle")
  d<-c(paste(Z$Part[[1]]),quantile(tdist,0.975))
  return(d)}
C<-lapply(participants,step_2)
return(C)}
n_iterations<-c(1:10)
D<-lapply(n_iterations,step_1)

Upper_boundaries<-as.data.frame(matrix(nrow=length(participants),ncol=length(n_iterations)+2))

for(i in 1:length(participants)){for(j in 1:length(n_iterations)){Upper_boundaries[i,j]<-D[[j]][[i]][[2]]}}
for(i in 1:length(participants)){Upper_boundaries[i,length(n_iterations)+1]<-D[[1]][[i]][[1]]}
for (i in 1:length(participants)){Upper_boundaries[i,length(n_iterations)+2]<-mean(sapply(Upper_boundaries[i,1:length(n_iterations)],function(x) as.numeric(x)))}

Upper_boundaries<-Upper_boundaries[,-1:-length(n_iterations)]
colnames(Upper_boundaries)<-c("Participant","UB")

RTData<-merge(intervalspriming2,Upper_boundaries,by.x="Part",by.y="Participant")
RTData<-filter(RTData,200<RT,RT<UB,Correct==1)

#write.csv(RTData,file="FULL_Data_munged.csv")
