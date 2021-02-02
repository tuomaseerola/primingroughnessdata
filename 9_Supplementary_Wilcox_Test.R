# t-test on each interval pair for Automatic responses to acoustically rough musical intervals:
print("Test on each interval pair for automatic responses to acoustical rough musical intervals:")
library(lsr)
library(dplyr)
intervalspriming2<-as.data.frame(read.csv("FULL_Data_munged.csv"))

bigfunction<-function(w){
  separate<-function(x){
    
    B<-filter(intervalspriming2,Intervals==x)
    return(B)}
  INT<-unique(intervalspriming2$Intervals)
  C<-lapply(INT,separate)
  
  testing<-function(y){
    y<-as.data.frame(y)
    y$Congruence<-ifelse(y$PrimeValence==y$TargetValence,"C","I")
    D<-dplyr::summarise(dplyr::group_by(y,Part,Congruence),RT=mean(RT),.groups = 'drop')
    E<-wilcox.test(data=D,log(RT)~Congruence,paired=TRUE,alternative="less",p.adjust="none")
    D<-dplyr::summarise(dplyr::group_by(y,Part,Congruence),RT=mean(RT),.groups = 'drop')
    es<-cohensD(data=D,RT~Congruence,method="paired")
    Fs<-list(E,es)
    return(Fs)
  }
  G<-lapply(C,testing)
}
n<-c(1:1)
H<-suppressWarnings(lapply(n,bigfunction))

TESTS<-matrix(nrow=10,ncol=4)
for(i in 1:10){TESTS[i,1]<-paste(unique(intervalspriming2$Intervals)[[i]])}
for(i in 1:10){TESTS[i,2]<-(H[[1]][[i]][[1]]$statistic)}
for(i in 1:10){TESTS[i,3]<-(H[[1]][[i]][[1]]$p.value)}
for(i in 1:10){TESTS[i,4]<-(H[[1]][[i]][[2]][[1]])}

colnames(TESTS)<-c("Intervals","V-value","p.value","Cohen's d")
TESTS<-data.frame(TESTS)
TESTS$p.value<-as.numeric(TESTS$p.value)     # Convert to numeric
TESTS$Cohen.s.d<-as.numeric(TESTS$Cohen.s.d) # Convert to numeric
#write.table(TESTS,"Table_S1.txt")
print(knitr::kable(TESTS,digits = 3))
