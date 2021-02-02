suppressMessages(library("dplyr"))

RTData<-as.data.frame(read.csv("FULL_Data_munged.csv"))


  RT_per_condition<-dplyr::summarise(dplyr::group_by(RTData,Intervals,Part,PrimeValence,TargetValence),RT=mean(RT),.groups = 'drop')
  RT_per_condition$multiplier<-ifelse(RT_per_condition$PrimeValence==RT_per_condition$TargetValence,-1,1)
  cortable<-dplyr::summarise(dplyr::group_by(RT_per_condition,Intervals,Part),index=sum(RT*multiplier),.groups = 'drop')
  deltarough<-as.data.frame(c(0.830,0.536,0.475,0.065,0.080,0.103,0.111,0.923,0.348,0.247))
  deltarough$pair<-c("d2P5","m2P5","m2tt","m3M3","m6M6","M7P5","ttP5","s2S5","m2M3","M2P5")
  deltarough$size<-c("E","H","H","L","L","L","L","E","H","H")
  df<-merge(cortable,deltarough,by.x="Intervals",by.y="pair")
  colnames(df)<-c("Intervals","Participant","Index","deltarough","size")
df$size<-factor(df$size)
print(cor.test(df$deltarough,df$Index))

