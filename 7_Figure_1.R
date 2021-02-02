# Figure_1.R
# T. Eerola, 22 May 2020

## 1. INITIALISE: SET PATH, CLEAR MEMORY AND LOAD LIBRARIES-------
library(ggplot2)

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

## 2. READ DATA --------------
RW <- read.csv('Wang_results.csv',header = T)
L<-read.csv('cents_and_interval_labels.txt',header = TRUE,sep = )
Cent<-seq(0,1200,by=1)
df <- data.frame(RW=RW$x,Cent)
rm(Cent)

## LABEL INTERVALS -------------
IV<-c("P1","m2","M2","m3","M3","P4","TT","P5","m6","M6","m7","M7","P8")
IVC<-c(seq(0,1200,by=100))
IVratio<-c(1/1,16/15,9/8,6/5,5/4,4/3,45/32,3/2,8/5,5/3,16/9,15/8,2/1)
IVratioC<-c('1/1','16/15','9/8','6/5','5/4','4/3','45/32','3/2','8/5','5/3','16/9','15/8','2/1')
IVynudge<-c(0.08,0.08,0.08,0.08,0.08,0.08,0.08,0.08,0.08,0.08,0.08,0.08,0.08)+0.04

IV_df<-data.frame(IV,IVratio,IVratioC,IVynudge,IVC)
head(IV_df)

df$IV<-NA
df$IVC<-NA
df$IVCN<-NA
for (k in 1:13) {
  ind<-which(df$Cent==IV_df$IVC[k])
  df$IV[ind]<-as.character(IV_df$IV[k])
  df$IVC[ind]<-as.character(IV_df$IVratioC[k])
  df$IVCN[ind]<-IV_df$IVynudge[k]
}
table(df$IV)
head(df)  

tmp<-seq(246.9,(246.9*2),length.out = 1201)
df$Hz<-tmp

# UB is lower boundary of ERB in CENTS!
UB<-c(187,277,316)
UB_index <- round(UB)

## PLOT ------------------------------------------------
g1<-ggplot(df,aes(x=Cent, y=RW,label=IV))+ 
  annotate("rect", xmin = UB_index[2], xmax = UB_index[3], ymin = 0, ymax = 1.4,  alpha = .1,fill='dodgerblue2',colour='dodgerblue2',size=0.25)+
  annotate("rect", xmin = UB_index[1], xmax = UB_index[2], ymin = 0, ymax = 1.4,  alpha = .2,fill='dodgerblue2',colour='dodgerblue2',size=0.25)+
  annotate("rect", xmin = 0, xmax = UB_index[1], ymin = 0, ymax = 1.4,alpha = .3,fill='dodgerblue2',colour='dodgerblue2',size=0.25)+
  geom_line(size=1.0,colour="black",alpha=0.80)+
  theme_bw()+
  xlab('Cents')+
  ylab('Roughness')+
  scale_x_continuous(limits = c(0,1200),breaks = seq(0,1200,by=100),expand = c(0.025,0.025))+
  scale_y_continuous(breaks = seq(0,1.40,by=0.25),expand = c(0.001,0.001),limits = c(0,1.40))+
  custom_theme_size+
  annotate("text", x = 100, y = 1.38,label = "ERB[a]",family = "Arial",parse=TRUE,size=3.5,hjust=0)+
  annotate("segment", x = 0, xend = UB_index[1], y = 1.35, yend = 1.35, linetype='longdash',size=0.25)+
  annotate("segment", x = UB_index[1], xend = UB_index[1], y = 1.35, yend = 1.34)+
  annotate("segment", x = UB_index[1], xend = UB_index[1], y = 1.35, yend = 1.36)+
  annotate("text", x = 200, y = 1.32,label = "ERB[b]",family = "Arial",parse=TRUE,size=3.5,hjust=0)+
  annotate("segment", x = 0, xend = UB_index[2], y = 1.29, yend = 1.29, linetype='longdash',size=0.25)+
  annotate("segment", x = UB_index[2], xend = UB_index[2], y = 1.29, yend = 1.28)+
  annotate("segment", x = UB_index[2], xend = UB_index[2], y = 1.29, yend = 1.30)+
  annotate("text", x = 300, y = 1.25,label = "ERB[c]",family = "Arial",parse=TRUE,size=3.5,hjust=0)+
  annotate("segment", x = 0, xend = UB_index[3], y = 1.22, yend = 1.22, linetype='longdash',size=0.25)+
  annotate("segment", x = UB_index[3], xend = UB_index[3], y = 1.22, yend = 1.21)+
  annotate("segment", x = UB_index[3], xend = UB_index[3], y = 1.22, yend = 1.23)+
  annotate('segment', x = df$Cent[!is.na(df$IV)], xend = df$Cent[!is.na(df$IV)], y = 0, yend = df$RW[!is.na(df$IV)],colour='gray50')+
  geom_label(nudge_y = +.02,na.rm = TRUE,label.size = .5,alpha=0.75,fill='gray15',colour='white',hjust=0.5,family = "Arial",size=6)+
  theme(text = element_text(family = "Arial"))

## SAVE ------------------------
#ggsave(filename = 'Figure1.pdf',g1,device = 'pdf',width = 8,height = 5)
print(g1)
