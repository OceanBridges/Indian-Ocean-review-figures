title: "Bar plot for qPCR data"
author: "Subhadeep chowdhury"
date: '"9th December 2022"
output: html_document

library(ggplot2)
library(viridis)
setwd("C:/Users/subhadeep/Desktop/Review/qpcr plot/qPCR plots data and figure_18th Oct 2022")
qpcr<-read.csv("ioqpcr_SC.csv")

qpcr$Station = factor(qpcr$Station, levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29"))

g<-ggplot(qpcr, aes( x=Station, y=Counts, fill=Group)) + 
  geom_bar(position="stack", stat="identity")+        #if you change "stack" to "fill" you will see the data as a percentage
  scale_fill_viridis(discrete = T)+
  theme_bw()+ylab("gene copies(log10)/L sea water")+xlab("Station")+
  ggtitle("nifH qPCR data from the IO")+ 
  theme(axis.title.x = element_text(size=10, vjust = 0.3),
        axis.title.y = element_text(size=8, vjust = 0.9),
        axis.text.y = element_text(size=16, vjust = 0.9),
        axis.text.x = element_text(size=12, vjust = 0.3, angle = 90, hjust=0.3))


g

#### Same plot but if you want to define your own colours
g<-ggplot(qpcr, aes( x=Station, y=Counts, fill=Group)) + 
  geom_bar(position="stack", stat="identity")+ 
    scale_fill_manual(values = c("darkorchid", "green" ,"orange", "blue1",
                              "black",
                               "cyan", "red", "gray",
                               "goldenrod", "saddlebrown",
                               "gold1", "slateblue", "chartreuse")) +
  theme_bw()+ylab("gene copies(log10)/L sea water")+xlab("Station")+
  #ggtitle("nifH qPCR data from the IO")+ 
  theme(axis.title.x = element_text(size=14, vjust = 0.3),
        axis.title.y = element_text(size=16, vjust = 0.9),
        axis.text.y = element_text(size=16, vjust = 0.9),
        axis.text.x = element_text(size=16, vjust = 0.3, angle = 90, hjust=0.3))
g

####