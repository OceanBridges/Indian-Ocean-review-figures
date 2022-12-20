title: "qPCR stations"
author: "Subhadeep chowdhury"
date: '"9th December 2022"

###Starting from here load ggplot, sf, r natural earth, rnaturalearthdata

library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

## [1] "sf"  
## [1] "data.frame"


##### Set working directory
setwd("C:/Users/subhadeep/Desktop/Review/qpcr plot/qPCR plots data and figure_18th Oct 2022")


#load your data; within seasons column mention the season to plot together
data <- read.table("Stations_IOqPCR_19_10_22.txt", header = TRUE, na.strings = "NaN")

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(25, 130), ylim = c(30, -30), expand = FALSE)+
  geom_point(data = data, mapping = aes(x = Longitude, y = Latitude, color = Monsoon), shape = 19, size= 3 ) + 
  scale_color_manual( values=c( "deeppink", "seagreen")) +
geom_text_repel(data = data,aes(x = Longitude, y = Latitude, label =paste(Station)),size = 3)


ggsave(plot = p, "IO_qPCR_19stations.png", units="cm", width=20, height=10, dpi=600)


