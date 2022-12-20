title: "DADA2 amplicon data visualization by ampvis2 for Io Review"
author: "subhadeep CHOWDHURY"
date: "9th December 2022"
output: html_document

####nifH from BoB (15 stations)
setwd("C:/Users/subhadeep/Desktop/wu_2019")
install.packages("remotes")
remotes::install_github("MadsAlbertsen/ampvis2")

library("readxl") # necessary to import the data from Excel file
library("dplyr") # filter and reformat data frames
library(ampvis2) # Ampvis for analysis
library(ggplot2) # for visualizing
library(ggdendro) # For making dendogram
library(tidyverse) 
library(factoextra)
library(cluster)
library(Biostrings)

#Making dendogram to make a hierachical clustering of the stations
##Loading the data

list.files()

myotutable <- read.csv("visualization_BoB.csv", 
                       check.names = FALSE, 
                       fileEncoding="UTF-8-BOM")

mymetadata <- read.csv("metadata.csv", 
                       check.names = FALSE,  
                       fileEncoding="UTF-8-BOM")


###We are using the Ward method since it finds the minimum variance in order to make the tree

metadata_dendo <- column_to_rownames(mymetadata, var = "SampleID") %>% 
  select(c("Area"))

hca<-hclust(dist(metadata_dendo),method="ward.D") #look for ?hclust for more info
                                                  #You can use also complete as another method

p <- fviz_dend(hca, k = 5,                 # Cut in four groups
          cex = 0.3,                 # label size
          color_labels_by_k = TRUE,  # color labels by groups
          ggtheme = theme_classic(),    # Change theme
          horiz = TRUE,
          rect=TRUE
)

p

ggsave(plot = p, "nifD_subhadeep_dendogram.png", units="cm", width=20, height=10, dpi=600)
######
amp_heatmap(
  data = data,
  group_by = "SampleID",
  tax_aggregate = "Class",
  tax_show = 5)
  
 #### ,
  plot_functions = TRUE,
  functions = c("FIL", 
                "AOB",
                "NOB",
                "PAO",
                "GAO"),
  order_x_by = c("Spring",
                 "Summer",
                 "Fall",
                 "Winter"),
  rel_widths = c(0.6, 0.4)
)
#Ampvis analysis
##define the row names from the ASV column

row.names(myotutable) <- myotutable$ASV

#remove the column ASV since it is now used as a row name

myotutable <- myotutable #%>% select(-ASV)

#Loading data into ampvis2 format

d <- amp_load(otutable = myotutable, 
              metadata = mymetadata, 
              fasta = "ASVs_BoB_whole area.fasta")


d$metadata
d$abund
d$tax
d$refseq

#Heatmap of phylum based on Station

dsubset <- amp_subset_samples(d, SampleID %in% d[["metadata"]][["SampleID"]])

#####to change the order of the panel####

dsubset$metadata$Area <- factor(dsubset$metadata$Area,      # Reordering group factor levels
                         levels = c("EIO", "BoB"))

p <- amp_heatmap(group_by = "SampleID",
            plot_colorscale = "sqrt",   #How to plot abundance in log10 or sqrt
            plot_values = TRUE,        #Plot values in boxes for each sample
            normalise = TRUE,
            tax_aggregate = "Class",
            tax_add = "Phylum",
            tax_show =5)
p

?amp_heatmap

ggsave(plot = p, "nifH_each_station.jpeg", units="cm", width=30, height=20, dpi=600)


# heatmap according to temperature and area
pa = wes_palettes %>% 
  names()

pa
pal = wes_palette(name = pa[7], n = 10, type = "continuous")
dsubset <- amp_subset_samples(d, Area %in% c("EIO", 
                                             "BoB"))
p <- amp_heatmap(dsubset,
         group_by = "SampleID",  #Grouping by Temp
         facet_by = "Area", #+Area",   # Plot diveded into Depths/Area
         facet_grid(~ "Area"),   
         tax_aggregate = "Class",  #Show Kingdom 
            tax_add = "Phylum",          #Add extra tax layer, of phylum
            tax_show = 15,   #Show tpo 20 ASV
            plot_colorscale = "sqrt",   #How to plot abundance in log10 or sqrt
            plot_values = FALSE,        #Plot values in boxes for each sample
            normalise = TRUE) + #Normalise read into percent per samples
  theme(axis.text.x = element_text(angle = 45, size=9, vjust = 1),
        axis.text.y = element_text(size=10),
        legend.position="right")
  
# ggtitle("nifH")  # Beutification 

p

ggsave(plot = p, "top 20 asv group by sample_name phylum class mangrove.jpeg", units="cm", width=40, height=20, dpi=800)

#draw boxplot

p <- amp_boxplot(dsubset,
            group_by = "SampleID",
            tax_aggregate = "Class",
            tax_show = 5,
            tax_add = "Phylum") #Boxplot, most abundant taxa
p

ggsave(plot = p, " Boxplot group by Sample_Name.jpeg", units="cm", width=20, height=10, dpi=800)


#rarafraction curve, to see sequencing enrichment

p <- amp_rarecurve(data = dsubset,
                   facet_by = "SampleID", 
                   stepsize = 100
                   )
p

ggsave(plot = p, "nifH rarefraction.png", units="cm", width=20, height=10, dpi=600)

# alphadiversity

alphadiversityresult <- amp_alphadiv(d,measure = c("shannon", "simpson"))

alphadiversityresult

print(alphadiversityresult)

#CCA graph for AREA NOTE!, you can change "type" to either CCA, RDA or PCA.

ordinationresult <- amp_ordinate(dsubset, 
                                 type = "CCA",
                                 #constrain = "SampleID",
                                 transform = "Hellinger",
                                 sample_color_by = "SampleID",
                                 sample_colorframe = TRUE,
                                 #sample_colorframe_label = "Temp",
                                 detailed_output = TRUE)
p <- ordinationresult$plot

p

ggsave(plot = p, "nifD_subhadeep_rda.png", units="cm", width=20, height=10, dpi=600)

