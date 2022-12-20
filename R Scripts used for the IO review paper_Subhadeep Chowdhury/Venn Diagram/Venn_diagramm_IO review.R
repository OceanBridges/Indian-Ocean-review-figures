Author: "Cora Hoerstmann"

##Referece acknowledgement:
#https://doi.org/10.3389/fgene.2021.706907 (Gao et al. 2021)
#install.packages("ggvenn")   


library("tidyr")
          
library("ggvenn")

#IMPORT
BoB_tax <- read.csv("D://Venn diagram_Cora_v2/BoB/ASVs_Taxonomy_BoB_whole area.tsv", sep = '\t', header = T, row.names = 1)
EIO_tax <-read.csv("D://Venn diagram_Cora_v2/EIO/ASVs_Taxonomy_EIO_whole area.tsv", sep = '\t', header = T, row.names = 1)
SIO_tax <- read.csv("D://Venn diagram_Cora_v2/SIO/ASVs_Taxonomy_SIO_whole area.tsv", sep = '\t', row.names = 1, header = T)


#REMOVE NAS FROM TAXONOMY LIST
Raes_tax_genus <- Raes_tax %>% drop_na(Genus)
Wu19_tax_genus <- Wu19_tax %>% drop_na(Genus)
Wu21_tax_genus <- Wu21_tax %>% drop_na(Genus)

BoB_tax_genus <- BoB_tax %>% drop_na(Genus)
EIO_tax_genus <- EIO_tax %>% drop_na(Genus)
SIO_tax_genus <- SIO_tax %>% drop_na(Genus)


# calculate overlaps at genus level
intersect(Raes_tax_genus$Genus, Wu19_tax_genus$Genus)
length(intersect(Raes_tax_genus$Genus, Wu19_tax_genus$Genus))
length(intersect(Raes_tax_genus$Genus, Wu21_tax_genus$Genus))
length(intersect(Wu21_tax_genus$Genus, Wu19_tax_genus$Genus))
length(intersect(intersect(Raes_tax_genus$Genus, Wu19_tax_genus$Genus), Wu21_tax_genus$Genus))

#draw venn diagramm


venn_Fam <- list(BoB = BoB_tax_genus$Family,
                 EIO = EIO_tax_genus$Family,
                 SIO = SIO_tax_genus$Family)

venn_order <- list(BoB = BoB_tax_genus$Order,
                   EIO = EIO_tax_genus$Order,
                   SIO = SIO_tax_genus$Order)



ggvenn(venn_Fam, c("BoB", "EIO", "SIO"), stroke_size = 0.5, text_size = 4, set_name_size = 0)

ggvenn(venn_order, c("BoB", "EIO", "SIO"), stroke_size = 0.5, text_size = 4, set_name_size = 0)


#save image 900x582 .png