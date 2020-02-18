install.packages("devtools")

install.packages("geomorph")
library(geomorph)

devtools::install_github("cardiomoon/ggiraphExtra")

require(ggiraph)
require(ggiraphExtra)
require(plyr)
install.packages("scales")
install.packages("ggrepel")
install.packages("GGally")
install.packages("proto")
require(GGally)
library(scales)
library(ggrepel)
library(proto)
library(ggplot2) 
library(lmodel2)
library(RRPP)
library(lsmeans)
library(psych)
library(plotly)
library(viridis)
library(broom)
library(ggpubr)
library(lattice)
library(ggiraphExtra)
library(plyr)

setwd("/Users/vickithomson/Dropbox/2017_Tiger_Snake_head_shape/2019/Final/Final/GeoMean_2/Jan_2020/Feb_2020")
Snakes<-read.delim("/Users/vickithomson/Dropbox/2017_Tiger_Snake_head_shape/2019/Final/Final/GeoMean_2/Jan_2020/Feb_2020/Data.csv", header=TRUE, ",") 

Snakes_Fabien <- subset(Snakes, Measurer == 'Fab')  # n = 859
dim(Snakes_Fabien)
Snakes_Fabien_Williams <- droplevels(Snakes_Fabien[Snakes_Fabien$Locality == "Williams Island (SA)",])
Snakes_Fabien_Reevesby <- droplevels(Snakes_Fabien[Snakes_Fabien$Locality == "Reevesby Island (SA)",])
Snakes_Fabien_Carnac <- droplevels(Snakes_Fabien[Snakes_Fabien$Locality == "Carnac Island (WA)",])
Snakes_Fabien_WAM <- droplevels(Snakes_Fabien[Snakes_Fabien$Locality == "Reevesby Island (WA)",])

Snakes_Fabien_Williams_Adult <- Snakes_Fabien[Snakes_Fabien$Age == "A",]
Snakes_Fabien_Adult_Williams <- Snakes_Fabien_Adult[Snakes_Fabien_Adult$Locality == "Williams Island (SA)",]

pdf('Dataset2_SVL_all.pdf')
par(mar=c(10.1,4.1,4.1,2.1))
boxplot(Snakes_Fabien_Adult$SVL~Snakes_Fabien_Adult$Locality, las = 3)
dev.off()



