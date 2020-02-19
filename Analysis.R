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

Snakes_Fabien <- subset(Snakes, Measurer == 'Fab')  # n = 944
dim(Snakes_Fabien)
Snakes_Fabien_Williams <- droplevels(Snakes_Fabien[Snakes_Fabien$Locality == "Williams Island (SA)",])
Snakes_Fabien_Reevesby <- droplevels(Snakes_Fabien[Snakes_Fabien$Locality == "Reevesby Island (SA)",])
Snakes_Fabien_Carnac <- droplevels(Snakes_Fabien[Snakes_Fabien$Locality == "Carnac Island (WA)",])
Snakes_Fabien_WAM <- droplevels(Snakes_Fabien[Snakes_Fabien$Locality == "WA mainland",])

Snakes_Fabien_Williams_Adult <- Snakes_Fabien_Williams[Snakes_Fabien_Williams$Age == "A",]
Snakes_Fabien_Williams_Juvenile <- Snakes_Fabien_Williams[Snakes_Fabien_Williams$Age == "J",]
Snakes_Fabien_Reevesby_Adult <- Snakes_Fabien_Reevesby[Snakes_Fabien_Reevesby$Age == "A",]
Snakes_Fabien_Reevesby_Juvenile <- Snakes_Fabien_Reevesby[Snakes_Fabien_Reevesby$Age == "J",]
Snakes_Fabien_Carnac_Adult <- Snakes_Fabien_Carnac[Snakes_Fabien_Carnac$Age == "A",]
Snakes_Fabien_Carnac_Juvenile <- Snakes_Fabien_Carnac[Snakes_Fabien_Carnac$Age == "J",]
Snakes_Fabien_WAM_Adult <- Snakes_Fabien_WAM[Snakes_Fabien_WAM$Age == "A",]
Snakes_Fabien_WAM_Juvenile <- Snakes_Fabien_WAM[Snakes_Fabien_WAM$Age == "J",]

Snakes_Fabien_Adult <- rbind(Snakes_Fabien_Williams_Adult, Snakes_Fabien_Reevesby_Adult, Snakes_Fabien_Carnac_Adult, Snakes_Fabien_WAM_Adult)
Snakes_Fabien_Juvenile <- rbind(Snakes_Fabien_Williams_Juvenile, Snakes_Fabien_Reevesby_Juvenile, Snakes_Fabien_Carnac_Juvenile, Snakes_Fabien_WAM_Juvenile)

Snakes_Fabien_all <- rbind(Snakes_Fabien_Adult, Snakes_Fabien_Juvenile)

pdf('Dataset2_SVL_all.pdf')
par(mar=c(10.1,4.1,4.1,2.1))
boxplot(Snakes_Fabien_all$SVL~Snakes_Fabien_all$Locality* Snakes_Fabien_all$Age, las = 3)
dev.off()

### T-tests of differences in measurements between populations
SnakesFA_col_MW <- rbind(Snakes_Fabien_WAM_Adult, Snakes_Fabien_Williams_Adult)
t.test(SnakesFA_col_MW$LSR_R1 ~ SnakesFA_col_MW$Locality) # p-value < 2.2e-16
t.test(SnakesFA_col_MW$LSR_R2 ~ SnakesFA_col_MW$Locality) # p-value = 0.003411
t.test(SnakesFA_col_MW$LSR_R4 ~ SnakesFA_col_MW$Locality) # p-value = 1.375e-12
t.test(SnakesFA_col_MW$LSR_R5 ~ SnakesFA_col_MW$Locality) # p-value = 0.07491
t.test(SnakesFA_col_MW$SVL ~ SnakesFA_col_MW$Locality) # p-value < 2.2e-16
t.test(SnakesFA_col_MW$GeoMean ~ SnakesFA_col_MW$Locality) # p-value < 2.2e-16

SnakesFA_col_MC <- rbind(Snakes_Fabien_WAM_Adult, Snakes_Fabien_Carnac_Adult)
t.test(SnakesFA_col_MC$LSR_R1 ~ SnakesFA_col_MC$Locality) # p-value = 1.045e-07
t.test(SnakesFA_col_MC$LSR_R2 ~ SnakesFA_col_MC$Locality) # p-value = 0.2348
t.test(SnakesFA_col_MC$LSR_R4 ~ SnakesFA_col_MC$Locality) # p-value = 0.006111
t.test(SnakesFA_col_MC$LSR_R5 ~ SnakesFA_col_MC$Locality) # p-value = 0.1546
t.test(SnakesFA_col_MC$SVL~ SnakesFA_col_MC$Locality) # p-value = 8.874e-15
t.test(SnakesFA_col_MC$GeoMean~ SnakesFA_col_MC$Locality) # p-value = 7.878e-13

SnakesFA_col_WC <- rbind(Snakes_Fabien_Williams_Adult, Snakes_Fabien_Carnac_Adult)
t.test(SnakesFA_col_WC$LSR_R1 ~ SnakesFA_col_WC$Locality) # p-value = 0.00844
t.test(SnakesFA_col_WC$LSR_R2 ~ SnakesFA_col_WC$Locality) # p-value = 0.0006076
t.test(SnakesFA_col_WC$LSR_R4 ~ SnakesFA_col_WC$Locality) # p-value = 0.009926
t.test(SnakesFA_col_WC$LSR_R5 ~ SnakesFA_col_WC$Locality) # p-value = 0.0143
t.test(SnakesFA_col_WC$SVL ~ SnakesFA_col_WC$Locality) # p-value < 2.2e-16
t.test(SnakesFA_col_WC$GeoMean ~ SnakesFA_col_WC$Locality) # p-value < 2.2e-16

SnakesFJ_col_MW <- rbind(Snakes_Fabien_WAM_Juvenile, Snakes_Fabien_Williams_Juvenile)
t.test(SnakesFJ_col_MW$LSR_R1 ~ SnakesFJ_col_MW$Locality) # p-value = 1.581e-07
t.test(SnakesFJ_col_MW$LSR_R2 ~ SnakesFJ_col_MW$Locality) # p-value = 0.04866
t.test(SnakesFJ_col_MW$LSR_R4 ~ SnakesFJ_col_MW$Locality) # p-value = 2.691e-14
t.test(SnakesFJ_col_MW$LSR_R5 ~ SnakesFJ_col_MW$Locality) # p-value = 2.833e-05
t.test(SnakesFJ_col_MW$SVL ~ SnakesFJ_col_MW$Locality) # p-value = 9.928e-05
t.test(SnakesFJ_col_MW$GeoMean ~ SnakesFJ_col_MW$Locality) # p-value = 2.416e-07

SnakesFJ_col_MC <- rbind(Snakes_Fabien_WAM_Juvenile, Snakes_Fabien_Carnac_Juvenile)
t.test(SnakesFJ_col_MC$LSR_R1 ~ SnakesFJ_col_MC$Locality) # p-value = 0.5309
t.test(SnakesFJ_col_MC$LSR_R2 ~ SnakesFJ_col_MC$Locality) # p-value = 0.6204
t.test(SnakesFJ_col_MC$LSR_R4 ~ SnakesFJ_col_MC$Locality) # p-value = 1.505e-07
t.test(SnakesFJ_col_MC$LSR_R5 ~ SnakesFJ_col_MC$Locality) # p-value = 0.000609
t.test(SnakesFJ_col_MC$SVL~ SnakesFJ_col_MC$Locality) # p-value = 0.01948
t.test(SnakesFJ_col_MC$GeoMean~ SnakesFJ_col_MC$Locality) # p-value = 0.9907

SnakesFJ_col_WC <- rbind(Snakes_Fabien_Williams_Juvenile, Snakes_Fabien_Carnac_Juvenile)
t.test(SnakesFJ_col_WC$LSR_R1 ~ SnakesFJ_col_WC$Locality) # p-value = 1.446e-08
t.test(SnakesFJ_col_WC$LSR_R2 ~ SnakesFJ_col_WC$Locality) # p-value = 0.01419
t.test(SnakesFJ_col_WC$LSR_R4 ~ SnakesFJ_col_WC$Locality) # p-value = 4.706e-06
t.test(SnakesFJ_col_WC$LSR_R5 ~ SnakesFJ_col_WC$Locality) # p-value = 0.1692
t.test(SnakesFJ_col_WC$SVL ~ SnakesFJ_col_WC$Locality) # p-value = 7.343e-07
t.test(SnakesFJ_col_WC$GeoMean ~ SnakesFJ_col_WC$Locality) # p-value = 1.117e-07



