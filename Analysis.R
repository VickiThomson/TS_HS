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
library(ggplot2) # Definitely need this package
library(lmodel2) # Definitely need this package
library(RRPP)
library(lsmeans)
library(psych)
library(plotly)
library(viridis)
library(broom)
library(ggpubr)
library(lattice)
library(ggiraphExtra)
library(dplyr) # Definitely need this package
library(forcats) # Definitely need this package

setwd("/Users/vickithomson/Dropbox/2017_Tiger_Snake_head_shape/2019/Final/Final/GeoMean_2/Jan_2020/Feb_2020")
Snakes<-read.delim("/Users/vickithomson/Dropbox/2017_Tiger_Snake_head_shape/2019/Final/Final/GeoMean_2/Jan_2020/Feb_2020/Data.csv", header=TRUE, ",") 

Snakes_Fabien <- subset(Snakes, Measurer == 'Fab')  # n = 944
dim(Snakes_Fabien)
Snakes_Fabien_Williams <- droplevels(Snakes_Fabien[Snakes_Fabien$Locality == "Williams Island (SA)",])
Snakes_Fabien_Reevesby <- droplevels(Snakes_Fabien[Snakes_Fabien$Locality == "Reevesby Island (SA)",])
Snakes_Fabien_Carnac <- droplevels(Snakes_Fabien[Snakes_Fabien$Locality == "Carnac Island (WA)",])
Snakes_Fabien_WAM <- droplevels(Snakes_Fabien[Snakes_Fabien$Locality == "WA mainland",])

Snakes_Fabien_Williams_Adult <- Snakes_Fabien_Williams[Snakes_Fabien_Williams$Age == "A",] # n = 36
Snakes_Fabien_Williams_Juvenile <- Snakes_Fabien_Williams[Snakes_Fabien_Williams$Age == "J",] # n = 57
Snakes_Fabien_Reevesby_Adult <- Snakes_Fabien_Reevesby[Snakes_Fabien_Reevesby$Age == "A",] # n = 36
Snakes_Fabien_Reevesby_Juvenile <- Snakes_Fabien_Reevesby[Snakes_Fabien_Reevesby$Age == "J",] # n = 15
Snakes_Fabien_Carnac_Adult <- Snakes_Fabien_Carnac[Snakes_Fabien_Carnac$Age == "A",] # n = 78
Snakes_Fabien_Carnac_Juvenile <- Snakes_Fabien_Carnac[Snakes_Fabien_Carnac$Age == "J",] # n = 242
Snakes_Fabien_WAM_Adult <- Snakes_Fabien_WAM[Snakes_Fabien_WAM$Age == "A",] # n = 133
Snakes_Fabien_WAM_Juvenile <- Snakes_Fabien_WAM[Snakes_Fabien_WAM$Age == "J",] # n = 281

Snakes_Fabien_Adult <- rbind(Snakes_Fabien_WAM_Adult, Snakes_Fabien_Carnac_Adult, Snakes_Fabien_Reevesby_Adult, Snakes_Fabien_Williams_Adult)
Snakes_Fabien_Juvenile <- rbind(Snakes_Fabien_WAM_Juvenile, Snakes_Fabien_Carnac_Juvenile, Snakes_Fabien_Reevesby_Juvenile, Snakes_Fabien_Williams_Juvenile)

Snakes_Fabien_all <- rbind(Snakes_Fabien_Juvenile,Snakes_Fabien_Adult)
Snakes_Fabien_all$Age <- factor(Snakes_Fabien_all$Age, levels = c("J","A"))

pdf('Dataset2_SVL_all.pdf')
par(mar=c(10.1,4.1,4.1,2.1))
boxplot(Snakes_Fabien_all$SVL~Snakes_Fabien_all$Locality*Snakes_Fabien_all$Age, las = 3)
dev.off()

### T-tests of differences in measurements between populations
SnakesFA_col_MW <- rbind(Snakes_Fabien_WAM_Adult, Snakes_Fabien_Williams_Adult)
t.test(SnakesFA_col_MW$LSR_R1 ~ SnakesFA_col_MW$Locality) # p-value < 2.2e-16
t.test(SnakesFA_col_MW$LSR_R2 ~ SnakesFA_col_MW$Locality) # p-value = 0.003411
t.test(SnakesFA_col_MW$LSR_R4 ~ SnakesFA_col_MW$Locality) # p-value = 1.375e-12
t.test(SnakesFA_col_MW$LSR_R5 ~ SnakesFA_col_MW$Locality) # p-value = 0.07491
t.test(SnakesFA_col_MW$SVL ~ SnakesFA_col_MW$Locality) # p-value < 2.2e-16
t.test(SnakesFA_col_MW$GeoMean ~ SnakesFA_col_MW$Locality) # p-value < 2.2e-16

SnakesFA_col_MR <- rbind(Snakes_Fabien_WAM_Adult, Snakes_Fabien_Reevesby_Adult)
t.test(SnakesFA_col_MR$LSR_R1 ~ SnakesFA_col_MR$Locality) # p-value = 6.422e-12
t.test(SnakesFA_col_MR$LSR_R2 ~ SnakesFA_col_MR$Locality) # p-value = 0.06866
t.test(SnakesFA_col_MR$LSR_R4 ~ SnakesFA_col_MR$Locality) # p-value = 3.574e-07
t.test(SnakesFA_col_MR$LSR_R5 ~ SnakesFA_col_MR$Locality) # p-value = 0.4585
t.test(SnakesFA_col_MR$SVL ~ SnakesFA_col_MR$Locality) # p-value = 1.061e-13
t.test(SnakesFA_col_MR$GeoMean ~ SnakesFA_col_MR$Locality) # p-value = 8.705e-16

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

SnakesFA_col_WR <- rbind(Snakes_Fabien_Williams_Adult, Snakes_Fabien_Reevesby_Adult)
t.test(SnakesFA_col_WR$LSR_R1 ~ SnakesFA_col_WR$Locality) # p-value = 0.007195
t.test(SnakesFA_col_WR$LSR_R2 ~ SnakesFA_col_WR$Locality) # p-value = 0.3407
t.test(SnakesFA_col_WR$LSR_R4 ~ SnakesFA_col_WR$Locality) # p-value = 0.09139
t.test(SnakesFA_col_WR$LSR_R5 ~ SnakesFA_col_WR$Locality) # p-value = 0.6881
t.test(SnakesFA_col_WR$SVL ~ SnakesFA_col_WR$Locality) # p-value = 6.255e-06
t.test(SnakesFA_col_WR$GeoMean ~ SnakesFA_col_WR$Locality) # p-value = 5.128e-13

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

### Using lmodel2 package to allow for error in measuring both the x and y of GeoMean vs SVL
# Calculating min and max of log(SVL) for each population to start and stop regression line
Will_min <- min(Snakes_Fabien_Williams$LogSVL)
Will_max <- max(Snakes_Fabien_Williams$LogSVL)
Car_min <- min(Snakes_Fabien_Carnac$LogSVL, na.rm = TRUE)
Car_max <- max(Snakes_Fabien_Carnac$LogSVL, na.rm = TRUE)
WAMain_min <- min(Snakes_Fabien_WAM$LogSVL, na.rm = TRUE)
WAMain_max <- max(Snakes_Fabien_WAM$LogSVL, na.rm = TRUE)
Reev_min <- min(Snakes_Fabien_Reevesby$LogSVL)
Reev_max <- max(Snakes_Fabien_Reevesby$LogSVL)

# lmodel2 regression between log(head size) and log(body size)
Fabien_Will_LM <- lmodel2(LogGeoMean ~ LogSVL, data = Snakes_Fabien_Williams, nperm = 10000)
Fabien_Car_LM <- lmodel2(LogGeoMean ~ LogSVL, data = Snakes_Fabien_Carnac, nperm = 10000)
Fabien_Main_LM <- lmodel2(LogGeoMean ~ LogSVL, data = Snakes_Fabien_WAM, nperm = 10000)
Fabien_Reev_LM <- lmodel2(LogGeoMean ~ LogSVL, data = Snakes_Fabien_Reevesby, nperm = 10000)

# Slope and intercept for each equation from lmodel2
Fabien_Will_LM$regression.results$Slope[[1]]
Fabien_Car_LM$regression.results$Slope[[1]]
Fabien_Main_LM$regression.results$Slope[[1]]
Fabien_Reev_LM$regression.results$Slope[[1]]
Fabien_Will_LM$regression.results$Intercept[[1]]
Fabien_Car_LM$regression.results$Intercept[[1]]
Fabien_Main_LM$regression.results$Intercept[[1]]
Fabien_Reev_LM$regression.results$Intercept[[1]]

# Equation of lines to be plotted
equation_William_LM <- function(x){Fabien_Will_LM$regression.results$Slope[[1]]*x+Fabien_Will_LM$regression.results$Intercept[[1]]}
equation_Carnac_LM <- function(x){Fabien_Car_LM$regression.results$Slope[[1]]*x+Fabien_Car_LM$regression.results$Intercept[[1]]}
equation_WAMainland_LM <- function(x){Fabien_Main_LM$regression.results$Slope[[1]]*x+Fabien_Main_LM$regression.results$Intercept[[1]]}
equation_Reevesby_LM <- function(x){Fabien_Reev_LM$regression.results$Slope[[1]]*x+Fabien_Reev_LM$regression.results$Intercept[[1]]}

# Text labels for plot
label1_Fabien <- paste('Slope = ', round(Fabien_Will_LM$regression.results$Slope[[1]], digits = 3), sep = "")
label2_Fabien <- paste('Slope = ', round(Fabien_Car_LM$regression.results$Slope[[1]], digits = 3), sep = "")
label3_Fabien <- paste('Slope = ', round(Fabien_Main_LM$regression.results$Slope[[1]], digits = 3), sep = "")
label4_Fabien <- paste('Slope = ', round(Fabien_Reev_LM$regression.results$Slope[[1]], digits = 3), sep = "")


# Plotting head size vs body size
pdf('Fabien_Geomean_SVL_interaction_LM.pdf')
ggplot(Snakes_Fabien_all,aes(y=LogGeoMean,x=LogSVL,color=Locality))+ scale_color_manual(values=c("#8DD3C7", "#FB8072", "#E69F00", "#999999"))+
  geom_point(shape=1)+
  stat_function(fun=equation_William_LM,geom="line",size=1, xlim=c(Will_min, Will_max), color="#8DD3C7")+
  annotate(geom = "text", x=2.175, y=1.3, label=label1_Fabien,color="#8DD3C7")+
  stat_function(fun=equation_Carnac_LM,geom="line",size=1, xlim=c(Car_min, Car_max), color="#E69F00")+
  annotate(geom = "text", x=3, y=1, label=label2_Fabien,color="#E69F00")+
  stat_function(fun=equation_WAMainland_LM,geom="line",size=1, xlim=c(WAMain_min, WAMain_max), color="#999999")+
  annotate(geom = "text", x=3, y=1.1, label=label3_Fabien,color="#999999")+
  stat_function(fun=equation_Reevesby_LM,geom="line",size=1, xlim=c(Reev_min, Reev_max), color="#FB8072")+
  annotate(geom = "text", x=2.25, y=1.1, label=label4_Fabien,color="#FB8072")+
  ggtitle('Head size vs. body size for adult and juvenile Tiger Snakes \nusing Dataset 2')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Log (Body size)") + ylab("Log (Head size)")
#  geom_abline(slope = 0.1, intercept = 5, color = "black")
dev.off()

### Anton's dataset
Snakes_Anton <- subset(Snakes, Measurer == 'An')  # n = 251
dim(Snakes_Anton)
Snakes_Anton_Kang <- droplevels(Snakes_Anton[Snakes_Anton$Locality == "Kangaroo Island (SA)",])
Snakes_Anton_Reev <- droplevels(Snakes_Anton[Snakes_Anton$Locality == "Reevesby Island (SA)",])
Snakes_Anton_SAM <- droplevels(Snakes_Anton[Snakes_Anton$Locality == "SA mainland",])

Snakes_Anton_Kang_Adult <- droplevels(Snakes_Anton_Kang[Snakes_Anton_Kang$Age == "A",]) # n=80
Snakes_Anton_Kang_Juvenile <- droplevels(Snakes_Anton_Kang[Snakes_Anton_Kang$Age == "J",]) # n=20
Snakes_Anton_Kang_Neonate <- droplevels(Snakes_Anton_Kang[Snakes_Anton_Kang$Age == "N",]) # n=27
Snakes_Anton_Reev_Adult <- droplevels(Snakes_Anton_Reev[Snakes_Anton_Reev$Age == "A",]) # n=6
Snakes_Anton_Reev_Juvenile <- droplevels(Snakes_Anton_Reev[Snakes_Anton_Reev$Age == "J",]) # n=1
Snakes_Anton_Reev_Neonate <- droplevels(Snakes_Anton_Reev[Snakes_Anton_Reev$Age == "N",]) # n=36
Snakes_Anton_SAM_Adult <- droplevels(Snakes_Anton_SAM[Snakes_Anton_SAM$Age == "A",]) # n=42
Snakes_Anton_SAM_Juvenile <- droplevels(Snakes_Anton_SAM[Snakes_Anton_SAM$Age == "J",]) # n=20
Snakes_Anton_SAM_Neonate <- droplevels(Snakes_Anton_SAM[Snakes_Anton_SAM$Locality == "N",]) # n=0

Anton_Adult_all <- rbind(Snakes_Anton_SAM_Adult, Snakes_Anton_Reev_Adult, Snakes_Anton_Kang_Adult)
Anton_Juvenile_all <- rbind(Snakes_Anton_SAM_Juvenile, Snakes_Anton_Kang_Juvenile)
Anton_Neonate_all <- rbind(Snakes_Anton_Reev_Neonate, Snakes_Anton_Kang_Neonate)
Snakes_Anton_all <- rbind(Anton_Neonate_all, Anton_Juvenile_all, Anton_Adult_all)
Snakes_Anton_all$Locality <- factor(Snakes_Anton_all$Locality, levels = c("SA mainland","Reevesby Island (SA)","Kangaroo Island (SA)"))

### T-tests of differences in measurements between populations
Anton_Adult_MK <- rbind(Snakes_Anton_SAM_Adult, Snakes_Anton_Kang_Adult)
t.test(Anton_Adult_MK$LSR_R1 ~ Anton_Adult_MK$Locality) # p-value = 0.9885
t.test(Anton_Adult_MK$LSR_R2 ~ Anton_Adult_MK$Locality) # p-value = 0.2074
t.test(Anton_Adult_MK$LSR_R4 ~ Anton_Adult_MK$Locality) # p-value = 0.7492
t.test(Anton_Adult_MK$LSR_R5 ~ Anton_Adult_MK$Locality) # p-value = 0.4793
t.test(Anton_Adult_MK$SVL ~ Anton_Adult_MK$Locality) # p-value < 2.2e-16

Anton_Adult_RK <- rbind(Snakes_Anton_Reev_Adult, Snakes_Anton_Kang_Adult)
t.test(Anton_Adult_RK$LSR_R1 ~ Anton_Adult_RK$Locality) # p-value = 0.2478
t.test(Anton_Adult_RK$LSR_R2 ~ Anton_Adult_RK$Locality) # p-value = 0.09556
t.test(Anton_Adult_RK$LSR_R4 ~ Anton_Adult_RK$Locality) # p-value = 0.01015
t.test(Anton_Adult_RK$LSR_R5 ~ Anton_Adult_RK$Locality) # p-value = 0.7852
t.test(Anton_Adult_RK$SVL ~ Anton_Adult_RK$Locality) # p-value = 0.03454

Anton_Adult_MR <- rbind(Snakes_Anton_SAM_Adult, Snakes_Anton_Reev_Adult)
t.test(Anton_Adult_MR$LSR_R1 ~ Anton_Adult_MR$Locality) # p-value = 0.2635
t.test(Anton_Adult_MR$LSR_R2 ~ Anton_Adult_MR$Locality) # p-value = 0.04686
t.test(Anton_Adult_MR$LSR_R4 ~ Anton_Adult_MR$Locality) # p-value = 0.009504
t.test(Anton_Adult_MR$LSR_R5 ~ Anton_Adult_MR$Locality) # p-value = 0.9763
t.test(Anton_Adult_MR$SVL ~ Anton_Adult_MR$Locality) # p-value = 0.01495

Anton_Juvenile_MK <- rbind(Snakes_Anton_SAM_Juvenile, Snakes_Anton_Kang_Juvenile)
t.test(Anton_Juvenile_MK$LSR_R1 ~ Anton_Juvenile_MK$Locality) # p-value = 0.3338
t.test(Anton_Juvenile_MK$LSR_R2 ~ Anton_Juvenile_MK$Locality) # p-value = 0.6769
t.test(Anton_Juvenile_MK$LSR_R4 ~ Anton_Juvenile_MK$Locality) # p-value = 0.8597
t.test(Anton_Juvenile_MK$LSR_R5 ~ Anton_Juvenile_MK$Locality) # p-value = 0.7141
t.test(Anton_Juvenile_MK$SVL ~ Anton_Juvenile_MK$Locality) # p-value = 0.7779

Anton_Neonate_RK <- rbind(Snakes_Anton_Reev_Neonate, Snakes_Anton_Kang_Neonate)
t.test(Anton_Neonate_RK$LSR_R1 ~ Anton_Neonate_RK$Locality) # p-value = 0.4446
t.test(Anton_Neonate_RK$LSR_R2 ~ Anton_Neonate_RK$Locality) # p-value = 0.002229
t.test(Anton_Neonate_RK$LSR_R4 ~ Anton_Neonate_RK$Locality) # p-value = 0.001088
t.test(Anton_Neonate_RK$LSR_R5 ~ Anton_Neonate_RK$Locality) # p-value = 0.1412
t.test(Anton_Neonate_RK$SVL ~ Anton_Neonate_RK$Locality) # p-value = 1.578e-13

pdf('Dataset1_SVL_all.pdf')
par(mar=c(10.1,4.1,4.1,2.1))
boxplot(Snakes_Anton_all$SVL~Snakes_Anton_all$Locality*Snakes_Anton_all$Age, las = 3)
dev.off()








