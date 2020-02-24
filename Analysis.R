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
library(ade4) # Definitely need this package

setwd("/Users/vickithomson/Dropbox/2017_Tiger_Snake_head_shape/2019/Final/Final/GeoMean_2/Jan_2020/Feb_2020")
Snakes<-read.delim("/Users/vickithomson/Dropbox/2017_Tiger_Snake_head_shape/2019/Final/Final/GeoMean_2/Jan_2020/Feb_2020/Data_revised.csv", header=TRUE, ",") 

Snakes_Fabien <- subset(Snakes, Measurer == 'Fab')  # n = 974
dim(Snakes_Fabien)
Snakes_Fabien_Williams <- droplevels(Snakes_Fabien[Snakes_Fabien$Locality == "Williams Island (SA)",])
Snakes_Fabien_Reevesby <- droplevels(Snakes_Fabien[Snakes_Fabien$Locality == "Reevesby Island (SA)",])
Snakes_Fabien_Carnac <- droplevels(Snakes_Fabien[Snakes_Fabien$Locality == "Carnac Island (WA)",])
Snakes_Fabien_WAM <- droplevels(Snakes_Fabien[Snakes_Fabien$Locality == "WA mainland",])

Snakes_Fabien_Williams_Adult <- Snakes_Fabien_Williams[Snakes_Fabien_Williams$Age == "A",] # n = 36
Snakes_Fabien_Williams_Juvenile <- Snakes_Fabien_Williams[Snakes_Fabien_Williams$Age == "J",] # n = 8
Snakes_Fabien_Williams_Neonate <- Snakes_Fabien_Williams[Snakes_Fabien_Williams$Age == "N",] # n = 49
Snakes_Fabien_Reevesby_Adult <- Snakes_Fabien_Reevesby[Snakes_Fabien_Reevesby$Age == "A",] # n = 36
Snakes_Fabien_Reevesby_Juvenile <- Snakes_Fabien_Reevesby[Snakes_Fabien_Reevesby$Age == "J",] # n = 6
Snakes_Fabien_Reevesby_Neonate <- Snakes_Fabien_Reevesby[Snakes_Fabien_Reevesby$Age == "N",] # n = 9
Snakes_Fabien_Carnac_Adult <- Snakes_Fabien_Carnac[Snakes_Fabien_Carnac$Age == "A",] # n = 85
Snakes_Fabien_Carnac_Juvenile <- Snakes_Fabien_Carnac[Snakes_Fabien_Carnac$Age == "J",] # n = 0
Snakes_Fabien_Carnac_Neonate <- Snakes_Fabien_Carnac[Snakes_Fabien_Carnac$Age == "N",] # n = 235
Snakes_Fabien_WAM_Adult <- Snakes_Fabien_WAM[Snakes_Fabien_WAM$Age == "A",] # n = 134
Snakes_Fabien_WAM_Juvenile <- Snakes_Fabien_WAM[Snakes_Fabien_WAM$Age == "J",] # n = 0
Snakes_Fabien_WAM_Neonate <- Snakes_Fabien_WAM[Snakes_Fabien_WAM$Age == "N",] # n = 260

Snakes_Fabien_Adult <- rbind(Snakes_Fabien_WAM_Adult, Snakes_Fabien_Carnac_Adult, Snakes_Fabien_Reevesby_Adult, Snakes_Fabien_Williams_Adult)
Snakes_Fabien_Neonate <- rbind(Snakes_Fabien_WAM_Neonate, Snakes_Fabien_Carnac_Neonate, Snakes_Fabien_Reevesby_Neonate, Snakes_Fabien_Williams_Neonate)

Snakes_Fabien_all <- rbind(Snakes_Fabien_Neonate,Snakes_Fabien_Adult)
Snakes_Fabien_all$Age <- factor(Snakes_Fabien_all$Age, levels = c("N","A"))

pdf('Dataset2_SVL_all.pdf')
par(mar=c(10.1,4.1,4.1,2.1))
boxplot(Snakes_Fabien_all$SVL~Snakes_Fabien_all$Locality*Snakes_Fabien_all$Age, las = 3, ylab = "Body size (SVL)", xlab = "", main = "Body size for each size class and population")
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
t.test(SnakesFA_col_MC$LSR_R1 ~ SnakesFA_col_MC$Locality) # p-value = 6.173e-08
t.test(SnakesFA_col_MC$LSR_R2 ~ SnakesFA_col_MC$Locality) # p-value = 0.08992
t.test(SnakesFA_col_MC$LSR_R4 ~ SnakesFA_col_MC$Locality) # p-value = 0.004549
t.test(SnakesFA_col_MC$LSR_R5 ~ SnakesFA_col_MC$Locality) # p-value = 0.09346
t.test(SnakesFA_col_MC$SVL~ SnakesFA_col_MC$Locality) # p-value = 6.16e-11
t.test(SnakesFA_col_MC$GeoMean~ SnakesFA_col_MC$Locality) # p-value = .423e-07

SnakesFA_col_WC <- rbind(Snakes_Fabien_Williams_Adult, Snakes_Fabien_Carnac_Adult)
t.test(SnakesFA_col_WC$LSR_R1 ~ SnakesFA_col_WC$Locality) # p-value = 0.01012
t.test(SnakesFA_col_WC$LSR_R2 ~ SnakesFA_col_WC$Locality) # p-value = 0.0001791
t.test(SnakesFA_col_WC$LSR_R4 ~ SnakesFA_col_WC$Locality) # p-value = 0.008459
t.test(SnakesFA_col_WC$LSR_R5 ~ SnakesFA_col_WC$Locality) # p-value = 0.007341
t.test(SnakesFA_col_WC$SVL ~ SnakesFA_col_WC$Locality) # p-value < 2.2e-16
t.test(SnakesFA_col_WC$GeoMean ~ SnakesFA_col_WC$Locality) # p-value < 2.2e-16

SnakesFA_col_WR <- rbind(Snakes_Fabien_Williams_Adult, Snakes_Fabien_Reevesby_Adult)
t.test(SnakesFA_col_WR$LSR_R1 ~ SnakesFA_col_WR$Locality) # p-value = 0.007195
t.test(SnakesFA_col_WR$LSR_R2 ~ SnakesFA_col_WR$Locality) # p-value = 0.3407
t.test(SnakesFA_col_WR$LSR_R4 ~ SnakesFA_col_WR$Locality) # p-value = 0.09139
t.test(SnakesFA_col_WR$LSR_R5 ~ SnakesFA_col_WR$Locality) # p-value = 0.6881
t.test(SnakesFA_col_WR$SVL ~ SnakesFA_col_WR$Locality) # p-value = 6.255e-06
t.test(SnakesFA_col_WR$GeoMean ~ SnakesFA_col_WR$Locality) # p-value = 5.128e-13

SnakesFN_col_MW <- rbind(Snakes_Fabien_WAM_Neonate, Snakes_Fabien_Williams_Neonate)
t.test(SnakesFN_col_MW$LSR_R1 ~ SnakesFN_col_MW$Locality) # p-value = 6.945e-08
t.test(SnakesFN_col_MW$LSR_R2 ~ SnakesFN_col_MW$Locality) # p-value = 0.1757
t.test(SnakesFN_col_MW$LSR_R4 ~ SnakesFN_col_MW$Locality) # p-value = 3.761e-12
t.test(SnakesFN_col_MW$LSR_R5 ~ SnakesFN_col_MW$Locality) # p-value = 8.76e-05
t.test(SnakesFN_col_MW$SVL ~ SnakesFN_col_MW$Locality) # p-value < 2.2e-16
t.test(SnakesFN_col_MW$GeoMean ~ SnakesFN_col_MW$Locality) # p-value < 2.2e-16

SnakesFN_col_MC <- rbind(Snakes_Fabien_WAM_Neonate, Snakes_Fabien_Carnac_Neonate)
t.test(SnakesFN_col_MC$LSR_R1 ~ SnakesFN_col_MC$Locality) # p-value = 0.9481
t.test(SnakesFN_col_MC$LSR_R2 ~ SnakesFN_col_MC$Locality) # p-value = 0.9156
t.test(SnakesFN_col_MC$LSR_R4 ~ SnakesFN_col_MC$Locality) # p-value = 5.143e-07
t.test(SnakesFN_col_MC$LSR_R5 ~ SnakesFN_col_MC$Locality) # p-value = 0.0007517
t.test(SnakesFN_col_MC$SVL~ SnakesFN_col_MC$Locality) # p-value = 0.0006121
t.test(SnakesFN_col_MC$GeoMean~ SnakesFN_col_MC$Locality) # p-value < 2.2e-16

SnakesFN_col_WC <- rbind(Snakes_Fabien_Williams_Neonate, Snakes_Fabien_Carnac_Neonate)
t.test(SnakesFN_col_WC$LSR_R1 ~ SnakesFN_col_WC$Locality) # p-value = 1.844e-08
t.test(SnakesFN_col_WC$LSR_R2 ~ SnakesFN_col_WC$Locality) # p-value = 0.1768
t.test(SnakesFN_col_WC$LSR_R4 ~ SnakesFN_col_WC$Locality) # p-value = 2.823e-05
t.test(SnakesFN_col_WC$LSR_R5 ~ SnakesFN_col_WC$Locality) # p-value = 0.1562
t.test(SnakesFN_col_WC$SVL ~ SnakesFN_col_WC$Locality) # p-value < 2.2e-16
t.test(SnakesFN_col_WC$GeoMean ~ SnakesFN_col_WC$Locality) # p-value < 2.2e-16

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
ggplot(Snakes_Fabien_all,aes(y=LogGeoMean,x=LogSVL,color=Locality))+ scale_color_manual(values=c("#999999", "#FB8072", "#E69F00", "#8DD3C7"))+
  geom_point(shape=1)+
  stat_function(fun=equation_William_LM,geom="line",size=1, xlim=c(Will_min, Will_max), color="#8DD3C7")+
  annotate(geom = "text", x=2.75, y=1.4, label=label1_Fabien,color="#8DD3C7")+
  stat_function(fun=equation_Carnac_LM,geom="line",size=1, xlim=c(Car_min, Car_max), color="#FB8072")+
  annotate(geom = "text", x=3, y=1, label=label2_Fabien,color="#FB8072")+
  stat_function(fun=equation_WAMainland_LM,geom="line",size=1, xlim=c(WAMain_min, WAMain_max), color="#999999")+
  annotate(geom = "text", x=3, y=1.1, label=label3_Fabien,color="#999999")+
  stat_function(fun=equation_Reevesby_LM,geom="line",size=1, xlim=c(Reev_min, Reev_max), color="#E69F00")+
  annotate(geom = "text", x=2.25, y=1.1, label=label4_Fabien,color="#E69F00")+
  ggtitle('Head size vs. body size for adult and neonate \nTiger Snakes using Dataset 2')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Log (Body size)") + ylab("Log (Head size)")
#  geom_abline(slope = 0.1, intercept = 5, color = "black")
dev.off()

### Anton's dataset
Snakes_Anton <- subset(Snakes, Measurer == 'An')  # n = 386
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
t.test(Anton_Juvenile_MK$LSR_R1 ~ Anton_Juvenile_MK$Locality) # p-value = 0.009554
t.test(Anton_Juvenile_MK$LSR_R2 ~ Anton_Juvenile_MK$Locality) # p-value = 0.1189
t.test(Anton_Juvenile_MK$LSR_R4 ~ Anton_Juvenile_MK$Locality) # p-value = 0.1096
t.test(Anton_Juvenile_MK$LSR_R5 ~ Anton_Juvenile_MK$Locality) # p-value = 0.5052
t.test(Anton_Juvenile_MK$SVL ~ Anton_Juvenile_MK$Locality) # p-value = 0.002338

Anton_Neonate_RK <- rbind(Snakes_Anton_Reev_Neonate, Snakes_Anton_Kang_Neonate)
t.test(Anton_Neonate_RK$LSR_R1 ~ Anton_Neonate_RK$Locality) # p-value = 0.4446
t.test(Anton_Neonate_RK$LSR_R2 ~ Anton_Neonate_RK$Locality) # p-value = 0.002229
t.test(Anton_Neonate_RK$LSR_R4 ~ Anton_Neonate_RK$Locality) # p-value = 0.001088
t.test(Anton_Neonate_RK$LSR_R5 ~ Anton_Neonate_RK$Locality) # p-value = 0.1412
t.test(Anton_Neonate_RK$SVL ~ Anton_Neonate_RK$Locality) # p-value = 1.578e-13

pdf('Dataset1_SVL_all.pdf')
par(mar=c(10.1,4.1,4.1,2.1))
boxplot(Snakes_Anton_all$SVL~Snakes_Anton_all$Locality*Snakes_Anton_all$Age, las = 3, ylab = "Body size (SVL)", xlab = "", main = "Body size for each size class and population")
dev.off()

### Using lmodel2 package to allow for error in measuring both the x and y of GeoMean vs SVL
# Calculating min and max of log(SVL) for each population to start and stop regression line
Kang_min <- min(Snakes_Anton_Kang$LogSVL)
Kang_max <- max(Snakes_Anton_Kang$LogSVL)
Reev_min <- min(Snakes_Anton_Reev$LogSVL, na.rm = TRUE)
Reev_max <- max(Snakes_Anton_Reev$LogSVL, na.rm = TRUE)
SAMain_min <- min(Snakes_Anton_SAM$LogSVL, na.rm = TRUE)
SAMain_max <- max(Snakes_Anton_SAM$LogSVL, na.rm = TRUE)

# lmodel2 regression between log(head size) and log(body size)
Anton_Kang_LM <- lmodel2(LogGeoMean ~ LogSVL, data = Snakes_Anton_Kang, nperm = 10000)
Anton_Main_LM <- lmodel2(LogGeoMean ~ LogSVL, data = Snakes_Anton_SAM, nperm = 10000)
Anton_Reev_LM <- lmodel2(LogGeoMean ~ LogSVL, data = Snakes_Anton_Reev, nperm = 10000)

# Slope and intercept for each equation from lmodel2
Anton_Kang_LM$regression.results$Slope[[1]]
Anton_Main_LM$regression.results$Slope[[1]]
Anton_Reev_LM$regression.results$Slope[[1]]
Anton_Kang_LM$regression.results$Intercept[[1]]
Anton_Main_LM$regression.results$Intercept[[1]]
Anton_Reev_LM$regression.results$Intercept[[1]]

# Equation of lines to be plotted
equation_Ant_Kangaroo_LM <- function(x){Anton_Kang_LM$regression.results$Slope[[1]]*x+Anton_Kang_LM$regression.results$Intercept[[1]]}
equation_Ant_SAMainland_LM <- function(x){Anton_Main_LM$regression.results$Slope[[1]]*x+Anton_Main_LM$regression.results$Intercept[[1]]}
equation_Ant_Reevesby_LM <- function(x){Anton_Reev_LM$regression.results$Slope[[1]]*x+Anton_Reev_LM$regression.results$Intercept[[1]]}

# Text labels for plot
label1_Anton <- paste('Slope = ', round(Anton_Kang_LM$regression.results$Slope[[1]], digits = 3), sep = "")
label2_Anton <- paste('Slope = ', round(Anton_Reev_LM$regression.results$Slope[[1]], digits = 3), sep = "")
label3_Anton <- paste('Slope = ', round(Anton_Main_LM$regression.results$Slope[[1]], digits = 3), sep = "")


# Plotting head size vs body size
pdf('Anton_Geomean_SVL_interaction_LM.pdf')
ggplot(Snakes_Anton_all,aes(y=LogGeoMean,x=LogSVL,color=Locality))+ scale_color_manual(values=c("#999999","#E69F00","#56B4E9"))+
  geom_point(shape=1)+
  stat_function(fun=equation_Ant_Kangaroo_LM,geom="line",size=1, xlim=c(Kang_min, Kang_max), color="#56B4E9")+
  annotate(geom = "text", x=2.8, y=1.4, label=label1_Anton,color="#56B4E9")+
  stat_function(fun=equation_Ant_SAMainland_LM,geom="line",size=1, xlim=c(SAMain_min, SAMain_max), color="#999999")+
  annotate(geom = "text", x=3, y=1.1, label=label3_Anton,color="#999999")+
  stat_function(fun=equation_Ant_Reevesby_LM,geom="line",size=1, xlim=c(Reev_min, Reev_max), color="#E69F00")+
  annotate(geom = "text", x=2.25, y=1.1, label=label2_Anton,color="#E69F00")+
  ggtitle('Head size vs. body size for adult and juvenile \nTiger Snakes using Dataset 2')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Log (Body size)") + ylab("Log (Head size)")
#  geom_abline(slope = 0.1, intercept = 5, color = "black")
dev.off()

### Head shape vs. Log GeoMean

#### Other plots for both datasets
Anton_Adult_all$Size <- "Adult"
Anton_Neonate_all$Size <- "Neonate"
Anton_all <- rbind(Anton_Adult_all, Anton_Neonate_all)
Snakes_Fabien_Adult$Size <- "Adult"
Snakes_Fabien_Neonate$Size <- "Neonate"
Snakes_Fabien_all <- rbind(Snakes_Fabien_Adult, Snakes_Fabien_Neonate)

Anton_imp <- Anton_all[,c(19,2,20,15,16,17,18,21)]
colnames(Anton_imp) <- c("LogSVL", "Locality", "LogGeoMean", "LSR_R1", "LSR_R2","LSR_R4", "LSR_R5","Size")
Fabien_imp <- Snakes_Fabien_all[,c(19,2,20,15,16,17,18,21)]
colnames(Fabien_imp) <- c("LogSVL", "Locality", "LogGeoMean", "LSR_R1", "LSR_R2","LSR_R4", "LSR_R5", "Size")

Anton_imp$State <- "SA"
Fabien_imp$State <- "WA"
Fabien_imp_WA <- Fabien_imp[Fabien_imp$Locality == "WA mainland" | Fabien_imp$Locality == "Carnac Island (WA)",]
SA_subset <- Fabien_imp[Fabien_imp$Locality == "Williams Island (SA)",]
SA_subset$State <- "SA"
Fabien_imp_new <- rbind(Fabien_imp_WA, SA_subset)

both1 <- rbind(Anton_imp, Fabien_imp_new)
both1_adult <- both1[both1$Size == "Adult",]
both1_neonate <- both1[both1$Size == "Neonate",]

#### Adults
df_adult <- data.frame(both1_adult$LSR_R1, both1_adult$LSR_R2, both1_adult$LSR_R4, both1_adult$LSR_R5)
gdf_adult_LSR <- dudi.pca(df_adult, scannf = FALSE, nf = 2)

both1_adult$PC1 <- gdf_adult_LSR$li$Axis1

both1_adult_Reev <- both1_adult[both1_adult$Locality == "Reevesby Island (SA)",]
both1_adult_Kang <- both1_adult[both1_adult$Locality == "Kangaroo Island (SA)",]
both1_adult_SAMain <- both1_adult[both1_adult$Locality == "SA mainland",]
both1_adult_WAMain <- both1_adult[both1_adult$Locality == "WA mainland",]
both1_adult_Carn <- both1_adult[both1_adult$Locality == "Carnac Island (WA)",]
both1_adult_Will <- both1_adult[both1_adult$Locality == "Williams Island (SA)",]

df_neonate <- data.frame(both1_neonate$LSR_R1, both1_neonate$LSR_R2, both1_neonate$LSR_R4, both1_neonate$LSR_R5)
gdf_neonate_LSR <- dudi.pca(df_neonate, scannf = FALSE, nf = 2)

both1_neonate$PC1 <- gdf_neonate_LSR$li$Axis1

both1_neonate_Reev <- both1_neonate[both1_neonate$Locality == "Reevesby Island (SA)",]
both1_neonate_Kang <- both1_neonate[both1_neonate$Locality == "Kangaroo Island (SA)",]
both1_neonate_SAMain <- both1_neonate[both1_neonate$Locality == "SA mainland",]
both1_neonate_WAMain <- both1_neonate[both1_neonate$Locality == "WA mainland",]
both1_neonate_Carn <- both1_neonate[both1_neonate$Locality == "Carnac Island (WA)",]
both1_neonate_Will <- both1_neonate[both1_neonate$Locality == "Williams Island (SA)",]


Anton_Adult_Kang_LM_Geo_SVL <- lmodel2(PC1 ~ LogGeoMean, data = both1_adult_Kang, nperm = 10000)
Anton_Adult_Reev_LM_Geo_SVL <- lmodel2(PC1 ~ LogGeoMean, data = both1_adult_Reev, nperm = 10000)
Anton_Adult_Main_LM_Geo_SVL <- lmodel2(PC1 ~ LogGeoMean, data = both1_adult_SAMain, nperm = 10000)
Anton_Neonate_Kang_LM_Geo_SVL <- lmodel2(PC1 ~ LogGeoMean, data = both1_neonate_Kang, nperm = 10000)
Anton_Neonate_Reev_LM_Geo_SVL <- lmodel2(PC1 ~ LogGeoMean, data = both1_neonate_Reev, nperm = 10000)
Fabien_Adult_Carnac_LM_Geo_SVL <- lmodel2(PC1 ~ LogGeoMean, data = both1_adult_Carn, nperm = 10000)
Fabien_Adult_Will_LM_Geo_SVL <- lmodel2(PC1 ~ LogGeoMean, data = both1_adult_Will, nperm = 10000)
Fabien_Adult_Main_LM_Geo_SVL <- lmodel2(PC1 ~ LogGeoMean, data = both1_adult_WAMain, nperm = 10000)
Fabien_Neonate_Carnac_LM_Geo_SVL <- lmodel2(PC1 ~ LogGeoMean, data = both1_neonate_Carn, nperm = 10000)
Fabien_Neonate_Will_LM_Geo_SVL <- lmodel2(PC1 ~ LogGeoMean, data = both1_neonate_Will, nperm = 10000)
Fabien_Neonate_Main_LM_Geo_SVL <- lmodel2(PC1 ~ LogGeoMean, data = both1_neonate_WAMain, nperm = 10000)
Anton_Adult_Kang_LM_Geo_SVL$regression.results$Slope[[1]]
Anton_Adult_Reev_LM_Geo_SVL$regression.results$Slope[[1]]
Anton_Adult_Main_LM_Geo_SVL$regression.results$Slope[[1]]
Anton_Adult_Kang_LM_Geo_SVL$regression.results$Intercept[[1]]
Anton_Adult_Reev_LM_Geo_SVL$regression.results$Intercept[[1]]
Anton_Adult_Main_LM_Geo_SVL$regression.results$Intercept[[1]]
Anton_Neonate_Kang_LM_Geo_SVL$regression.results$Slope[[1]]
Anton_Neonate_Reev_LM_Geo_SVL$regression.results$Slope[[1]]
Anton_Neonate_Kang_LM_Geo_SVL$regression.results$Intercept[[1]]
Anton_Neonate_Reev_LM_Geo_SVL$regression.results$Intercept[[1]]
Fabien_Adult_Carnac_LM_Geo_SVL$regression.results$Slope[[1]]
Fabien_Adult_Will_LM_Geo_SVL$regression.results$Slope[[1]]
Fabien_Adult_Main_LM_Geo_SVL$regression.results$Slope[[1]]
Fabien_Neonate_Carnac_LM_Geo_SVL$regression.results$Slope[[1]]
Fabien_Neonate_Will_LM_Geo_SVL$regression.results$Slope[[1]]
Fabien_Neonate_Main_LM_Geo_SVL$regression.results$Slope[[1]]
Fabien_Adult_Carnac_LM_Geo_SVL$regression.results$Intercept[[1]]
Fabien_Adult_Will_LM_Geo_SVL$regression.results$Intercept[[1]]
Fabien_Adult_Main_LM_Geo_SVL$regression.results$Intercept[[1]]
Fabien_Neonate_Carnac_LM_Geo_SVL$regression.results$Intercept[[1]]
Fabien_Neonate_Will_LM_Geo_SVL$regression.results$Intercept[[1]]
Fabien_Neonate_Main_LM_Geo_SVL$regression.results$Intercept[[1]]


equation_Adult_Kangaroo_LM_Geo_SVL <- function(x){Anton_Adult_Kang_LM_Geo_SVL$regression.results$Slope[[1]]*x+Anton_Adult_Kang_LM_Geo_SVL$regression.results$Intercept[[1]]}
equation_Adult_Reevesby_LM_Geo_SVL <- function(x){Anton_Adult_Reev_LM_Geo_SVL$regression.results$Slope[[1]]*x+Anton_Adult_Reev_LM_Geo_SVL$regression.results$Intercept[[1]]}
equation_Adult_SAMainland_LM_Geo_SVL <- function(x){Anton_Adult_Main_LM_Geo_SVL$regression.results$Slope[[1]]*x+Anton_Adult_Main_LM_Geo_SVL$regression.results$Intercept[[1]]}
equation_Neonate_Kangaroo_LM_Geo_SVL <- function(x){Anton_Neonate_Kang_LM_Geo_SVL$regression.results$Slope[[1]]*x+Anton_Neonate_Kang_LM_Geo_SVL$regression.results$Intercept[[1]]}
equation_Neonate_Reevesby_LM_Geo_SVL <- function(x){Anton_Neonate_Reev_LM_Geo_SVL$regression.results$Slope[[1]]*x+Anton_Neonate_Reev_LM_Geo_SVL$regression.results$Intercept[[1]]}
equation_Adult_Carnac_LM_Geo_SVL <- function(x){Fabien_Adult_Carnac_LM_Geo_SVL$regression.results$Slope[[1]]*x+Fabien_Adult_Carnac_LM_Geo_SVL$regression.results$Intercept[[1]]}
equation_Adult_Williams_LM_Geo_SVL <- function(x){Fabien_Adult_Will_LM_Geo_SVL$regression.results$Slope[[1]]*x+Fabien_Adult_Will_LM_Geo_SVL$regression.results$Intercept[[1]]}
equation_Adult_WAMainland_LM_Geo_SVL <- function(x){Fabien_Adult_Main_LM_Geo_SVL$regression.results$Slope[[1]]*x+Fabien_Adult_Main_LM_Geo_SVL$regression.results$Intercept[[1]]}
equation_Neonate_Carnac_LM_Geo_SVL <- function(x){Fabien_Neonate_Carnac_LM_Geo_SVL$regression.results$Slope[[1]]*x+Fabien_Neonate_Carnac_LM_Geo_SVL$regression.results$Intercept[[1]]}
equation_Neonate_Williams_LM_Geo_SVL <- function(x){Fabien_Neonate_Will_LM_Geo_SVL$regression.results$Slope[[1]]*x+Fabien_Neonate_Will_LM_Geo_SVL$regression.results$Intercept[[1]]}
equation_Neonate_WAMainland_LM_Geo_SVL <- function(x){Fabien_Neonate_Main_LM_Geo_SVL$regression.results$Slope[[1]]*x+Fabien_Neonate_Main_LM_Geo_SVL$regression.results$Intercept[[1]]}

Kang_adult_min <- min(both1_adult_Kang$LogGeoMean)
Kang_adult_max <- max(both1_adult_Kang$LogGeoMean)
Reev_adult_min <- min(both1_adult_Reev$LogGeoMean)
Reev_adult_max <- max(both1_adult_Reev$LogGeoMean)
SAMain_adult_min <- min(both1_adult_SAMain$LogGeoMean)
SAMain_adult_max <- max(both1_adult_SAMain$LogGeoMean)
Carnac_adult_min <- min(both1_adult_Carn$LogGeoMean)
Carnac_adult_max <- max(both1_adult_Carn$LogGeoMean)
Will_adult_min <- min(both1_adult_Will$LogGeoMean)
Will_adult_max <- max(both1_adult_Will$LogGeoMean)
WAMain_adult_min <- min(both1_adult_WAMain$LogGeoMean)
WAMain_adult_max <- max(both1_adult_WAMain$LogGeoMean)

Kang_neonate_min <- min(both1_neonate_Kang$LogGeoMean)
Kang_neonate_max <- max(both1_neonate_Kang$LogGeoMean)
Reev_neonate_min <- min(both1_neonate_Reev$LogGeoMean)
Reev_neonate_max <- max(both1_neonate_Reev$LogGeoMean)
Carnac_neonate_min <- min(both1_neonate_Carn$LogGeoMean)
Carnac_neonate_max <- max(both1_neonate_Carn$LogGeoMean)
Will_neonate_min <- min(both1_neonate_Will$LogGeoMean)
Will_neonate_max <- max(both1_neonate_Will$LogGeoMean)
WAMain_neonate_min <- min(both1_neonate_WAMain$LogGeoMean)
WAMain_neonate_max <- max(both1_neonate_WAMain$LogGeoMean)


#### Adults
both1_adult_SA <- both1_adult[both1_adult$State == "SA",]
slope1 <- paste('Slope = ', round(Anton_Adult_Main_LM_Geo_SVL$regression.results$Slope[[1]], digits = 3), sep = "")
slope2 <- paste('Slope = ', round(Anton_Adult_Kang_LM_Geo_SVL$regression.results$Slope[[1]], digits = 3), sep = "")
slope3 <- paste('Slope = ', round(Anton_Adult_Reev_LM_Geo_SVL$regression.results$Slope[[1]], digits = 3), sep = "")
slope4 <- paste('Slope = ', round(Fabien_Adult_Will_LM_Geo_SVL$regression.results$Slope[[1]], digits = 3), sep = "")

pdf('Head_shape_vs_LogGeoMean_Adults_SA.pdf')
ggplot(both1_adult_SA, aes(y=PC1,x=LogGeoMean, color = Locality))+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#8DD3C7"))+
  geom_point(shape=1)+
  stat_function(fun=equation_Adult_Williams_LM_Geo_SVL,geom="line",size=1, xlim=c(Will_adult_min, Will_adult_max), color= "#8DD3C7")+
  annotate(geom = "text", x=1.3, y=-1.5, label=slope4,color= "#8DD3C7")+
  stat_function(fun=equation_Adult_Kangaroo_LM_Geo_SVL,geom="line",size=1, xlim=c(Kang_adult_min, Kang_adult_max), color="#56B4E9")+
  annotate(geom = "text", x=1.3, y=2.5, label=slope2,color="#56B4E9")+
  stat_function(fun=equation_Adult_Reevesby_LM_Geo_SVL,geom="line",size=1, xlim=c(Reev_adult_min, Reev_adult_max), color="#E69F00")+
  annotate(geom = "text", x=1.2, y=2, label=slope3,color="#E69F00")+
  stat_function(fun=equation_Adult_SAMainland_LM_Geo_SVL,geom="line",size=1, xlim=c(SAMain_adult_min, SAMain_adult_max), color="#999999")+
  annotate(geom = "text", x=1.2, y=0, label=slope1,color="#999999")+
  ggtitle('Head shape versus head size for adult Tiger Snakes from SA')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Head size")+ylab("Head shape")
dev.off()
both1_adult_WA <- both1_adult[both1_adult$State == "WA",]
slope1_FAB <- paste('Slope = ', round(Fabien_Adult_Main_LM_Geo_SVL$regression.results$Slope[[1]], digits = 3), sep = "")
slope2_FAB <- paste('Slope = ', round(Fabien_Adult_Carnac_LM_Geo_SVL$regression.results$Slope[[1]], digits = 3), sep = "")

pdf('Head_shape_vs_LogGeoMean_Adults_WA.pdf')
ggplot(both1_adult_WA, aes(y=PC1,x=LogGeoMean, color = Locality))+scale_color_manual(values=c("#999999", "#E69F00"))+
  geom_point(shape=1)+
  stat_function(fun=equation_Adult_Carnac_LM_Geo_SVL,geom="line",size=1, xlim=c(Carnac_adult_min, Carnac_adult_max), color="#E69F00")+
  annotate(geom = "text", x=1.25, y=-2.5, label=slope2_FAB,color="#E69F00")+
  stat_function(fun=equation_Adult_WAMainland_LM_Geo_SVL,geom="line",size=1, xlim=c(WAMain_adult_min, WAMain_adult_max), color="#999999")+
  annotate(geom = "text", x=1.25, y=1.3, label=slope1_FAB,color="#999999")+
  ggtitle('Head shape versus head size for adult Tiger Snakes from WA')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Head size")+ylab("Head shape")
dev.off()

### Neonates
both1_neonate_SA <- both1_neonate[both1_neonate$State == "SA",]
slope2_neo <- paste('Slope = ', round(Anton_Neonate_Kang_LM_Geo_SVL$regression.results$Slope[[1]], digits = 3), sep = "")
slope3_neo <- paste('Slope = ', round(Anton_Neonate_Reev_LM_Geo_SVL$regression.results$Slope[[1]], digits = 3), sep = "")
slope4_neo <- paste('Slope = ', round(Fabien_Neonate_Will_LM_Geo_SVL$regression.results$Slope[[1]], digits = 3), sep = "")

pdf('Head_shape_vs_LogGeoMean_Neonates_SA.pdf')
ggplot(both1_neonate_SA, aes(y=PC1,x=LogGeoMean, color = Locality))+scale_color_manual(values=c("#E69F00", "#56B4E9", "#8DD3C7"))+
  geom_point(shape=1)+
  stat_function(fun=equation_Neonate_Williams_LM_Geo_SVL,geom="line",size=1, xlim=c(Will_neonate_min, Will_neonate_max), color="#8DD3C7")+
  annotate(geom = "text", x=0.92, y=-2, label=slope4_neo,color="#8DD3C7")+
  stat_function(fun=equation_Neonate_Kangaroo_LM_Geo_SVL,geom="line",size=1, xlim=c(Kang_neonate_min, Kang_neonate_max), color="#56B4E9")+
  annotate(geom = "text", x=0.93, y=2, label=slope2_neo,color="#56B4E9")+
  stat_function(fun=equation_Neonate_Reevesby_LM_Geo_SVL,geom="line",size=1, xlim=c(Reev_neonate_min, Reev_neonate_max), color="#E69F00")+
  annotate(geom = "text", x=0.82, y=3, label=slope3_neo,color="#E69F00")+
  ggtitle('Head shape versus head size for neonate Tiger Snakes in SA')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Head size")+ylab("Head shape")
dev.off()

both1_neonate_WA <- both1_neonate[both1_neonate$State == "WA",]
slope3_neo_FAB <- paste('Slope = ', round(Fabien_Neonate_Carnac_LM_Geo_SVL$regression.results$Slope[[1]], digits = 3), sep = "")
slope4_neo_FAB <- paste('Slope = ', round(Fabien_Neonate_Will_LM_Geo_SVL$regression.results$Slope[[1]], digits = 3), sep = "")
pdf('Head_shape_vs_LogGeoMean_Neonates_WA.pdf')
ggplot(both1_neonate_WA, aes(y=PC1,x=LogGeoMean, color = Locality))+scale_color_manual(values=c("#999999", "#E69F00"))+
  geom_point(shape=1)+
  stat_function(fun=equation_Neonate_Carnac_LM_Geo_SVL,geom="line",size=1, xlim=c(Carnac_neonate_min, Carnac_neonate_max), color="#E69F00")+
  annotate(geom = "text", x=0.8, y=1, label=slope3_neo_FAB,color="#E69F00")+
  stat_function(fun=equation_Neonate_WAMainland_LM_Geo_SVL,geom="line",size=1, xlim=c(WAMain_neonate_min, WAMain_neonate_max), color="#999999")+
  annotate(geom = "text", x=0.85, y=-5, label=slope4_neo_FAB,color="#999999")+
  ggtitle('Head shape versus head size for neonate Tiger Snakes in WA')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Head size")+ylab("Head shape")
dev.off()






