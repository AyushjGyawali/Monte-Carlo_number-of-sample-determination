#random sampling and power #phos #beeville
rm(list = ls())
library(dplyr)
library(beanplot)
library(ggplot2)
library(broom)
library(Rmisc)
library(nlme)
library(lme4)
library(lmerTest)
library(multcomp)
library(emmeans)
setwd("C:/Users/ayush.joshigyawali/Dropbox/Ayush_TAMU/enzyme study/jacob/R/input_csv")

#importing inrow and between row data separately 
#have to manually put the plot number before importing

# poxc_ir<- read.csv("poxc_inrow.csv")
# poxc_br <- read.csv ("poxc_betweenrow.csv")

data <- read.csv ("EnzymeCompiledData_june242021.csv")

colnames(data) <- c("sampleid",
                    "location",
                    "plotno",
                    "Tillage",
                    "Crop.Type",
                    "rowposition",
                    "bgluc",
                    "nag",
                    "phos")

data <- data[which(data$location == "Beeville"),]
data <- data[,-c(7,8,10,11)]
#data$Plot.no. <- as.character(data$Plot.no.)
#data$bgluc <- as.numeric(data$bgluc)
#data$nag <- as.numeric (data$nag)
data$phos <- as.numeric(as.character(data$phos))
data$phos <- pmax(data$phos,0)
data <- droplevels(data)
data_inrow <- data[which(data$rowposition == "In Row"),]
data_betrow <- data[which(data$rowposition == "Out Row"),]

#seperating into specific treatments
#inrow
nt_f_ir <- data_inrow[which(data_inrow$Tillage == "NT" & data_inrow$Crop.Type == "Fallow"),]
nt_s_ir <- data_inrow[which(data_inrow$Tillage == "NT" & data_inrow$Crop.Type == "Sorghum"),]
nt_m_ir <- data_inrow[which(data_inrow$Tillage == "NT" & data_inrow$Crop.Type == "Cover Crop"),]
ct_f_ir <- data_inrow[which(data_inrow$Tillage == "CT" & data_inrow$Crop.Type == "Fallow"),]
ct_s_ir <- data_inrow[which(data_inrow$Tillage == "CT" & data_inrow$Crop.Type == "Sorghum"),]
ct_m_ir <- data_inrow[which(data_inrow$Tillage == "CT" & data_inrow$Crop.Type == "Cover Crop"),]

#betweenrow
nt_f_br <- data_betrow[which(data_betrow$Tillage == "NT" & data_betrow$Crop.Type == "Fallow"),]
nt_s_br <- data_betrow[which(data_betrow$Tillage == "NT" & data_betrow$Crop.Type == "Sorghum"),]
nt_m_br <- data_betrow[which(data_betrow$Tillage == "NT" & data_betrow$Crop.Type == "Cover Crop"),]
ct_f_br <- data_betrow[which(data_betrow$Tillage == "CT" & data_betrow$Crop.Type == "Fallow"),]
ct_s_br <- data_betrow[which(data_betrow$Tillage == "CT" & data_betrow$Crop.Type == "Sorghum"),]
ct_m_br <- data_betrow[which(data_betrow$Tillage == "CT" & data_betrow$Crop.Type == "Cover Crop"),]

#separating into each plot
#inrow
nt_f_ir_1 <- nt_f_ir[which(nt_f_ir$plotno == 12),]
nt_f_ir_2 <- nt_f_ir[which(nt_f_ir$plotno == 16),]
nt_f_ir_3 <- nt_f_ir[which(nt_f_ir$plotno == 38),]

nt_s_ir_1 <- nt_s_ir[which(nt_s_ir$plotno == 11),]
nt_s_ir_2 <- nt_s_ir[which(nt_s_ir$plotno == 20),]
nt_s_ir_3 <- nt_s_ir[which(nt_s_ir$plotno == 39),]

nt_m_ir_1 <- nt_m_ir[which(nt_m_ir$plotno == 13),]
nt_m_ir_2 <- nt_m_ir[which(nt_m_ir$plotno == 17),]
nt_m_ir_3 <- nt_m_ir[which(nt_m_ir$plotno == 37),]

ct_f_ir_1 <- ct_f_ir[which(ct_f_ir$plotno == 5),]
ct_f_ir_2 <- ct_f_ir[which(ct_f_ir$plotno == 22),]
ct_f_ir_3 <- ct_f_ir[which(ct_f_ir$plotno == 31),]

ct_s_ir_1 <- ct_s_ir[which(ct_s_ir$plotno == 2),]
ct_s_ir_2 <- ct_s_ir[which(ct_s_ir$plotno == 25),]
ct_s_ir_3 <- ct_s_ir[which(ct_s_ir$plotno == 32),]

ct_m_ir_1 <- ct_m_ir[which(ct_m_ir$plotno == 3),]
ct_m_ir_2 <- ct_m_ir[which(ct_m_ir$plotno == 24),]
ct_m_ir_3 <- ct_m_ir[which(ct_m_ir$plotno == 33),]


#betweenrow
nt_f_br_1 <- nt_f_br[which(nt_f_br$plotno == 12),]
nt_f_br_2 <- nt_f_br[which(nt_f_br$plotno == 16),]
nt_f_br_3 <- nt_f_br[which(nt_f_br$plotno == 38),]

nt_s_br_1 <- nt_s_br[which(nt_s_br$plotno == 11),]
nt_s_br_2 <- nt_s_br[which(nt_s_br$plotno == 20),]
nt_s_br_3 <- nt_s_br[which(nt_s_br$plotno == 39),]

nt_m_br_1 <- nt_m_br[which(nt_m_br$plotno == 13),]
nt_m_br_2 <- nt_m_br[which(nt_m_br$plotno == 17),]
nt_m_br_3 <- nt_m_br[which(nt_m_br$plotno == 37),]

ct_f_br_1 <- ct_f_br[which(ct_f_br$plotno == 5),]
ct_f_br_2 <- ct_f_br[which(ct_f_br$plotno == 22),]
ct_f_br_3 <- ct_f_br[which(ct_f_br$plotno == 31),]

ct_s_br_1 <- ct_s_br[which(ct_s_br$plotno == 2),]
ct_s_br_2 <- ct_s_br[which(ct_s_br$plotno == 25),]
ct_s_br_3 <- ct_s_br[which(ct_s_br$plotno == 32),]

ct_m_br_1 <- ct_m_br[which(ct_m_br$plotno == 3),]
ct_m_br_2 <- ct_m_br[which(ct_m_br$plotno == 24),]
ct_m_br_3 <- ct_m_br[which(ct_m_br$plotno == 33),]


#inrow###########################################################
################################################################

#sample 1#########################################################################################

Tillage_1 <- c (rep ("nt",9), rep ("ct", 9))
Tillage_1 <- as.factor(Tillage_1)
cover_type_1 <- c(rep("fallow",3), rep("sorghum",3), rep("mix",3),
                  
                  rep("fallow",3), rep("sorghum",3), rep("mix",3))
cover_type_1 <- as.factor(cover_type_1)
Rep <- rep((1:3), 6)
Rep <- as.factor(Rep)

# sample1_trt <- data.frame (tillage = Tillage_1, cover_type = cover_type_1, Rep = Rep )
# sample1_trt$Rep <- as.factor(sample1_trt$Rep)


sample1 <- matrix(ncol = 10000, nrow = 18)
sample1_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample1[1, i] = sample (nt_f_ir_1$phos, 1, replace = T)
  sample1[2, i] = sample (nt_f_ir_2$phos, 1, replace = T)
  sample1[3, i] = sample (nt_f_ir_3$phos, 1, replace = T)
  sample1[4, i] = sample (nt_s_ir_1$phos, 1, replace = T)
  sample1[5, i] = sample (nt_s_ir_2$phos, 1, replace = T)
  sample1[6, i] = sample (nt_s_ir_3$phos, 1, replace = T)
  sample1[7, i] = sample (nt_m_ir_1$phos, 1, replace = T)
  sample1[8, i] = sample (nt_m_ir_2$phos, 1, replace = T)
  sample1[9, i] = sample (nt_m_ir_3$phos, 1, replace = T)
  
  sample1[10, i] = sample (ct_f_ir_1$phos, 1, replace = T)
  sample1[11, i] = sample (ct_f_ir_2$phos, 1, replace = T)
  sample1[12, i] = sample (ct_f_ir_3$phos, 1, replace = T)
  sample1[13, i] = sample (ct_s_ir_1$phos, 1, replace = T)
  sample1[14, i] = sample (ct_s_ir_2$phos, 1, replace = T)
  sample1[15, i] = sample (ct_s_ir_3$phos, 1, replace = T)
  sample1[16, i] = sample (ct_m_ir_1$phos, 1, replace = T)
  sample1[17, i] = sample (ct_m_ir_2$phos, 1, replace = T)
  sample1[18, i] = sample (ct_m_ir_3$phos, 1, replace = T)
  
  sample_1 <- data.frame (phos = sample1[,i], tillage = Tillage_1, covercrop = cover_type_1, Rep = Rep)
  
  tillage_sum_ir <- summarySE(data = sample_1, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_ir <- summarySE(data = sample_1, measurevar = "phos", groupvars = "covercrop")
  combo_sum_ir <- summarySE(data = sample_1, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_power <- p$n
  tillage_sampleno <- tillage_power/9
  
  sample1_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_power <- p$n
  cover_type_sampleno <- cover_type_power/6
  
  sample1_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- sd_groupmean/overallsd
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_power <- p$n
  combo_sampleno <- combo_power/3
  
  sample1_power [3,i] <- combo_sampleno
  
}

#sample2########################################################################################
Tillage_2 <- c (rep ("nt",18), rep ("ct", 18))
Tillage_2 <- as.factor(Tillage_2)
cover_type_2 <- c(rep("fallow",6), rep("sorghum",6), rep("mix",6),
                  
                  rep("fallow",6), rep("sorghum",6), rep("mix",6))
cover_type_2 <- as.factor(cover_type_2)
Rep <- rep(c(1,1,2,2,3,3), 6)
Rep <- as.factor(Rep)

# sample2_trt <- data.frame (tillage = Tillage_2, cover_type = cover_type_2, Rep = Rep )
# sample2_trt$Rep <- as.factor(sample2_trt$Rep)


sample2 <- matrix(ncol = 10000, nrow = 36)
sample2_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample2[1:2, i] = sample (nt_f_ir_1$phos, 2, replace = T)
  sample2[3:4, i] = sample (nt_f_ir_2$phos, 2, replace = T)
  sample2[5:6, i] = sample (nt_f_ir_3$phos, 2, replace = T)
  sample2[7:8, i] = sample (nt_s_ir_1$phos, 2, replace = T)
  sample2[9:10, i] = sample (nt_s_ir_2$phos, 2, replace = T)
  sample2[11:12, i] = sample (nt_s_ir_3$phos, 2, replace = T)
  sample2[13:14, i] = sample (nt_m_ir_1$phos, 2, replace = T)
  sample2[15:16, i] = sample (nt_m_ir_2$phos, 2, replace = T)
  sample2[17:18, i] = sample (nt_m_ir_3$phos, 2, replace = T)
  
  sample2[19:20, i] = sample (ct_f_ir_1$phos, 2, replace = T)
  sample2[21:22, i] = sample (ct_f_ir_2$phos, 2, replace = T)
  sample2[23:24, i] = sample (ct_f_ir_3$phos, 2, replace = T)
  sample2[25:26, i] = sample (ct_s_ir_1$phos, 2, replace = T)
  sample2[27:28, i] = sample (ct_s_ir_2$phos, 2, replace = T)
  sample2[29:30, i] = sample (ct_s_ir_3$phos, 2, replace = T)
  sample2[31:32, i] = sample (ct_m_ir_1$phos, 2, replace = T)
  sample2[33:34, i] = sample (ct_m_ir_2$phos, 2, replace = T)
  sample2[35:36, i] = sample (ct_m_ir_3$phos, 2, replace = T)
  
  sample_2 <- data.frame (phos = sample2[,i], tillage = Tillage_2, covercrop = cover_type_2, Rep = Rep)
  
  tillage_sum_ir <- summarySE(data = sample_2, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_ir <- summarySE(data = sample_2, measurevar = "phos", groupvars = "covercrop")
  combo_sum_ir <- summarySE(data = sample_2, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_power <- p$n
  tillage_sampleno <- tillage_power/9
  
  sample2_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_power <- p$n
  cover_type_sampleno <- cover_type_power/6
  
  sample2_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- sd_groupmean/overallsd
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_power <- p$n
  combo_sampleno <- combo_power/3
  
  sample2_power [3,i] <- combo_sampleno
}

#sample3########################################################################################
Tillage_3 <- c (rep ("nt",27), rep ("ct", 27))
cover_type_3 <- c(rep("fallow",9), rep("sorghum",9), rep("mix",9),
                  
                  rep("fallow",9), rep("sorghum",9), rep("mix",9))
Rep <- rep(c(1,1,1,2,2,2,3,3,3), 6)
Rep <- as.factor(Rep)

sample3_trt <- data.frame (tillage = Tillage_3, cover_type = cover_type_3, Rep = Rep )
sample3_trt$Rep <- as.factor(sample3_trt$Rep)


sample3 <- matrix(ncol = 10000, nrow = 54)
sample3_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample3[1:3, i] = sample (nt_f_ir_1$phos, 3, replace = T)
  sample3[4:6, i] = sample (nt_f_ir_2$phos, 3, replace = T)
  sample3[7:9, i] = sample (nt_f_ir_3$phos, 3, replace = T)
  sample3[10:12, i] = sample (nt_s_ir_1$phos, 3, replace = T)
  sample3[13:15, i] = sample (nt_s_ir_2$phos, 3, replace = T)
  sample3[16:18, i] = sample (nt_s_ir_3$phos, 3, replace = T)
  sample3[19:21, i] = sample (nt_m_ir_1$phos, 3, replace = T)
  sample3[22:24, i] = sample (nt_m_ir_2$phos, 3, replace = T)
  sample3[25:27, i] = sample (nt_m_ir_3$phos, 3, replace = T)
  
  sample3[28:30, i] = sample (ct_f_ir_1$phos, 3, replace = T)
  sample3[31:33, i] = sample (ct_f_ir_2$phos, 3, replace = T)
  sample3[34:36, i] = sample (ct_f_ir_3$phos, 3, replace = T)
  sample3[37:39, i] = sample (ct_s_ir_1$phos, 3, replace = T)
  sample3[40:42, i] = sample (ct_s_ir_2$phos, 3, replace = T)
  sample3[43:45, i] = sample (ct_s_ir_3$phos, 3, replace = T)
  sample3[46:48, i] = sample (ct_m_ir_1$phos, 3, replace = T)
  sample3[49:51, i] = sample (ct_m_ir_2$phos, 3, replace = T)
  sample3[52:54, i] = sample (ct_m_ir_3$phos, 3, replace = T)
  
  sample_3 <- data.frame (phos = sample3[,i], tillage = Tillage_3, covercrop = cover_type_3, Rep = Rep)
  
  tillage_sum_ir <- summarySE(data = sample_3, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_ir <- summarySE(data = sample_3, measurevar = "phos", groupvars = "covercrop")
  combo_sum_ir <- summarySE(data = sample_3, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_power <- p$n
  tillage_sampleno <- tillage_power/9
  
  sample3_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_power <- p$n
  cover_type_sampleno <- cover_type_power/6
  
  sample3_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- sd_groupmean/overallsd
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_power <- p$n
  combo_sampleno <- combo_power/3
  
  sample3_power [3,i] <- combo_sampleno
  
}

#sample4########################################################################################
Tillage_4 <- c (rep ("nt",36), rep ("ct", 36))
cover_type_4 <- c(rep("fallow",12), rep("sorghum",12), rep("mix",12),
                  
                  rep("fallow",12), rep("sorghum",12), rep("mix",12))
Rep <- rep(c(1,1,1,1,2,2,2,2,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample4_trt <- data.frame (tillage = Tillage_4, cover_type = cover_type_4, Rep = Rep )
sample4_trt$Rep <- as.factor(sample4_trt$Rep)


sample4 <- matrix(ncol = 10000, nrow = 72)
sample4_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample4[1:4, i] = sample (nt_f_ir_1$phos, 4, replace = T)
  sample4[5:8, i] = sample (nt_f_ir_2$phos, 4, replace = T)
  sample4[9:12, i] = sample (nt_f_ir_3$phos, 4, replace = T)
  sample4[13:16, i] = sample (nt_s_ir_1$phos, 4, replace = T)
  sample4[17:20, i] = sample (nt_s_ir_2$phos, 4, replace = T)
  sample4[21:24, i] = sample (nt_s_ir_3$phos, 4, replace = T)
  sample4[25:28, i] = sample (nt_m_ir_1$phos, 4, replace = T)
  sample4[29:32, i] = sample (nt_m_ir_2$phos, 4, replace = T)
  sample4[33:36, i] = sample (nt_m_ir_3$phos, 4, replace = T)
  
  sample4[37:40, i] = sample (ct_f_ir_1$phos, 4, replace = T)
  sample4[41:44, i] = sample (ct_f_ir_2$phos, 4, replace = T)
  sample4[45:48, i] = sample (ct_f_ir_3$phos, 4, replace = T)
  sample4[49:52, i] = sample (ct_s_ir_1$phos, 4, replace = T)
  sample4[53:56, i] = sample (ct_s_ir_2$phos, 4, replace = T)
  sample4[57:60, i] = sample (ct_s_ir_3$phos, 4, replace = T)
  sample4[61:64, i] = sample (ct_m_ir_1$phos, 4, replace = T)
  sample4[65:68, i] = sample (ct_m_ir_2$phos, 4, replace = T)
  sample4[69:72, i] = sample (ct_m_ir_3$phos, 4, replace = T)
  
  sample_4 <- data.frame (phos = sample4[,i], tillage = Tillage_4, covercrop = cover_type_4, 
                          Rep = Rep)
  tillage_sum_ir <- summarySE(data = sample_4, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_ir <- summarySE(data = sample_4, measurevar = "phos", groupvars = "covercrop")
  combo_sum_ir <- summarySE(data = sample_4, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_power <- p$n
  tillage_sampleno <- tillage_power/9
  
  sample4_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_power <- p$n
  cover_type_sampleno <- cover_type_power/6
  
  sample4_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- sd_groupmean/overallsd
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_power <- p$n
  combo_sampleno <- combo_power/3
  
  sample4_power [3,i] <- combo_sampleno
  
}

#sample5########################################################################################
Tillage_5 <- c (rep ("nt",45), rep ("ct", 45))
cover_type_5 <- c(rep("fallow",15), rep("sorghum",15), rep("mix",15),
                  
                  rep("fallow",15), rep("sorghum",15), rep("mix",15))
Rep <- rep(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample5_trt <- data.frame (tillage = Tillage_5, cover_type = cover_type_5, Rep = Rep )
sample5_trt$Rep <- as.factor(sample5_trt$Rep)


sample5 <- matrix(ncol = 10000, nrow = 90)
sample5_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample5[1:5, i] = sample (nt_f_ir_1$phos, 5, replace = T)
  sample5[6:10, i] = sample (nt_f_ir_2$phos, 5, replace = T)
  sample5[11:15, i] = sample (nt_f_ir_3$phos, 5, replace = T)
  sample5[16:20, i] = sample (nt_s_ir_1$phos, 5, replace = T)
  sample5[21:25, i] = sample (nt_s_ir_2$phos, 5, replace = T)
  sample5[26:30, i] = sample (nt_s_ir_3$phos, 5, replace = T)
  sample5[31:35, i] = sample (nt_m_ir_1$phos, 5, replace = T)
  sample5[36:40, i] = sample (nt_m_ir_2$phos, 5, replace = T)
  sample5[41:45, i] = sample (nt_m_ir_3$phos, 5, replace = T)
  
  sample5[46:50, i] = sample (ct_f_ir_1$phos, 5, replace = T)
  sample5[51:55, i] = sample (ct_f_ir_2$phos, 5, replace = T)
  sample5[56:60, i] = sample (ct_f_ir_3$phos, 5, replace = T)
  sample5[61:65, i] = sample (ct_s_ir_1$phos, 5, replace = T)
  sample5[66:70, i] = sample (ct_s_ir_2$phos, 5, replace = T)
  sample5[71:75, i] = sample (ct_s_ir_3$phos, 5, replace = T)
  sample5[76:80, i] = sample (ct_m_ir_1$phos, 5, replace = T)
  sample5[81:85, i] = sample (ct_m_ir_2$phos, 5, replace = T)
  sample5[86:90, i] = sample (ct_m_ir_3$phos, 5, replace = T)
  
  sample_5 <- data.frame (phos = sample5[,i], tillage = Tillage_5, covercrop = cover_type_5, Rep= Rep)
  
  tillage_sum_ir <- summarySE(data = sample_5, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_ir <- summarySE(data = sample_5, measurevar = "phos", groupvars = "covercrop")
  combo_sum_ir <- summarySE(data = sample_5, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_power <- p$n
  tillage_sampleno <- tillage_power/9
  
  sample5_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_power <- p$n
  cover_type_sampleno <- cover_type_power/6
  
  sample5_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- sd_groupmean/overallsd
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_power <- p$n
  combo_sampleno <- combo_power/3
  
  sample5_power [3,i] <- combo_sampleno
}

#sample6########################################################################################
Tillage_6 <- c (rep ("nt",54), rep ("ct", 54))
cover_type_6 <- c(rep("fallow",18), rep("sorghum",18), rep("mix",18),
                  
                  rep("fallow",18), rep("sorghum",18), rep("mix",18))
Rep <- rep(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample6_trt <- data.frame (tillage = Tillage_6, cover_type = cover_type_6, Rep = Rep )
sample6_trt$Rep <- as.factor(sample6_trt$Rep)


sample6 <- matrix(ncol = 10000, nrow = 108)
sample6_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample6[1:6, i] = sample (nt_f_ir_1$phos, 6, replace = T)
  sample6[7:12, i] = sample (nt_f_ir_2$phos, 6, replace = T)
  sample6[13:18, i] = sample (nt_f_ir_3$phos, 6, replace = T)
  sample6[19:24, i] = sample (nt_s_ir_1$phos, 6, replace = T)
  sample6[25:30, i] = sample (nt_s_ir_2$phos, 6, replace = T)
  sample6[31:36, i] = sample (nt_s_ir_3$phos, 6, replace = T)
  sample6[37:42, i] = sample (nt_m_ir_1$phos, 6, replace = T)
  sample6[43:48, i] = sample (nt_m_ir_2$phos, 6, replace = T)
  sample6[49:54, i] = sample (nt_m_ir_3$phos, 6, replace = T)
  
  sample6[55:60, i] = sample (ct_f_ir_1$phos, 6, replace = T)
  sample6[61:66, i] = sample (ct_f_ir_2$phos, 6, replace = T)
  sample6[67:72, i] = sample (ct_f_ir_3$phos, 6, replace = T)
  sample6[73:78, i] = sample (ct_s_ir_1$phos, 6, replace = T)
  sample6[79:84, i] = sample (ct_s_ir_2$phos, 6, replace = T)
  sample6[85:90, i] = sample (ct_s_ir_3$phos, 6, replace = T)
  sample6[91:96, i] = sample (ct_m_ir_1$phos, 6, replace = T)
  sample6[97:102, i] = sample (ct_m_ir_2$phos, 6, replace = T)
  sample6[103:108, i] = sample (ct_m_ir_3$phos, 6, replace = T)
  
  sample_6 <- data.frame (phos = sample6[,i], tillage = Tillage_6, covercrop = cover_type_6, Rep = Rep)
  
  tillage_sum_ir <- summarySE(data = sample_6, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_ir <- summarySE(data = sample_6, measurevar = "phos", groupvars = "covercrop")
  combo_sum_ir <- summarySE(data = sample_6, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_power <- p$n
  tillage_sampleno <- tillage_power/9
  
  sample6_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_power <- p$n
  cover_type_sampleno <- cover_type_power/6
  
  sample6_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- sd_groupmean/overallsd
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_power <- p$n
  combo_sampleno <- combo_power/3
  
  sample6_power [3,i] <- combo_sampleno
}

#sample7########################################################################################
Tillage_7 <- c (rep ("nt",63), rep ("ct", 63))
cover_type_7 <- c(rep("fallow",21), rep("sorghum",21), rep("mix",21),
                  
                  rep("fallow",21), rep("sorghum",21), rep("mix",21))
Rep <- rep(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample7_trt <- data.frame (tillage = Tillage_7, cover_type = cover_type_7, Rep = Rep )
sample7_trt$Rep <- as.factor(sample7_trt$Rep)


sample7 <- matrix(ncol = 10000, nrow = 126)
sample7_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample7[1:7, i] = sample (nt_f_ir_1$phos, 7, replace = T)
  sample7[8:14, i] = sample (nt_f_ir_2$phos, 7, replace = T)
  sample7[15:21, i] = sample (nt_f_ir_3$phos, 7, replace = T)
  sample7[22:28, i] = sample (nt_s_ir_1$phos, 7, replace = T)
  sample7[29:35, i] = sample (nt_s_ir_2$phos, 7, replace = T)
  sample7[36:42, i] = sample (nt_s_ir_3$phos, 7, replace = T)
  sample7[43:49, i] = sample (nt_m_ir_1$phos, 7, replace = T)
  sample7[50:56, i] = sample (nt_m_ir_2$phos, 7, replace = T)
  sample7[57:63, i] = sample (nt_m_ir_3$phos, 7, replace = T)
  
  sample7[64:70, i] = sample (ct_f_ir_1$phos, 7, replace = T)
  sample7[71:77, i] = sample (ct_f_ir_2$phos, 7, replace = T)
  sample7[78:84, i] = sample (ct_f_ir_3$phos, 7, replace = T)
  sample7[85:91, i] = sample (ct_s_ir_1$phos, 7, replace = T)
  sample7[92:98, i] = sample (ct_s_ir_2$phos, 7, replace = T)
  sample7[99:105, i] = sample (ct_s_ir_3$phos, 7, replace = T)
  sample7[106:112, i] = sample (ct_m_ir_1$phos, 7, replace = T)
  sample7[113:119, i] = sample (ct_m_ir_2$phos, 7, replace = T)
  sample7[120:126, i] = sample (ct_m_ir_3$phos, 7, replace = T)
  
  sample_7 <- data.frame (phos = sample7[,i], tillage = Tillage_7, covercrop = cover_type_7, Rep=Rep)
  
  tillage_sum_ir <- summarySE(data = sample_7, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_ir <- summarySE(data = sample_7, measurevar = "phos", groupvars = "covercrop")
  combo_sum_ir <- summarySE(data = sample_7, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_power <- p$n
  tillage_sampleno <- tillage_power/9
  
  sample7_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_power <- p$n
  cover_type_sampleno <- cover_type_power/6
  
  sample7_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- sd_groupmean/overallsd
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_power <- p$n
  combo_sampleno <- combo_power/3
  
  sample7_power [3,i] <- combo_sampleno
}

#sample8########################################################################################
Tillage_8 <- c (rep ("nt",72), rep ("ct", 72))
cover_type_8 <- c(rep("fallow",24), rep("sorghum",24), rep("mix",24),
                  
                  rep("fallow",24), rep("sorghum",24), rep("mix",24))
Rep <- rep(c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample8_trt <- data.frame (tillage = Tillage_8, cover_type = cover_type_8, Rep = Rep )
sample8_trt$Rep <- as.factor(sample8_trt$Rep)


sample8 <- matrix(ncol = 10000, nrow = 144)
sample8_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample8[1:8, i] = sample (nt_f_ir_1$phos, 8, replace = T)
  sample8[9:16, i] = sample (nt_f_ir_2$phos, 8, replace = T)
  sample8[17:24, i] = sample (nt_f_ir_3$phos, 8, replace = T)
  sample8[25:32, i] = sample (nt_s_ir_1$phos, 8, replace = T)
  sample8[33:40, i] = sample (nt_s_ir_2$phos, 8, replace = T)
  sample8[41:48, i] = sample (nt_s_ir_3$phos, 8, replace = T)
  sample8[49:56, i] = sample (nt_m_ir_1$phos, 8, replace = T)
  sample8[57:64, i] = sample (nt_m_ir_2$phos, 8, replace = T)
  sample8[65:72, i] = sample (nt_m_ir_3$phos, 8, replace = T)
  
  sample8[73:80, i] = sample (ct_f_ir_1$phos, 8, replace = T)
  sample8[81:88, i] = sample (ct_f_ir_2$phos, 8, replace = T)
  sample8[89:96, i] = sample (ct_f_ir_3$phos, 8, replace = T)
  sample8[97:104, i] = sample (ct_s_ir_1$phos, 8, replace = T)
  sample8[105:112, i] = sample (ct_s_ir_2$phos, 8, replace = T)
  sample8[113:120, i] = sample (ct_s_ir_3$phos, 8, replace = T)
  sample8[121:128, i] = sample (ct_m_ir_1$phos, 8, replace = T)
  sample8[129:136, i] = sample (ct_m_ir_2$phos, 8, replace = T)
  sample8[137:144, i] = sample (ct_m_ir_3$phos, 8, replace = T)
  
  sample_8 <- data.frame (phos = sample8[,i], tillage = Tillage_8, covercrop = cover_type_8, 
                          Rep = Rep)
  
  tillage_sum_ir <- summarySE(data = sample_8, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_ir <- summarySE(data = sample_8, measurevar = "phos", groupvars = "covercrop")
  combo_sum_ir <- summarySE(data = sample_8, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_power <- p$n
  tillage_sampleno <- tillage_power/9
  
  sample8_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_power <- p$n
  cover_type_sampleno <- cover_type_power/6
  
  sample8_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- sd_groupmean/overallsd
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_power <- p$n
  combo_sampleno <- combo_power/3
  
  sample8_power [3,i] <- combo_sampleno
}

#sample9########################################################################################
Tillage_9 <- c (rep ("nt",81), rep ("ct", 81))
cover_type_9 <- c(rep("fallow",27), rep("sorghum",27), rep("mix",27),
                  
                  rep("fallow",27), rep("sorghum",27), rep("mix",27))
Rep <- rep(c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample9_trt <- data.frame (tillage = Tillage_9, cover_type = cover_type_9, Rep = Rep )
sample9_trt$Rep <- as.factor(sample9_trt$Rep)


sample9 <- matrix(ncol = 10000, nrow = 162)
sample9_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample9[1:9, i] = sample (nt_f_ir_1$phos, 9, replace = T)
  sample9[10:18, i] = sample (nt_f_ir_2$phos, 9, replace = T)
  sample9[19:27, i] = sample (nt_f_ir_3$phos, 9, replace = T)
  sample9[28:36, i] = sample (nt_s_ir_1$phos, 9, replace = T)
  sample9[37:45, i] = sample (nt_s_ir_2$phos, 9, replace = T)
  sample9[46:54, i] = sample (nt_s_ir_3$phos, 9, replace = T)
  sample9[55:63, i] = sample (nt_m_ir_1$phos, 9, replace = T)
  sample9[64:72, i] = sample (nt_m_ir_2$phos, 9, replace = T)
  sample9[73:81, i] = sample (nt_m_ir_3$phos, 9, replace = T)
  
  sample9[82:90, i] = sample (ct_f_ir_1$phos, 9, replace = T)
  sample9[91:99, i] = sample (ct_f_ir_2$phos, 9, replace = T)
  sample9[100:108, i] = sample (ct_f_ir_3$phos, 9, replace = T)
  sample9[109:117, i] = sample (ct_s_ir_1$phos, 9, replace = T)
  sample9[118:126, i] = sample (ct_s_ir_2$phos, 9, replace = T)
  sample9[127:135, i] = sample (ct_s_ir_3$phos, 9, replace = T)
  sample9[136:144, i] = sample (ct_m_ir_1$phos, 9, replace = T)
  sample9[145:153, i] = sample (ct_m_ir_2$phos, 9, replace = T)
  sample9[154:162, i] = sample (ct_m_ir_3$phos, 9, replace = T)
  
  sample_9 <- data.frame (phos = sample9[,i], tillage = Tillage_9, covercrop = cover_type_9, Rep=Rep)
  
  tillage_sum_ir <- summarySE(data = sample_9, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_ir <- summarySE(data = sample_9, measurevar = "phos", groupvars = "covercrop")
  combo_sum_ir <- summarySE(data = sample_9, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_power <- p$n
  tillage_sampleno <- tillage_power/9
  
  sample9_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_power <- p$n
  cover_type_sampleno <- cover_type_power/6
  
  sample9_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_ir$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- sd_groupmean/overallsd
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_power <- p$n
  combo_sampleno <- combo_power/3
  
  sample9_power [3,i] <- combo_sampleno
}

###################################################################
#between row
###################################################################


#sample 1#########################################################################################

Tillage_1 <- c (rep ("nt",9), rep ("ct", 9))
Tillage_1 <- as.factor(Tillage_1)
cover_type_1 <- c(rep("fallow",3), rep("sorghum",3), rep("mix",3),
                  
                  rep("fallow",3), rep("sorghum",3), rep("mix",3))
cover_type_1 <- as.factor(cover_type_1)
Rep <- rep((1:3), 6)
Rep <- as.factor(Rep)

# sample1_trt <- data.frame (tillage = Tillage_1, cover_type = cover_type_1, Rep = Rep )
# sample1_trt$Rep <- as.factor(sample1_trt$Rep)


sample1 <- matrix(ncol = 10000, nrow = 18)
sample1_br_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample1[1, i] = sample (nt_f_br_1$phos, 1, replace = T)
  sample1[2, i] = sample (nt_f_br_2$phos, 1, replace = T)
  sample1[3, i] = sample (nt_f_br_3$phos, 1, replace = T)
  sample1[4, i] = sample (nt_s_br_1$phos, 1, replace = T)
  sample1[5, i] = sample (nt_s_br_2$phos, 1, replace = T)
  sample1[6, i] = sample (nt_s_br_3$phos, 1, replace = T)
  sample1[7, i] = sample (nt_m_br_1$phos, 1, replace = T)
  sample1[8, i] = sample (nt_m_br_2$phos, 1, replace = T)
  sample1[9, i] = sample (nt_m_br_3$phos, 1, replace = T)
  
  sample1[10, i] = sample (ct_f_br_1$phos, 1, replace = T)
  sample1[11, i] = sample (ct_f_br_2$phos, 1, replace = T)
  sample1[12, i] = sample (ct_f_br_3$phos, 1, replace = T)
  sample1[13, i] = sample (ct_s_br_1$phos, 1, replace = T)
  sample1[14, i] = sample (ct_s_br_2$phos, 1, replace = T)
  sample1[15, i] = sample (ct_s_br_3$phos, 1, replace = T)
  sample1[16, i] = sample (ct_m_br_1$phos, 1, replace = T)
  sample1[17, i] = sample (ct_m_br_2$phos, 1, replace = T)
  sample1[18, i] = sample (ct_m_br_3$phos, 1, replace = T)
  
  sample_1 <- data.frame (phos = sample1[,i], tillage = Tillage_1, covercrop = cover_type_1, Rep = Rep)
  
  tillage_sum_br <- summarySE(data = sample_1, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_br <- summarySE(data = sample_1, measurevar = "phos", groupvars = "covercrop")
  combo_sum_br <- summarySE(data = sample_1, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_br_br_power <- p$n
  tillage_sampleno <- tillage_br_br_power/9
  
  sample1_br_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_br_br_power <- p$n
  cover_type_sampleno <- cover_type_br_br_power/6
  
  sample1_br_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_br_br_power <- p$n
  combo_sampleno <- combo_br_br_power/3
  
  sample1_br_power [3,i] <- combo_sampleno
  
}

#sample2########################################################################################
Tillage_2 <- c (rep ("nt",18), rep ("ct", 18))
Tillage_2 <- as.factor(Tillage_2)
cover_type_2 <- c(rep("fallow",6), rep("sorghum",6), rep("mix",6),
                  
                  rep("fallow",6), rep("sorghum",6), rep("mix",6))
cover_type_2 <- as.factor(cover_type_2)
Rep <- rep(c(1,1,2,2,3,3), 6)
Rep <- as.factor(Rep)

# sample2_trt <- data.frame (tillage = Tillage_2, cover_type = cover_type_2, Rep = Rep )
# sample2_trt$Rep <- as.factor(sample2_trt$Rep)


sample2 <- matrix(ncol = 10000, nrow = 36)
sample2_br_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample2[1:2, i] = sample (nt_f_br_1$phos, 2, replace = T)
  sample2[3:4, i] = sample (nt_f_br_2$phos, 2, replace = T)
  sample2[5:6, i] = sample (nt_f_br_3$phos, 2, replace = T)
  sample2[7:8, i] = sample (nt_s_br_1$phos, 2, replace = T)
  sample2[9:10, i] = sample (nt_s_br_2$phos, 2, replace = T)
  sample2[11:12, i] = sample (nt_s_br_3$phos, 2, replace = T)
  sample2[13:14, i] = sample (nt_m_br_1$phos, 2, replace = T)
  sample2[15:16, i] = sample (nt_m_br_2$phos, 2, replace = T)
  sample2[17:18, i] = sample (nt_m_br_3$phos, 2, replace = T)
  
  sample2[19:20, i] = sample (ct_f_br_1$phos, 2, replace = T)
  sample2[21:22, i] = sample (ct_f_br_2$phos, 2, replace = T)
  sample2[23:24, i] = sample (ct_f_br_3$phos, 2, replace = T)
  sample2[25:26, i] = sample (ct_s_br_1$phos, 2, replace = T)
  sample2[27:28, i] = sample (ct_s_br_2$phos, 2, replace = T)
  sample2[29:30, i] = sample (ct_s_br_3$phos, 2, replace = T)
  sample2[31:32, i] = sample (ct_m_br_1$phos, 2, replace = T)
  sample2[33:34, i] = sample (ct_m_br_2$phos, 2, replace = T)
  sample2[35:36, i] = sample (ct_m_br_3$phos, 2, replace = T)
  
  sample_2 <- data.frame (phos = sample2[,i], tillage = Tillage_2, covercrop = cover_type_2, Rep = Rep)
  
  tillage_sum_br <- summarySE(data = sample_2, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_br <- summarySE(data = sample_2, measurevar = "phos", groupvars = "covercrop")
  combo_sum_br <- summarySE(data = sample_2, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_br_power <- p$n
  tillage_sampleno <- tillage_br_power/9
  
  sample2_br_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_br_power <- p$n
  cover_type_sampleno <- cover_type_br_power/6
  
  sample2_br_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_br_power <- p$n
  combo_sampleno <- combo_br_power/3
  
  sample2_br_power [3,i] <- combo_sampleno
}

#sample3########################################################################################
Tillage_3 <- c (rep ("nt",27), rep ("ct", 27))
cover_type_3 <- c(rep("fallow",9), rep("sorghum",9), rep("mix",9),
                  
                  rep("fallow",9), rep("sorghum",9), rep("mix",9))
Rep <- rep(c(1,1,1,2,2,2,3,3,3), 6)
Rep <- as.factor(Rep)

sample3_trt <- data.frame (tillage = Tillage_3, cover_type = cover_type_3, Rep = Rep )
sample3_trt$Rep <- as.factor(sample3_trt$Rep)


sample3 <- matrix(ncol = 10000, nrow = 54)
sample3_br_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample3[1:3, i] = sample (nt_f_br_1$phos, 3, replace = T)
  sample3[4:6, i] = sample (nt_f_br_2$phos, 3, replace = T)
  sample3[7:9, i] = sample (nt_f_br_3$phos, 3, replace = T)
  sample3[10:12, i] = sample (nt_s_br_1$phos, 3, replace = T)
  sample3[13:15, i] = sample (nt_s_br_2$phos, 3, replace = T)
  sample3[16:18, i] = sample (nt_s_br_3$phos, 3, replace = T)
  sample3[19:21, i] = sample (nt_m_br_1$phos, 3, replace = T)
  sample3[22:24, i] = sample (nt_m_br_2$phos, 3, replace = T)
  sample3[25:27, i] = sample (nt_m_br_3$phos, 3, replace = T)
  
  sample3[28:30, i] = sample (ct_f_br_1$phos, 3, replace = T)
  sample3[31:33, i] = sample (ct_f_br_2$phos, 3, replace = T)
  sample3[34:36, i] = sample (ct_f_br_3$phos, 3, replace = T)
  sample3[37:39, i] = sample (ct_s_br_1$phos, 3, replace = T)
  sample3[40:42, i] = sample (ct_s_br_2$phos, 3, replace = T)
  sample3[43:45, i] = sample (ct_s_br_3$phos, 3, replace = T)
  sample3[46:48, i] = sample (ct_m_br_1$phos, 3, replace = T)
  sample3[49:51, i] = sample (ct_m_br_2$phos, 3, replace = T)
  sample3[52:54, i] = sample (ct_m_br_3$phos, 3, replace = T)
  
  sample_3 <- data.frame (phos = sample3[,i], tillage = Tillage_3, covercrop = cover_type_3, Rep = Rep)
  
  tillage_sum_br <- summarySE(data = sample_3, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_br <- summarySE(data = sample_3, measurevar = "phos", groupvars = "covercrop")
  combo_sum_br <- summarySE(data = sample_3, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_br_power <- p$n
  tillage_sampleno <- tillage_br_power/9
  
  sample3_br_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_br_power <- p$n
  cover_type_sampleno <- cover_type_br_power/6
  
  sample3_br_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_br_power <- p$n
  combo_sampleno <- combo_br_power/3
  
  sample3_br_power [3,i] <- combo_sampleno
  
}

#sample4########################################################################################
Tillage_4 <- c (rep ("nt",36), rep ("ct", 36))
cover_type_4 <- c(rep("fallow",12), rep("sorghum",12), rep("mix",12),
                  
                  rep("fallow",12), rep("sorghum",12), rep("mix",12))
Rep <- rep(c(1,1,1,1,2,2,2,2,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample4_trt <- data.frame (tillage = Tillage_4, cover_type = cover_type_4, Rep = Rep )
sample4_trt$Rep <- as.factor(sample4_trt$Rep)


sample4 <- matrix(ncol = 10000, nrow = 72)
sample4_br_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample4[1:4, i] = sample (nt_f_br_1$phos, 4, replace = T)
  sample4[5:8, i] = sample (nt_f_br_2$phos, 4, replace = T)
  sample4[9:12, i] = sample (nt_f_br_3$phos, 4, replace = T)
  sample4[13:16, i] = sample (nt_s_br_1$phos, 4, replace = T)
  sample4[17:20, i] = sample (nt_s_br_2$phos, 4, replace = T)
  sample4[21:24, i] = sample (nt_s_br_3$phos, 4, replace = T)
  sample4[25:28, i] = sample (nt_m_br_1$phos, 4, replace = T)
  sample4[29:32, i] = sample (nt_m_br_2$phos, 4, replace = T)
  sample4[33:36, i] = sample (nt_m_br_3$phos, 4, replace = T)
  
  sample4[37:40, i] = sample (ct_f_br_1$phos, 4, replace = T)
  sample4[41:44, i] = sample (ct_f_br_2$phos, 4, replace = T)
  sample4[45:48, i] = sample (ct_f_br_3$phos, 4, replace = T)
  sample4[49:52, i] = sample (ct_s_br_1$phos, 4, replace = T)
  sample4[53:56, i] = sample (ct_s_br_2$phos, 4, replace = T)
  sample4[57:60, i] = sample (ct_s_br_3$phos, 4, replace = T)
  sample4[61:64, i] = sample (ct_m_br_1$phos, 4, replace = T)
  sample4[65:68, i] = sample (ct_m_br_2$phos, 4, replace = T)
  sample4[69:72, i] = sample (ct_m_br_3$phos, 4, replace = T)
  
  sample_4 <- data.frame (phos = sample4[,i], tillage = Tillage_4, covercrop = cover_type_4, 
                          Rep = Rep)
  tillage_sum_br <- summarySE(data = sample_4, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_br <- summarySE(data = sample_4, measurevar = "phos", groupvars = "covercrop")
  combo_sum_br <- summarySE(data = sample_4, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_br_power <- p$n
  tillage_sampleno <- tillage_br_power/9
  
  sample4_br_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_br_power <- p$n
  cover_type_sampleno <- cover_type_br_power/6
  
  sample4_br_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_br_power <- p$n
  combo_sampleno <- combo_br_power/3
  
  sample4_br_power [3,i] <- combo_sampleno
  
}

#sample5########################################################################################
Tillage_5 <- c (rep ("nt",45), rep ("ct", 45))
cover_type_5 <- c(rep("fallow",15), rep("sorghum",15), rep("mix",15),
                  
                  rep("fallow",15), rep("sorghum",15), rep("mix",15))
Rep <- rep(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample5_trt <- data.frame (tillage = Tillage_5, cover_type = cover_type_5, Rep = Rep )
sample5_trt$Rep <- as.factor(sample5_trt$Rep)


sample5 <- matrix(ncol = 10000, nrow = 90)
sample5_br_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample5[1:5, i] = sample (nt_f_br_1$phos, 5, replace = T)
  sample5[6:10, i] = sample (nt_f_br_2$phos, 5, replace = T)
  sample5[11:15, i] = sample (nt_f_br_3$phos, 5, replace = T)
  sample5[16:20, i] = sample (nt_s_br_1$phos, 5, replace = T)
  sample5[21:25, i] = sample (nt_s_br_2$phos, 5, replace = T)
  sample5[26:30, i] = sample (nt_s_br_3$phos, 5, replace = T)
  sample5[31:35, i] = sample (nt_m_br_1$phos, 5, replace = T)
  sample5[36:40, i] = sample (nt_m_br_2$phos, 5, replace = T)
  sample5[41:45, i] = sample (nt_m_br_3$phos, 5, replace = T)
  
  sample5[46:50, i] = sample (ct_f_br_1$phos, 5, replace = T)
  sample5[51:55, i] = sample (ct_f_br_2$phos, 5, replace = T)
  sample5[56:60, i] = sample (ct_f_br_3$phos, 5, replace = T)
  sample5[61:65, i] = sample (ct_s_br_1$phos, 5, replace = T)
  sample5[66:70, i] = sample (ct_s_br_2$phos, 5, replace = T)
  sample5[71:75, i] = sample (ct_s_br_3$phos, 5, replace = T)
  sample5[76:80, i] = sample (ct_m_br_1$phos, 5, replace = T)
  sample5[81:85, i] = sample (ct_m_br_2$phos, 5, replace = T)
  sample5[86:90, i] = sample (ct_m_br_3$phos, 5, replace = T)
  
  sample_5 <- data.frame (phos = sample5[,i], tillage = Tillage_5, covercrop = cover_type_5, Rep= Rep)
  
  tillage_sum_br <- summarySE(data = sample_5, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_br <- summarySE(data = sample_5, measurevar = "phos", groupvars = "covercrop")
  combo_sum_br <- summarySE(data = sample_5, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_br_power <- p$n
  tillage_sampleno <- tillage_br_power/9
  
  sample5_br_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_br_power <- p$n
  cover_type_sampleno <- cover_type_br_power/6
  
  sample5_br_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_br_power <- p$n
  combo_sampleno <- combo_br_power/3
  
  sample5_br_power [3,i] <- combo_sampleno
}

#sample6########################################################################################
Tillage_6 <- c (rep ("nt",54), rep ("ct", 54))
cover_type_6 <- c(rep("fallow",18), rep("sorghum",18), rep("mix",18),
                  
                  rep("fallow",18), rep("sorghum",18), rep("mix",18))
Rep <- rep(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample6_trt <- data.frame (tillage = Tillage_6, cover_type = cover_type_6, Rep = Rep )
sample6_trt$Rep <- as.factor(sample6_trt$Rep)


sample6 <- matrix(ncol = 10000, nrow = 108)
sample6_br_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample6[1:6, i] = sample (nt_f_br_1$phos, 6, replace = T)
  sample6[7:12, i] = sample (nt_f_br_2$phos, 6, replace = T)
  sample6[13:18, i] = sample (nt_f_br_3$phos, 6, replace = T)
  sample6[19:24, i] = sample (nt_s_br_1$phos, 6, replace = T)
  sample6[25:30, i] = sample (nt_s_br_2$phos, 6, replace = T)
  sample6[31:36, i] = sample (nt_s_br_3$phos, 6, replace = T)
  sample6[37:42, i] = sample (nt_m_br_1$phos, 6, replace = T)
  sample6[43:48, i] = sample (nt_m_br_2$phos, 6, replace = T)
  sample6[49:54, i] = sample (nt_m_br_3$phos, 6, replace = T)
  
  sample6[55:60, i] = sample (ct_f_br_1$phos, 6, replace = T)
  sample6[61:66, i] = sample (ct_f_br_2$phos, 6, replace = T)
  sample6[67:72, i] = sample (ct_f_br_3$phos, 6, replace = T)
  sample6[73:78, i] = sample (ct_s_br_1$phos, 6, replace = T)
  sample6[79:84, i] = sample (ct_s_br_2$phos, 6, replace = T)
  sample6[85:90, i] = sample (ct_s_br_3$phos, 6, replace = T)
  sample6[91:96, i] = sample (ct_m_br_1$phos, 6, replace = T)
  sample6[97:102, i] = sample (ct_m_br_2$phos, 6, replace = T)
  sample6[103:108, i] = sample (ct_m_br_3$phos, 6, replace = T)
  
  sample_6 <- data.frame (phos = sample6[,i], tillage = Tillage_6, covercrop = cover_type_6, Rep = Rep)
  
  tillage_sum_br <- summarySE(data = sample_6, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_br <- summarySE(data = sample_6, measurevar = "phos", groupvars = "covercrop")
  combo_sum_br <- summarySE(data = sample_6, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_br_power <- p$n
  tillage_sampleno <- tillage_br_power/9
  
  sample6_br_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_br_power <- p$n
  cover_type_sampleno <- cover_type_br_power/6
  
  sample6_br_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_br_power <- p$n
  combo_sampleno <- combo_br_power/3
  
  sample6_br_power [3,i] <- combo_sampleno
}

#sample7########################################################################################
Tillage_7 <- c (rep ("nt",63), rep ("ct", 63))
cover_type_7 <- c(rep("fallow",21), rep("sorghum",21), rep("mix",21),
                  
                  rep("fallow",21), rep("sorghum",21), rep("mix",21))
Rep <- rep(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample7_trt <- data.frame (tillage = Tillage_7, cover_type = cover_type_7, Rep = Rep )
sample7_trt$Rep <- as.factor(sample7_trt$Rep)


sample7 <- matrix(ncol = 10000, nrow = 126)
sample7_br_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample7[1:7, i] = sample (nt_f_br_1$phos, 7, replace = T)
  sample7[8:14, i] = sample (nt_f_br_2$phos, 7, replace = T)
  sample7[15:21, i] = sample (nt_f_br_3$phos, 7, replace = T)
  sample7[22:28, i] = sample (nt_s_br_1$phos, 7, replace = T)
  sample7[29:35, i] = sample (nt_s_br_2$phos, 7, replace = T)
  sample7[36:42, i] = sample (nt_s_br_3$phos, 7, replace = T)
  sample7[43:49, i] = sample (nt_m_br_1$phos, 7, replace = T)
  sample7[50:56, i] = sample (nt_m_br_2$phos, 7, replace = T)
  sample7[57:63, i] = sample (nt_m_br_3$phos, 7, replace = T)
  
  sample7[64:70, i] = sample (ct_f_br_1$phos, 7, replace = T)
  sample7[71:77, i] = sample (ct_f_br_2$phos, 7, replace = T)
  sample7[78:84, i] = sample (ct_f_br_3$phos, 7, replace = T)
  sample7[85:91, i] = sample (ct_s_br_1$phos, 7, replace = T)
  sample7[92:98, i] = sample (ct_s_br_2$phos, 7, replace = T)
  sample7[99:105, i] = sample (ct_s_br_3$phos, 7, replace = T)
  sample7[106:112, i] = sample (ct_m_br_1$phos, 7, replace = T)
  sample7[113:119, i] = sample (ct_m_br_2$phos, 7, replace = T)
  sample7[120:126, i] = sample (ct_m_br_3$phos, 7, replace = T)
  
  sample_7 <- data.frame (phos = sample7[,i], tillage = Tillage_7, covercrop = cover_type_7, Rep=Rep)
  
  tillage_sum_br <- summarySE(data = sample_7, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_br <- summarySE(data = sample_7, measurevar = "phos", groupvars = "covercrop")
  combo_sum_br <- summarySE(data = sample_7, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_br_power <- p$n
  tillage_sampleno <- tillage_br_power/9
  
  sample7_br_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_br_power <- p$n
  cover_type_sampleno <- cover_type_br_power/6
  
  sample7_br_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_br_power <- p$n
  combo_sampleno <- combo_br_power/3
  
  sample7_br_power [3,i] <- combo_sampleno
}

#sample8########################################################################################
Tillage_8 <- c (rep ("nt",72), rep ("ct", 72))
cover_type_8 <- c(rep("fallow",24), rep("sorghum",24), rep("mix",24),
                  
                  rep("fallow",24), rep("sorghum",24), rep("mix",24))
Rep <- rep(c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample8_trt <- data.frame (tillage = Tillage_8, cover_type = cover_type_8, Rep = Rep )
sample8_trt$Rep <- as.factor(sample8_trt$Rep)


sample8 <- matrix(ncol = 10000, nrow = 144)
sample8_br_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample8[1:8, i] = sample (nt_f_br_1$phos, 8, replace = T)
  sample8[9:16, i] = sample (nt_f_br_2$phos, 8, replace = T)
  sample8[17:24, i] = sample (nt_f_br_3$phos, 8, replace = T)
  sample8[25:32, i] = sample (nt_s_br_1$phos, 8, replace = T)
  sample8[33:40, i] = sample (nt_s_br_2$phos, 8, replace = T)
  sample8[41:48, i] = sample (nt_s_br_3$phos, 8, replace = T)
  sample8[49:56, i] = sample (nt_m_br_1$phos, 8, replace = T)
  sample8[57:64, i] = sample (nt_m_br_2$phos, 8, replace = T)
  sample8[65:72, i] = sample (nt_m_br_3$phos, 8, replace = T)
  
  sample8[73:80, i] = sample (ct_f_br_1$phos, 8, replace = T)
  sample8[81:88, i] = sample (ct_f_br_2$phos, 8, replace = T)
  sample8[89:96, i] = sample (ct_f_br_3$phos, 8, replace = T)
  sample8[97:104, i] = sample (ct_s_br_1$phos, 8, replace = T)
  sample8[105:112, i] = sample (ct_s_br_2$phos, 8, replace = T)
  sample8[113:120, i] = sample (ct_s_br_3$phos, 8, replace = T)
  sample8[121:128, i] = sample (ct_m_br_1$phos, 8, replace = T)
  sample8[129:136, i] = sample (ct_m_br_2$phos, 8, replace = T)
  sample8[137:144, i] = sample (ct_m_br_3$phos, 8, replace = T)
  
  sample_8 <- data.frame (phos = sample8[,i], tillage = Tillage_8, covercrop = cover_type_8, 
                          Rep = Rep)
  
  tillage_sum_br <- summarySE(data = sample_8, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_br <- summarySE(data = sample_8, measurevar = "phos", groupvars = "covercrop")
  combo_sum_br <- summarySE(data = sample_8, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_br_power <- p$n
  tillage_sampleno <- tillage_br_power/9
  
  sample8_br_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_br_power <- p$n
  cover_type_sampleno <- cover_type_br_power/6
  
  sample8_br_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_br_power <- p$n
  combo_sampleno <- combo_br_power/3
  
  sample8_br_power [3,i] <- combo_sampleno
}

#sample9########################################################################################
Tillage_9 <- c (rep ("nt",81), rep ("ct", 81))
cover_type_9 <- c(rep("fallow",27), rep("sorghum",27), rep("mix",27),
                  
                  rep("fallow",27), rep("sorghum",27), rep("mix",27))
Rep <- rep(c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample9_trt <- data.frame (tillage = Tillage_9, cover_type = cover_type_9, Rep = Rep )
sample9_trt$Rep <- as.factor(sample9_trt$Rep)


sample9 <- matrix(ncol = 10000, nrow = 162)
sample9_br_power <- matrix(ncol = 10000, nrow = 3)

for(i in 1:10000) {
  
  
  sample9[1:9, i] = sample (nt_f_br_1$phos, 9, replace = T)
  sample9[10:18, i] = sample (nt_f_br_2$phos, 9, replace = T)
  sample9[19:27, i] = sample (nt_f_br_3$phos, 9, replace = T)
  sample9[28:36, i] = sample (nt_s_br_1$phos, 9, replace = T)
  sample9[37:45, i] = sample (nt_s_br_2$phos, 9, replace = T)
  sample9[46:54, i] = sample (nt_s_br_3$phos, 9, replace = T)
  sample9[55:63, i] = sample (nt_m_br_1$phos, 9, replace = T)
  sample9[64:72, i] = sample (nt_m_br_2$phos, 9, replace = T)
  sample9[73:81, i] = sample (nt_m_br_3$phos, 9, replace = T)
  
  sample9[82:90, i] = sample (ct_f_br_1$phos, 9, replace = T)
  sample9[91:99, i] = sample (ct_f_br_2$phos, 9, replace = T)
  sample9[100:108, i] = sample (ct_f_br_3$phos, 9, replace = T)
  sample9[109:117, i] = sample (ct_s_br_1$phos, 9, replace = T)
  sample9[118:126, i] = sample (ct_s_br_2$phos, 9, replace = T)
  sample9[127:135, i] = sample (ct_s_br_3$phos, 9, replace = T)
  sample9[136:144, i] = sample (ct_m_br_1$phos, 9, replace = T)
  sample9[145:153, i] = sample (ct_m_br_2$phos, 9, replace = T)
  sample9[154:162, i] = sample (ct_m_br_3$phos, 9, replace = T)
  
  sample_9 <- data.frame (phos = sample9[,i], tillage = Tillage_9, covercrop = cover_type_9, Rep=Rep)
  
  tillage_sum_br <- summarySE(data = sample_9, measurevar = "phos", groupvars = "tillage")
  cover_type_sum_br <- summarySE(data = sample_9, measurevar = "phos", groupvars = "covercrop")
  combo_sum_br <- summarySE(data = sample_9, measurevar = "phos", groupvars = c("tillage",
                                                                                "covercrop"))
  groupmeans <- tillage_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  overallsd <- sd(data_inrow$phos)
  
  f <- 0.25
  
  p <- pwr.anova.test(k = 2, f = f, sig.level = 0.05, power = 0.8)
  tillage_br_power <- p$n
  tillage_sampleno <- tillage_br_power/9
  
  sample9_br_power [1,i] <- tillage_sampleno
  
  groupmeans <- cover_type_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 3, f = f, sig.level = 0.05, power = 0.8)
  cover_type_br_power <- p$n
  cover_type_sampleno <- cover_type_br_power/6
  
  sample9_br_power [2,i] <- cover_type_sampleno
  
  groupmeans <- combo_sum_br$phos
  sd_groupmean <- sd(groupmeans)
  
  f <- 0.25
  p <- pwr.anova.test(k = 6, f = f, sig.level = 0.05, power = 0.8)
  combo_br_power <- p$n
  combo_sampleno <- combo_br_power/3
  
  sample9_br_power [3,i] <- combo_sampleno
}

#compiling

ir_1_tillage <- sample1_power[1,]
ir_1_interraction <- sample1_power[3,]
ir_1_covercrop <- sample1_power[2,]

ir_2_tillage <- sample2_power[1,]
ir_2_interraction <- sample2_power[3,]
ir_2_covercrop <- sample2_power[2,]


ir_3_tillage <- sample3_power[1,]
ir_3_interraction <- sample3_power[3,]
ir_3_covercrop <- sample3_power[2,]

ir_4_tillage <- sample4_power[1,]
ir_4_interraction <- sample4_power[3,]
ir_4_covercrop <- sample4_power[2,]

ir_5_tillage <- sample5_power[1,]
ir_5_interraction <- sample5_power[3,]
ir_5_covercrop <- sample5_power[2,]

ir_6_tillage <- sample6_power[1,]
ir_6_interraction <- sample6_power[3,]
ir_6_covercrop <- sample6_power[2,]

ir_7_tillage <- sample7_power[1,]
ir_7_interraction <- sample7_power[3,]
ir_7_covercrop <- sample7_power[2,]

ir_8_tillage <- sample8_power[1,]
ir_8_interraction <- sample8_power[3,]
ir_8_covercrop <- sample8_power[2,]

ir_9_tillage <- sample9_power[1,]
ir_9_interraction <- sample9_power[3,]
ir_9_covercrop <- sample9_power[2,]


#br###########################################

br_1_tillage <- sample1_br_power[1,]
br_1_interraction <- sample1_br_power[3,]
br_1_covercrop <- sample1_br_power[2,]

br_2_tillage <- sample2_br_power[1,]
br_2_interraction <- sample2_br_power[3,]
br_2_covercrop <- sample2_br_power[2,]


br_3_tillage <- sample3_br_power[1,]
br_3_interraction <- sample3_br_power[3,]
br_3_covercrop <- sample3_br_power[2,]

br_4_tillage <- sample4_br_power[1,]
br_4_interraction <- sample4_br_power[3,]
br_4_covercrop <- sample4_br_power[2,]

br_5_tillage <- sample5_br_power[1,]
br_5_interraction <- sample5_br_power[3,]
br_5_covercrop <- sample5_br_power[2,]

br_6_tillage <- sample6_br_power[1,]
br_6_interraction <- sample6_br_power[3,]
br_6_covercrop <- sample6_br_power[2,]

br_7_tillage <- sample7_br_power[1,]
br_7_interraction <- sample7_br_power[3,]
br_7_covercrop <- sample7_br_power[2,]

br_8_tillage <- sample8_br_power[1,]
br_8_interraction <- sample8_br_power[3,]
br_8_covercrop <- sample8_br_power[2,]

br_9_tillage <- sample9_br_power[1,]
br_9_interraction <- sample9_br_power[3,]
br_9_covercrop <- sample9_br_power[2,]


tillage_power <- cbind(
  t(ir_1_tillage),
  t(ir_2_tillage),
  t(ir_3_tillage),
  t(ir_4_tillage),
  t(ir_5_tillage),
  t(ir_6_tillage),
  t(ir_7_tillage),
  t(ir_8_tillage),
  t(ir_9_tillage),
  t(br_1_tillage),
  t(br_2_tillage),
  t(br_3_tillage),
  t(br_4_tillage),
  t(br_5_tillage),
  t(br_6_tillage),
  t(br_7_tillage),
  t(br_8_tillage),
  t(br_9_tillage)
)

tillage_power <- t(tillage_power)

cover_power <- cbind(
  t(ir_1_covercrop),
  t(ir_2_covercrop),
  t(ir_3_covercrop),
  t(ir_4_covercrop),
  t(ir_5_covercrop),
  t(ir_6_covercrop),
  t(ir_7_covercrop),
  t(ir_8_covercrop),
  t(ir_9_covercrop),
  t(br_1_covercrop),
  t(br_2_covercrop),
  t(br_3_covercrop),
  t(br_4_covercrop),
  t(br_5_covercrop),
  t(br_6_covercrop),
  t(br_7_covercrop),
  t(br_8_covercrop),
  t(br_9_covercrop)
)

cover_power <- t(cover_power)

interraction_power <- cbind(
  t(ir_1_interraction),
  t(ir_2_interraction),
  t(ir_3_interraction),
  t(ir_4_interraction),
  t(ir_5_interraction),
  t(ir_6_interraction),
  t(ir_7_interraction),
  t(ir_8_interraction),
  t(ir_9_interraction),
  t(br_1_interraction),
  t(br_2_interraction),
  t(br_3_interraction),
  t(br_4_interraction),
  t(br_5_interraction),
  t(br_6_interraction),
  t(br_7_interraction),
  t(br_8_interraction),
  t(br_9_interraction)
)

interraction_power <- t(interraction_power)

one <- c(rep(1,10000))
two <- c(rep(2,10000))
three <- c(rep(3,10000))
four <- c(rep(4,10000))
five <- c(rep(5,10000))
six <- c(rep(6,10000))
seven <- c(rep(7,10000))
eight <- c(rep(8,10000))
nine <- c(rep(9,10000))



sample_no <- cbind(t(one), t(two), t(three), t(four), t(five), t(six), t(seven), t(eight), t(nine),
                   t(one), t(two), t(three), t(four), t(five), t(six), t(seven), t(eight), t(nine))
sample_no <- t(sample_no)

row <- c (rep ("In Row",90000), rep ("Between Row", 90000))
row <- t(row)
row <- t(row)

compiled <- data.frame(
  sample_no,
  row,
  tillage_power,
  cover_power,
  interraction_power
)

compiled$sample_no <- as.factor(compiled$sample_no)

mean_tillage <- summarySE(compiled, measurevar = "tillage_power", groupvars = c("sample_no","row"))
mean_cover <- summarySE(compiled, measurevar = "cover_power", groupvars = c("sample_no", "row"))
mean_interraction <- summarySE(compiled, measurevar = "interraction_power", groupvars = c("sample_no", "row"))

#exporting summary files
write.csv(mean_tillage, "beeville_phos_mean_tillage_power.csv")
write.csv(mean_cover, "beeville_phos_mean_cover_power.csv")
write.csv(mean_interraction, "beeville_phos_mean_interraction_power.csv")

#exporting_compiled
write.csv(compiled, "beeville_phos_allsimulation_power.csv")

#plot

tillage_plot <- ggplot(compiled, aes(x= sample_no, y= tillage_power, fill = row))

tillage_plot+geom_boxplot(aes(fill = row),
                          position = position_dodge(0.9))+
  
  geom_hline(yintercept = 0.05)+
  stat_summary(fun= mean, color = "red", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 3,
               show.legend = FALSE)+
  theme_classic() +
  
  scale_y_continuous(limits = c(0,25))+
  
  theme(plot.margin = unit(c(2,2,2,2), units="points"), ##good legend size
        
        #                                       panel.border = element_rect(color = "black",
        
        #                                                                 fill = NA, size = 1.75),
        
        axis.text.y = element_text(size = 16),  ## change font size of y axis
        
        axis.title.y = element_text(size = 16),
        
        axis.text.x = element_text(size = 16),  ## change font size of y axis
        
        axis.title.x = element_text(size = 16),
        
        legend.margin = margin(0, 7, 10, 0, "point"), ##remove margin on legend
        
        legend.text = element_text(size = 15),
        
        legend.position = c(0.85, 0.85),
        
        legend.key=element_blank(),  ##removed gray border around legend symbols
        
        legend.key.size = unit(7, "mm"),
        
        #legend.background = element_rect(color = "black", size = 1.25), #legend.box
        
        legend.key.height = unit(7, "mm"),
        
        legend.key.width = unit(7, "mm"),
        
        #                                         legend.box.spacing = unit(10, "mm"),
        
        legend.text.align = 0,
        
        axis.line.x = element_line(color = "black", size = 1.75),
        
        
        axis.ticks.x= element_line(size = 1.75, color = "black"),
        
        axis.line.y = element_line(color = "black", size = 1.75),
        
        axis.ticks.y = element_line(size = 1.75, color = "black"),
        
        axis.ticks.length = unit(2, "mm"))


cover_plot <- ggplot(compiled, aes(x= sample_no, y= cover_power, fill = row))

cover_plot+geom_boxplot(aes(fill = row),
                        position = position_dodge(0.9))+
  
  geom_hline(yintercept = 0.05)+
  stat_summary(fun= mean, color = "red", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 3,
               show.legend = FALSE)+
  theme_classic() +
  
  scale_y_continuous(limits = c(0,25))+
  
  theme(plot.margin = unit(c(2,2,2,2), units="points"), ##good legend size
        
        #                                       panel.border = element_rect(color = "black",
        
        #                                                                 fill = NA, size = 1.75),
        
        axis.text.y = element_text(size = 16),  ## change font size of y axis
        
        axis.title.y = element_text(size = 16),
        
        axis.text.x = element_text(size = 16),  ## change font size of y axis
        
        axis.title.x = element_text(size = 16),
        
        legend.margin = margin(0, 7, 10, 0, "point"), ##remove margin on legend
        
        legend.text = element_text(size = 15),
        
        legend.position = c(0.85, 0.85),
        
        legend.key=element_blank(),  ##removed gray border around legend symbols
        
        legend.key.size = unit(7, "mm"),
        
        #legend.background = element_rect(color = "black", size = 1.25), #legend.box
        
        legend.key.height = unit(7, "mm"),
        
        legend.key.width = unit(7, "mm"),
        
        #                                         legend.box.spacing = unit(10, "mm"),
        
        legend.text.align = 0,
        
        axis.line.x = element_line(color = "black", size = 1.75),
        
        
        axis.ticks.x= element_line(size = 1.75, color = "black"),
        
        axis.line.y = element_line(color = "black", size = 1.75),
        
        axis.ticks.y = element_line(size = 1.75, color = "black"),
        
        axis.ticks.length = unit(2, "mm"))

interraction_plot <- ggplot(compiled, aes(x= sample_no, y= interraction_power, fill = row))

interraction_plot+geom_boxplot(aes(fill = row),
                               position = position_dodge(0.9))+
  
  geom_hline(yintercept = 0.05)+
  stat_summary(fun= mean, color = "red", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 3,
               show.legend = FALSE)+
  theme_classic() +
  
  scale_y_continuous(limits = c(0,25))+
  
  theme(plot.margin = unit(c(2,2,2,2), units="points"), ##good legend size
        
        #                                       panel.border = element_rect(color = "black",
        
        #                                                                 fill = NA, size = 1.75),
        
        axis.text.y = element_text(size = 16),  ## change font size of y axis
        
        axis.title.y = element_text(size = 16),
        
        axis.text.x = element_text(size = 16),  ## change font size of y axis
        
        axis.title.x = element_text(size = 16),
        
        legend.margin = margin(0, 7, 10, 0, "point"), ##remove margin on legend
        
        legend.text = element_text(size = 15),
        
        legend.position = c(0.85, 0.85),
        
        legend.key=element_blank(),  ##removed gray border around legend symbols
        
        legend.key.size = unit(7, "mm"),
        
        #legend.background = element_rect(color = "black", size = 1.25), #legend.box
        
        legend.key.height = unit(7, "mm"),
        
        legend.key.width = unit(7, "mm"),
        
        #                                         legend.box.spacing = unit(10, "mm"),
        
        legend.text.align = 0,
        
        axis.line.x = element_line(color = "black", size = 1.75),
        
        
        axis.ticks.x= element_line(size = 1.75, color = "black"),
        
        axis.line.y = element_line(color = "black", size = 1.75),
        
        axis.ticks.y = element_line(size = 1.75, color = "black"),
        
        axis.ticks.length = unit(2, "mm"))
