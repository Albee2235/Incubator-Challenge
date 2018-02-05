######################################################################
#                                                                    #
#    Association between Heavy Metal Elements (Lead and Cadmiun)     #
#    in Blood and Muscle Strength in the National Health and         # 
#    Nutrition Examination Survey (NHANES 2011-2014)                 #                         
#                                                                    #
#                   Author: Wei Liu                                  #
#                   Date: 2018-2-5                                   #
#                   Email: albeeadam@gmail.com                       #
#                                                                    #
######################################################################

# Preliminaries

library(arm); library(leaps); library(tableone); library(nhanesA); library(Hmisc)
library(pander); library(ROCR)
library(rms); library(forcats); library(broom); library(foreign)
source("Love-boost.R")
library(tidyverse)

# Import NHANES 2011-2012 tables

PBCD_G  <- nhanes('PBCD_G')
MGX_G  <- nhanes('MGX_G')
DEMO_G  <- nhanes('DEMO_G')
BMX_G  <- nhanes('BMX_G')
SMQ_G  <- nhanes('SMQ_G')
ALQ_G  <- nhanes('ALQ_G')
PAQ_G  <- nhanes('PAQ_G')
DIQ_G <- nhanes('DIQ_G')
BPX_G <- nhanes('BPX_G')
CDQ_G <- nhanes("CDQ_G")
CFQ_G <- nhanes('CFQ_G')

d1 <- merge(PBCD_G, MGX_G)
d2 <- merge(d1, DEMO_G)
d3 <- merge(d2, BMX_G)
d4 <- merge(d3, SMQ_G)
d5 <- merge(d4, ALQ_G)
d6 <- merge(d5, PAQ_G)
d7 <- merge(d6, DIQ_G)
d8 <- merge(d7, BPX_G)
d9 <- merge(d8, CDQ_G )
d10 <- merge(d9, CFQ_G)

# Import NHANES 2013-2014 tables

PBCD_H  <- nhanes('PBCD_H')
MGX_H  <- nhanes('MGX_H')
DEMO_H  <- nhanes('DEMO_H')
BMX_H  <- nhanes('BMX_H')
SMQ_H  <- nhanes('SMQ_H')
ALQ_H  <- nhanes('ALQ_H')
PAQ_H  <- nhanes('PAQ_H')
DIQ_H <- nhanes('DIQ_H')
BPX_H <- nhanes('BPX_H')
CDQ_H <- nhanes("CDQ_H")
CFQ_H <- nhanes('CFQ_H')

d1.H <- merge(PBCD_H, MGX_H)
d2.H <- merge(d1.H, DEMO_H)
d3.H <- merge(d2.H, BMX_H)
d4.H <- merge(d3.H, SMQ_H)
d5.H <- merge(d4.H, ALQ_H)
d6.H <- merge(d5.H, PAQ_H)
d7.H <- merge(d6.H, DIQ_H)
d8.H <- merge(d7.H, BPX_H)
d9.H <- merge(d8.H, CDQ_H)
d10.H <- merge(d9.H, CFQ_H)

# Rename and select needed variables from NHANES 2011-2012

data <- read.csv("d10.csv") %>%
  rename(CombiGS = MGDCGSZ,
         Blood_lead = LBXBPB,
         Blood_cadmiun = LBXBCD,
         Blood_mercury = LBXTHG,
         Age = RIDAGEYR,
         Sex = RIAGENDR,
         Education = DMDEDUC2,
         Ethnicity = RIDRETH1,
         Weight = BMXWT,
         Armcir = BMXARMC,
         Smoke1 = SMQ020,
         Smoke2 = SMQ040,
         Alcohol1 = ALQ110,
         Alcohol2 = ALQ120Q,
         Alcohol3 = ALQ130,
         Phyact1 = PAQ605,
         Phyact2 = PAQ620,
         Phyact3 = PAQ635,
         Phyact4 = PAQ650,
         Phyact5 = PAQ665,
         Phyact6 = PAD680,
         Diabetes = DIQ010,
         Systolic_BP1 = BPXSY1,
         Systolic_BP2 = BPXSY2,
         Systolic_BP3 = BPXSY3,
         Systolic_BP4 = BPXSY4,
         Angina1 = CDQ001,
         Angina2 = CDQ002,
         Angina3 = CDQ003,
         Angina4 = CDQ004,
         Angina5 = CDQ005,
         Angina6 = CDQ006,
         Angina7 = CDQ009D,
         Angina8 = CDQ009E,
         Angina9 = CDQ009F,
         Angina10 = CDQ009G,
         DSST = CFDDS) %>%
  select(CombiGS, Blood_lead, Blood_cadmiun, Blood_mercury, 
         Age, Sex, Education, Ethnicity, Weight, Armcir,
         Smoke1, Smoke2, Alcohol1, Alcohol2, Alcohol3, 
         Phyact1, Phyact2, Phyact3, Phyact4, Phyact5, Phyact6, Diabetes,
         Systolic_BP1, Systolic_BP2, Systolic_BP3, Systolic_BP4,
         Angina1, Angina2, Angina3, Angina4, Angina5, Angina6, Angina7, Angina8, Angina9, Angina10, DSST) %>% tbl_df

# Rename and select needed variables from NHANES 2013-2014

data.H <- read.csv("d10.H.csv") %>%
  rename(CombiGS = MGDCGSZ,
         Blood_lead = LBXBPB,
         Blood_cadmiun = LBXBCD,
         Blood_mercury = LBXTHG,
         Age = RIDAGEYR,
         Sex = RIAGENDR,
         Education = DMDEDUC2,
         Ethnicity = RIDRETH1,
         Weight = BMXWT,
         Armcir = BMXARMC,
         Smoke1 = SMQ020,
         Smoke2 = SMQ040,
         Alcohol1 = ALQ110,
         Alcohol2 = ALQ120Q,
         Alcohol3 = ALQ130,
         Phyact1 = PAQ605,
         Phyact2 = PAQ620,
         Phyact3 = PAQ635,
         Phyact4 = PAQ650,
         Phyact5 = PAQ665,
         Phyact6 = PAD680,
         Diabetes = DIQ010,
         Systolic_BP1 = BPXSY1,
         Systolic_BP2 = BPXSY2,
         Systolic_BP3 = BPXSY3,
         Systolic_BP4 = BPXSY4,
         Angina1 = CDQ001,
         Angina2 = CDQ002,
         Angina3 = CDQ003,
         Angina4 = CDQ004,
         Angina5 = CDQ005,
         Angina6 = CDQ006,
         Angina7 = CDQ009D,
         Angina8 = CDQ009E,
         Angina9 = CDQ009F,
         Angina10 = CDQ009G,
         DSST = CFDDS) %>%
  select(CombiGS, Blood_lead, Blood_cadmiun, Blood_mercury, 
         Age, Sex, Education, Ethnicity, Weight, Armcir,
         Smoke1, Smoke2, Alcohol1, Alcohol2, Alcohol3, 
         Phyact1, Phyact2, Phyact3, Phyact4, Phyact5, Phyact6, Diabetes,
         Systolic_BP1, Systolic_BP2, Systolic_BP3, Systolic_BP4,
         Angina1, Angina2, Angina3, Angina4, Angina5, Angina6, Angina7, Angina8, Angina9, Angina10, DSST) %>%tbl_df

# Combine two datasets

total <- rbind(data, data.H)

# Tidy the data set "total"

muscle <- total%>%
  filter(!is.na(CombiGS), !is.na(Blood_lead), !is.na(Blood_cadmiun), !is.na(Blood_mercury),
         Age >= 61 & Age <= 79,
         Education %in% c("1","2", "3", "4", "5"),
         Smoke1 %in% c("1","2"),
         Phyact1 %in% c("1","2"),
         Phyact2 %in% c("1","2"),
         Diabetes %in% c("1","2")) %>%
  mutate(Smoke = as.factor(ifelse(Smoke1 != 1, "Never", 
                                  ifelse(Smoke2 == 1 | Smoke2 == 2, 
                                         "Current", "Past"))),
         Alcohol11 = ifelse(Alcohol3 == 1, "Current light", 
                            ifelse(Alcohol3 > 1 & Alcohol2 <= 82, 
                                   "Current heavy", "not know")),
         Alcohol12 = ifelse(Alcohol1 == 2, "None", 
                            ifelse(Alcohol1 == 1 & Alcohol2 == 0, 
                                   "Past", "Current light")),
         Phyact11 = ifelse(Phyact1 == 1, "8", 
                           ifelse(Phyact2 == 1, "4", "0")),
         Phyact12 = ifelse(Phyact3 == 1, "4", "0"),
         Phyact13 = ifelse(Phyact4 == 1, "8", 
                           ifelse(Phyact5 == 1, "4", "0")),
         
         Angina_a = ifelse(Angina7 == 4 | Angina8 == 5, "1", "0"),
         Angina_b = ifelse(Angina9 == 6 & Angina10 == 7, "1", "0"),
         Angina_ab = ifelse(Angina_a == 1 | Angina_b == 1, "1", "0"),
         Angina = as.factor(ifelse(Angina1 != 1 | Angina2 != 1 | Angina4 != 1 | Angina5 != 1 | Angina6 != 1
                                   | Angina_ab != 1, "No", "Yes")),
         log_lead = log(Blood_lead),
         log_cadmiun = log(Blood_cadmiun),
         log_mercury = log(Blood_mercury))

muscle$Sex <- factor(muscle$Sex, levels = c(1, 2), labels = c("M", "F"))

muscle$Education <- factor(muscle$Education, levels = c(1, 2, 3, 4, 5), 
                           labels = c("Less than 9th grade","9-11th grade", "High school graduate", 
                                      "Some college or AA degree", "College graduate or above"))

muscle$Ethnicity <- factor(muscle$Ethnicity, levels = c(1, 2, 3, 4, 5), 
                           labels = c("Mexican American","Other Hispanic", 
                                      "Non-Hispanic White", "Non-Hispanic Black", "Other"))

muscle$Alcohol <- with(muscle,ifelse(is.na(Alcohol11),Alcohol12,Alcohol11))
# delete Alcohol coded as not know
muscle <- muscle %>%
  filter(Alcohol != "not know")

muscle$Phyact11 <- as.numeric(muscle$Phyact11)
muscle$Phyact12 <- as.numeric(muscle$Phyact12)
muscle$Phyact13 <- as.numeric(muscle$Phyact13)
muscle$Phyact <- muscle$Phyact11 + muscle$Phyact12 + muscle$Phyact13

muscle$Phyact <- as.character(muscle$Phyact)
muscle$Phyactfinal <- fct_recode(muscle$Phyact,
                                 "Seldom" = "0",
                                 "Seldom" = "4",
                                 "Sometimes" = "8",
                                 "Sometimes" = "12",
                                 "Always" = "16",
                                 "Always" = "20")

muscle$Diabetes <- factor(muscle$Diabetes, levels = c(1, 2), labels = c("Yes", "No"))

muscle$Systolic_BP_mean <-rowMeans(muscle[,23:26], na.rm = TRUE)
muscle$Hypertension <- factor(ifelse(muscle$Systolic_BP_mean >= 140, "Yes", "No"))

# Pre-anlaysis

muscle_M <- muscle %>%
  filter(Sex == "M")
muscle_F <- muscle %>% 
  filter(Sex == "F")

## Figure 1

f1_muscle_F <- lm(CombiGS ~ log_lead, data = muscle_F)
f1_muscle_M <- lm(CombiGS ~ log_lead, data = muscle_M)
summary(f1_muscle_F)
summary(f1_muscle_M)

p1 <- ggplot(muscle_F, aes(x = log_lead, y = CombiGS)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Female (n = 668)") +
  xlab("log(Blood Lead)") +
  ylab("Muscle strength") +
  annotate("text", x = 0, y = 10, col = "purple", size = 5,
           label = paste("Pearson r = ", signif(cor(muscle_F$log_lead, muscle_F$CombiGS),3))) +
  annotate("text", x = 0.5, y = 90, col = "red", size = 5,
           label = paste("Muscle strength = ", signif(coef(lm(muscle_F$CombiGS ~ muscle_F$log_lead))[1],3), "+",
                         signif(coef(lm(muscle_F$CombiGS ~ muscle_F$log_lead))[2],2), "log(Blood Lead)")) +
  annotate("text", x = -0.5, y = 85, col = "red", size = 5,
           label = paste("P-value = 0.963")) +
  theme_bw()

p2 <- ggplot(muscle_M, aes(x = log_lead, y = CombiGS)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Male (n = 562)") +
  xlab("log(Blood Lead)") +
  ylab("Muscle strength") +
  annotate("text", x = 1, y = 20, col = "purple", size = 5,
           label = paste("Pearson r = ", signif(cor(muscle_M$log_lead, muscle_M$CombiGS),3))) +
  annotate("text", x = 1, y = 130, col = "red", size = 5,
           label = paste("Muscle strength = ", signif(coef(lm(muscle_M$CombiGS ~ muscle_M$log_lead))[1],3),
                         signif(coef(lm(muscle_M$CombiGS ~ muscle_M$log_lead))[2],2), "log(Blood Lead)")) +
  annotate("text", x = 2.25, y = 120, col = "red", size = 5,
           label = paste("P-value = 0.00275")) +
  theme_bw()

require(grid) # needed to make textGrob run
grid_title1 <- "Relationship between muscle strength and blood lead"
annotation1 <- "Elderly people aged between 61 and 79 from NHANES 2011 - 2014"
gridExtra::grid.arrange(p1, p2, nrow = 1, 
                        top=textGrob(grid_title1, gp=gpar(fontsize=18,font=1)),
                        bottom = annotation1)


## Figure 2

f2_muscle_F <- lm(CombiGS ~ log_cadmiun, data = muscle_F)
f2_muscle_M <- lm(CombiGS ~ log_cadmiun, data = muscle_M)
summary(f2_muscle_F)
summary(f2_muscle_M)

p3 <- ggplot(muscle_F, aes(x = log_cadmiun, y = CombiGS)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Female (n = 668)") +
  xlab("log(Blood Cadmium)") +
  ylab("Muscle strength") +
  annotate("text", x = -0.5, y = 10, col = "purple", size = 5,
           label = paste("Pearson r = ", signif(cor(muscle_F$log_cadmiun, muscle_F$CombiGS),3))) +
  annotate("text", x = -0.5, y = 90, col = "red", size = 5,
           label = paste("Muscle strength = ", signif(coef(lm(muscle_F$CombiGS ~ muscle_F$log_cadmiun))[1],3), 
                         signif(coef(lm(muscle_F$CombiGS ~ muscle_F$log_cadmiun))[2],2), "log(Blood Cadmium)")) +
  annotate("text", x = -1.5, y = 82, col = "red", size = 5,
           label = paste("P-value = 0.404")) +
  theme_bw()

p4 <- ggplot(muscle_M, aes(x = log_cadmiun, y = CombiGS)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Male (n = 562)") +
  xlab("log(Blood Cadmium)") +
  ylab("Muscle strength") +
  annotate("text", x = 0, y = 20, col = "purple", size = 5,
           label = paste("Pearson r = ", signif(cor(muscle_M$log_cadmiun, muscle_M$CombiGS),3))) +
  annotate("text", x = -0.5, y = 130, col = "red", size = 5,
           label = paste("Muscle strength = ", signif(coef(lm(muscle_M$CombiGS ~ muscle_M$log_cadmiun))[1],3),
                         signif(coef(lm(muscle_M$CombiGS ~ muscle_M$log_cadmiun))[2],2), "log(Blood Cadmium)")) +
  annotate("text", x = -0.5, y = 120, col = "red", size = 5,
           label = paste("P-value = 0.0058")) +
  theme_bw()

require(grid) # needed to make textGrob run
grid_title2 <- "Relationship between muscle strength and blood Cadmium"
annotation2 <- "Elderly people aged between 61 and 79 from NHANES 2011 - 2014"
gridExtra::grid.arrange(p3, p4, nrow = 1, 
                        top=textGrob(grid_title2, gp=gpar(fontsize=18,font=1)),
                        bottom = annotation2)

f3_muscle_F <- lm(CombiGS ~ log_mercury, data = muscle_F)
f3_muscle_M <- lm(CombiGS ~ log_mercury, data = muscle_M)
summary(f3_muscle_F)
summary(f3_muscle_M)

# Select varaibles in need for in-depth analysis

muscle_1 <- muscle %>%
  select(CombiGS, log_lead, log_cadmiun, log_mercury, Blood_lead, Blood_cadmiun, Blood_mercury,
         Age, Sex, Education, Ethnicity, Weight, 
         Armcir, Smoke, Alcohol, Phyactfinal, Hypertension, Angina, DSST)

## change character to factor

muscle_1$Alcohol <- as.factor(muscle_1$Alcohol)

# Check missing pattern by subject

names(muscle_1)
na.pattern(muscle_1)

# Focus on complete cases and produce a descriptive summary table 

describe(muscle_final)

# Descriptive summary stratified by sex

vars <- c("CombiGS", "Blood_lead", "Blood_cadmiun","Blood_mercury", 
          "Age", "Education", "Ethnicity", "Weight", "Armcir", "Smoke", 
          "Alcohol", "Phyactfinal", "Hypertension", "Angina", "DSST")
factorVars <- c("Education", "Ethnicity", "Smoke", "Alcohol", 
                "Phyactfinal", "Hypertension", "Angina")
attemp1 <- CreateTableOne(data = muscle_final, vars = vars, 
                          factorVars = factorVars, strata = c("Sex"))
print(attemp1)

# Check normality of response varaibles 

muscle_final_F <- muscle_final %>%
  filter(Sex == "F")
muscle_final_M <- muscle_final %>%
  filter(Sex == "M")
eda.1sam(dataframe = muscle_final_F, variable = muscle_final_F$CombiGS,
         x.title = "Grip Strength for female",
         ov.title = "Grip Strength for female")
eda.1sam(dataframe = muscle_final_M, variable = muscle_final_M$CombiGS,
         x.title = "Grip Strength for male",
         ov.title = "Grip Strength for male")

# Predictor selection for Blood_lead

## Split the data into the train and test data sets

set.seed(43201)
train=sample(c(TRUE,FALSE), nrow(muscle_final_F),rep=TRUE)
test=(!train)

## Best subset approach

preds <- with(muscle_final_F[train,], cbind(log_lead, Age, Education, Ethnicity, Weight, Armcir, Smoke, Alcohol, Phyactfinal, Hypertension, Angina, DSST))
regfit.best <- regsubsets(preds, muscle_final_F[train,]$CombiGS, force.in="log_lead", nvmax=12)
rs <- summary(regfit.best)
rs

(To be continued...)

































