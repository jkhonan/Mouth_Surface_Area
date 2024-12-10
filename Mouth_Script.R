#Written by Jackie Larson and Jenna Honan

setwd("C:/Users/jhonan/Desktop/Research_Projects/Mouth_SA_V/Mouth_Surface_Area_Volume")

#Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(EnvStats)
library(ggpubr)

Mouth_Study_Surface_Areas <- read_excel("Mouth_Study_Surface_Areas.xlsx")
Mouth_Study_Demographics <- read_excel("Mouth_Study_Demographics.xlsx")
#Mouth_Study_Volume <- read_excel("Mouth_Study_Volume.xlsx")

Mouth_Study_Demographics$Age_y <- c(20,
                                  22,
                                  31,
                                  58,
                                  34,
                                  22,
                                  24,
                                  22,
                                  33,
                                  30,
                                  26,
                                  25,
                                  24,
                                  45,
                                  45,
                                  22,
                                  20,
                                  32,
                                  26,
                                  22)

Mouth_Study_Demographics$Ethnicity <- c("Hispanic",
                                        "Hispanic",
                                        "Non-Hispanic",
                                        "Non-Hispanic",
                                        "Hispanic",
                                        "Hispanic",
                                        "Hispanic",
                                        "Hispanic",
                                        "Hispanic",
                                        "Hispanic",
                                        "Hispanic",
                                        "Hispanic",
                                        "Hispanic",
                                        "Non-Hispanic",
                                        "Hispanic",
                                        "Hispanic",
                                        "Non-Hispanic",
                                        "Hispanic",
                                        "Non-Hispanic",
                                        "Hispanic")

Mouth_Study_Demographics$BMI_kg_m2 <- Mouth_Study_Demographics$Weight_kgs/((Mouth_Study_Demographics$Height_m)^2)

Mouth_Study_All <- left_join(Mouth_Study_Surface_Areas, Mouth_Study_Demographics, by = "ParticipantID")

Mouth_Study_All <- left_join(Mouth_Study_All, Mouth_Study_Volume, by = "ParticipantID")

columns_to_remove <- which(names(Mouth_Study_All) %in% c("Surface_Area_JKH_cm2", 
                                                         "Surface_Area_JLL_cm2", 
                                                         "Image_File_Name_JKH",
                                                         "Image_File_Name_JLL"))

# Remove the columns
Mouth_Study_All <- Mouth_Study_All[, -columns_to_remove]

Mouth_Study_All$BMI_Category <- ifelse(Mouth_Study_All$BMI_kg_m2<18.5, "Underweight",
                                       ifelse(Mouth_Study_All$BMI_kg_m2>=18.5 & Mouth_Study_All$BMI_kg_m2<25, "Healthy",
                                              ifelse(Mouth_Study_All$BMI_kg_m2>=25 & Mouth_Study_All$BMI_kg_m2<30, "Overweight",
                                                     ifelse(Mouth_Study_All$BMI_kg_m2>=30, "Obese", NA))))


#Demographic Info
hist(Mouth_Study_Demographics$Age_y)
hist(Mouth_Study_Demographics$Height_m) 
hist(Mouth_Study_Demographics$Weight_kgs)
hist(Mouth_Study_Demographics$BMI_kg_m2)

Mouth_Study_Demographics %>% 
  summarise(x_age=mean(Age_y),
            sd_age=sd(Age_y),
            x_height=mean(Height_m),
            sd_height=sd(Height_m),
            x_weight=mean(Weight_kgs),
            sd_weight=sd(Weight_kgs),
            x_BMI=mean(BMI_kg_m2),
            sd_BMI=sd(BMI_kg_m2),
            n_Hisp=sum(Ethnicity=="Hispanic"),
            n_Non_Hip=sum(Ethnicity=="Non-Hispanic"))

Mouth_Study_Demographics %>% 
  group_by(as.factor(Gender)) %>% 
  summarise(x_age=mean(Age_y),
            sd_age=sd(Age_y),
            x_height=mean(Height_m),
            sd_height=sd(Height_m),
            x_weight=mean(Weight_kgs),
            sd_weight=sd(Weight_kgs),
            x_BMI=mean(BMI_kg_m2),
            sd_BMI=sd(BMI_kg_m2),
            n_Hisp=sum(Ethnicity=="Hispanic"),
            n_Non_Hip=sum(Ethnicity=="Non-Hispanic"))

t.test(Age_y~as.factor(Gender), data=Mouth_Study_Demographics)
t.test(Height_m~as.factor(Gender), data=Mouth_Study_Demographics)
t.test(Weight_kgs~as.factor(Gender), data=Mouth_Study_Demographics)
t.test(BMI_kg_m2~as.factor(Gender), data=Mouth_Study_Demographics)
chisq.test(Mouth_Study_Demographics$Ethnicity, Mouth_Study_Demographics$Gender)

Mouth_Study_Surface_Areas$Average_Surface_Area_cm2 <- as.numeric(Mouth_Study_Surface_Areas$Average_Surface_Area_cm2)

#Descriptive Stats for Samples
##Surface Areas
Mouth_Study_Surface_Areas %>% 
  group_by(Print_Type) %>% 
  summarise(n=sum(!is.na(as.numeric(Average_Surface_Area_cm2))),
            AM=mean(Average_Surface_Area_cm2, na.rm = T),
            SD=sd(Average_Surface_Area_cm2, na.rm = T),
            median=median(Average_Surface_Area_cm2, na.rm = T),
            range=range(Average_Surface_Area_cm2, na.rm = T))


filtered_neutral <- Mouth_Study_Surface_Areas %>%
  filter(Print_Type == "Neutral")

hist(filtered_neutral$Average_Surface_Area_cm2)
hist(log(filtered_neutral$Average_Surface_Area_cm2))

filtered_pucker <- Mouth_Study_Surface_Areas %>%
  filter(Print_Type == "Pucker")

hist(filtered_pucker$Average_Surface_Area_cm2)
hist(log(filtered_pucker$Average_Surface_Area_cm2))

filtered_straw <- Mouth_Study_Surface_Areas %>%
  filter(Print_Type == "Straw")

hist(filtered_straw$Average_Surface_Area_cm2)
hist(log(filtered_straw$Average_Surface_Area_cm2))

filtered_cup <- Mouth_Study_Surface_Areas %>%
  filter(Print_Type == "Cup")

filtered_cup <- filtered_cup %>%
  filter(!is.na(Average_Surface_Area_cm2))

hist(filtered_cup$Average_Surface_Area_cm2)
hist(log(filtered_cup$Average_Surface_Area_cm2))


filtered_void <- Mouth_Study_Surface_Areas %>%
  filter(Print_Type == "Apple Bite")

filtered_void <- filtered_void %>%
  filter(!is.na(Average_Surface_Area_cm2))

hist(filtered_void$Average_Surface_Area_cm2)
hist(log(filtered_void$Average_Surface_Area_cm2))


#Percentiles and GeoAverages
geoMean(filtered_neutral$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_neutral$Average_Surface_Area_cm2, na.rm = T)

geoMean(filtered_pucker$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_pucker$Average_Surface_Area_cm2, na.rm = T)

geoMean(filtered_straw$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_straw$Average_Surface_Area_cm2, na.rm = T)

geoMean(filtered_cup$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_cup$Average_Surface_Area_cm2, na.rm = T)

geoMean(filtered_void$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_void$Average_Surface_Area_cm2, na.rm = T)

quantile(filtered_neutral$Average_Surface_Area_cm2, probs = c(.05, .25, .75, .95))

quantile(filtered_pucker$Average_Surface_Area_cm2, probs = c(.05, .25, .75, .95))

quantile(filtered_straw$Average_Surface_Area_cm2, probs = c(.05, .25, .75, .95))

quantile(filtered_cup$Average_Surface_Area_cm2, probs = c(.05, .25, .75, .95))

quantile(filtered_void$Average_Surface_Area_cm2, probs = c(.05, .25, .75, .95))

##Volume
#Such low confidence that this portion of the study was discarded.
#Mouth_Study_Volume %>% 
#  summarise(n=sum(!is.na(as.numeric(Volume_Full_Missing_Piece_mL))),
#            AM=mean(Volume_Full_Missing_Piece_mL, na.rm = T),
#            SD=sd(Volume_Full_Missing_Piece_mL, na.rm = T),
#            median=median(Volume_Full_Missing_Piece_mL, na.rm = T),
#            range=range(Volume_Full_Missing_Piece_mL, na.rm = T))

#hist(Mouth_Study_Volume$Volume_Full_Missing_Piece_mL)
#hist(log(Mouth_Study_Volume$Volume_Full_Missing_Piece_mL))

#Percentiles and GeoAverages
#geoMean(Mouth_Study_Volume$Volume_Full_Missing_Piece_mL, na.rm = T)
#geoSD(Mouth_Study_Volume$Volume_Full_Missing_Piece_mL, na.rm = T)

#quantile(Mouth_Study_Volume$Volume_Full_Missing_Piece_mL, probs = c(.05, .25, .75, .95))

#By Gender
##Surface Area
Mouth_Study_All$Average_Surface_Area_cm2 <- as.numeric(Mouth_Study_All$Average_Surface_Area_cm2)

Mouth_Study_All %>% 
  group_by(Print_Type, Gender) %>% 
  summarise(n=sum(!is.na(as.numeric(Average_Surface_Area_cm2))))


filtered_neutral2 <- Mouth_Study_All %>%
  filter(Print_Type == "Neutral")
filtered_neutral2 <- filtered_neutral2 %>%
  filter(!is.na(Average_Surface_Area_cm2))
filtered_neutral_F <- filtered_neutral2 %>%
  filter(Gender=="F")
filtered_neutral_M <- filtered_neutral2 %>%
  filter(Gender=="M")


filtered_pucker2 <- Mouth_Study_All %>%
  filter(Print_Type == "Pucker")
filtered_pucker2 <- filtered_pucker2 %>%
  filter(!is.na(Average_Surface_Area_cm2))
filtered_pucker_F <- filtered_pucker2 %>%
  filter(Gender=="F")
filtered_pucker_M <- filtered_pucker2 %>%
  filter(Gender=="M")


filtered_cup2 <- Mouth_Study_All %>%
  filter(Print_Type == "Cup")
filtered_cup2 <- filtered_cup2 %>%
  filter(!is.na(Average_Surface_Area_cm2))
filtered_cup_F <- filtered_cup2 %>%
  filter(Gender=="F")
filtered_cup_M <- filtered_cup2 %>%
  filter(Gender=="M")


filtered_straw2 <- Mouth_Study_All %>%
  filter(Print_Type == "Straw")
filtered_straw2 <- filtered_straw2 %>%
  filter(!is.na(Average_Surface_Area_cm2))
filtered_straw_F <- filtered_straw2 %>%
  filter(Gender=="F")
filtered_straw_M <- filtered_straw2 %>%
  filter(Gender=="M")


filtered_void2 <- Mouth_Study_All %>%
  filter(Print_Type == "Apple Bite")
filtered_void2 <- filtered_void2 %>%
  filter(!is.na(Average_Surface_Area_cm2))
filtered_void_F <- filtered_void2 %>%
  filter(Gender=="F")
filtered_void_M <- filtered_void2 %>%
  filter(Gender=="M")

#GeoAverages
geoMean(filtered_neutral_F$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_neutral_F$Average_Surface_Area_cm2, na.rm = T)

geoMean(filtered_neutral_M$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_neutral_M$Average_Surface_Area_cm2, na.rm = T)


geoMean(filtered_pucker_F$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_pucker_F$Average_Surface_Area_cm2, na.rm = T)

geoMean(filtered_pucker_M$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_pucker_M$Average_Surface_Area_cm2, na.rm = T)


geoMean(filtered_cup_F$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_cup_F$Average_Surface_Area_cm2, na.rm = T)

geoMean(filtered_cup_M$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_cup_M$Average_Surface_Area_cm2, na.rm = T)


geoMean(filtered_straw_F$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_straw_F$Average_Surface_Area_cm2, na.rm = T)

geoMean(filtered_straw_M$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_straw_M$Average_Surface_Area_cm2, na.rm = T)


geoMean(filtered_void_F$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_void_F$Average_Surface_Area_cm2, na.rm = T)

geoMean(filtered_void_M$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_void_M$Average_Surface_Area_cm2, na.rm = T)

##Volume
#Such low confidence that this portion of the study was discarded.
#Volume_Only <- Mouth_Study_All %>%
#  distinct(ParticipantID, .keep_all = TRUE)

#Volume_Only %>% 
#  group_by(Gender) %>% 
#  summarise(n=sum(!is.na(as.numeric(Volume_Full_Missing_Piece_mL))))

#Volume_Only_F <- subset(Volume_Only, Volume_Only$Gender=="F")
#Volume_Only_M <- subset(Volume_Only, Volume_Only$Gender=="M")

#geoMean(Volume_Only_F$Volume_Full_Missing_Piece_mL, na.rm = T)
#geoSD(Volume_Only_F$Volume_Full_Missing_Piece_mL, na.rm = T)

#geoMean(Volume_Only_M$Volume_Full_Missing_Piece_mL, na.rm = T)
#geoSD(Volume_Only_M$Volume_Full_Missing_Piece_mL, na.rm = T)

#t-tests
t.test(log(filtered_neutral_F$Average_Surface_Area_cm2), log(filtered_neutral_M$Average_Surface_Area_cm2))
t.test(log(filtered_pucker_F$Average_Surface_Area_cm2), log(filtered_pucker_M$Average_Surface_Area_cm2))
t.test(log(filtered_cup_F$Average_Surface_Area_cm2), log(filtered_cup_M$Average_Surface_Area_cm2))
t.test(log(filtered_straw_F$Average_Surface_Area_cm2), log(filtered_straw_M$Average_Surface_Area_cm2))
t.test(log(filtered_void_F$Average_Surface_Area_cm2), log(filtered_void_M$Average_Surface_Area_cm2))
#t.test(log(Volume_Only_F$Volume_Full_Missing_Piece_mL), log(Volume_Only_M$Volume_Full_Missing_Piece_mL))


#By Ethnicity
##Surface Area

Mouth_Study_All %>% 
  group_by(Print_Type, Ethnicity) %>% 
  summarise(n=sum(!is.na(as.numeric(Average_Surface_Area_cm2))))


filtered_neutral_H <- filtered_neutral2 %>%
  filter(Ethnicity=="Hispanic")
filtered_neutral_NH <- filtered_neutral2 %>%
  filter(Ethnicity=="Non-Hispanic")

filtered_pucker_H <- filtered_pucker2 %>%
  filter(Ethnicity=="Hispanic")
filtered_pucker_NH <- filtered_pucker2 %>%
  filter(Ethnicity=="Non-Hispanic")

filtered_cup_H <- filtered_cup2 %>%
  filter(Ethnicity=="Hispanic")
filtered_cup_NH <- filtered_cup2 %>%
  filter(Ethnicity=="Non-Hispanic")

filtered_straw_H <- filtered_straw2 %>%
  filter(Ethnicity=="Hispanic")
filtered_straw_NH <- filtered_straw2 %>%
  filter(Ethnicity=="Non-Hispanic")

filtered_void_H <- filtered_void2 %>%
  filter(Ethnicity=="Hispanic")
filtered_void_NH <- filtered_void2 %>%
  filter(Ethnicity=="Non-Hispanic")

#GeoAverages
geoMean(filtered_neutral_H$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_neutral_H$Average_Surface_Area_cm2, na.rm = T)

geoMean(filtered_neutral_NH$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_neutral_NH$Average_Surface_Area_cm2, na.rm = T)


geoMean(filtered_pucker_H$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_pucker_H$Average_Surface_Area_cm2, na.rm = T)

geoMean(filtered_pucker_NH$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_pucker_NH$Average_Surface_Area_cm2, na.rm = T)


geoMean(filtered_cup_H$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_cup_H$Average_Surface_Area_cm2, na.rm = T)

geoMean(filtered_cup_NH$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_cup_NH$Average_Surface_Area_cm2, na.rm = T)


geoMean(filtered_straw_H$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_straw_H$Average_Surface_Area_cm2, na.rm = T)

geoMean(filtered_straw_NH$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_straw_NH$Average_Surface_Area_cm2, na.rm = T)


geoMean(filtered_void_H$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_void_H$Average_Surface_Area_cm2, na.rm = T)

geoMean(filtered_void_NH$Average_Surface_Area_cm2, na.rm = T)
geoSD(filtered_void_NH$Average_Surface_Area_cm2, na.rm = T)

##Volume
#Such low confidence that this portion of the study was discarded.
#Volume_Only <- Mouth_Study_All %>%
#  distinct(ParticipantID, .keep_all = TRUE)

#Volume_Only %>% 
#  group_by(Ethnicity) %>% 
#  summarise(n=sum(!is.na(as.numeric(Volume_Full_Missing_Piece_mL))))

#Volume_Only_H <- subset(Volume_Only, Volume_Only$Ethnicity=="Hispanic")
#Volume_Only_NH <- subset(Volume_Only, Volume_Only$Ethnicity=="Non-Hispanic")

#geoMean(Volume_Only_H$Volume_Full_Missing_Piece_mL, na.rm = T)
#geoSD(Volume_Only_H$Volume_Full_Missing_Piece_mL, na.rm = T)

#geoMean(Volume_Only_NH$Volume_Full_Missing_Piece_mL, na.rm = T)
#geoSD(Volume_Only_NH$Volume_Full_Missing_Piece_mL, na.rm = T)

#t-tests
t.test(log(filtered_neutral_H$Average_Surface_Area_cm2), log(filtered_neutral_NH$Average_Surface_Area_cm2))
t.test(log(filtered_pucker_H$Average_Surface_Area_cm2), log(filtered_pucker_NH$Average_Surface_Area_cm2))
t.test(log(filtered_cup_H$Average_Surface_Area_cm2), log(filtered_cup_NH$Average_Surface_Area_cm2))
t.test(log(filtered_straw_H$Average_Surface_Area_cm2), log(filtered_straw_NH$Average_Surface_Area_cm2))
t.test(log(filtered_void_H$Average_Surface_Area_cm2), log(filtered_void_NH$Average_Surface_Area_cm2))
#t.test(log(Volume_Only_H$Volume_Full_Missing_Piece_mL), log(Volume_Only_NH$Volume_Full_Missing_Piece_mL))


#Correlations
##Height
cor.test(log(filtered_neutral2$Average_Surface_Area_cm2), filtered_neutral2$Height_m, method = "pearson")
cor.test(log(filtered_pucker2$Average_Surface_Area_cm2), filtered_pucker2$Height_m, method = "pearson")
cor.test(log(filtered_cup2$Average_Surface_Area_cm2), filtered_cup2$Height_m, method = "pearson")
cor.test(log(filtered_straw2$Average_Surface_Area_cm2), filtered_straw2$Height_m, method = "pearson")
cor.test(log(filtered_void2$Average_Surface_Area_cm2), filtered_void2$Height_m, method = "pearson")
#cor.test(log(Volume_Only$Volume_Full_Missing_Piece_mL), Volume_Only$Height_m, method = "pearson")

##Weight
cor.test(log(filtered_neutral2$Average_Surface_Area_cm2), filtered_neutral2$Weight_kgs, method = "pearson")
cor.test(log(filtered_pucker2$Average_Surface_Area_cm2), filtered_pucker2$Weight_kgs, method = "pearson")
cor.test(log(filtered_cup2$Average_Surface_Area_cm2), filtered_cup2$Weight_kgs, method = "pearson")
cor.test(log(filtered_straw2$Average_Surface_Area_cm2), filtered_straw2$Weight_kgs, method = "pearson")
cor.test(log(filtered_void2$Average_Surface_Area_cm2), filtered_void2$Weight_kgs, method = "pearson")
#cor.test(log(Volume_Only$Volume_Full_Missing_Piece_mL), Volume_Only$Weight_kgs, method = "pearson")

##BMI
cor.test(log(filtered_neutral2$Average_Surface_Area_cm2), filtered_neutral2$BMI_kg_m2, method = "pearson")
cor.test(log(filtered_pucker2$Average_Surface_Area_cm2), filtered_pucker2$BMI_kg_m2, method = "pearson")
cor.test(log(filtered_cup2$Average_Surface_Area_cm2), filtered_cup2$BMI_kg_m2, method = "pearson")
cor.test(log(filtered_straw2$Average_Surface_Area_cm2), filtered_straw2$BMI_kg_m2, method = "pearson")
cor.test(log(filtered_void2$Average_Surface_Area_cm2), filtered_void2$BMI_kg_m2, method = "pearson")
#cor.test(log(Volume_Only$Volume_Full_Missing_Piece_mL), Volume_Only$BMI_kg_m2, method = "pearson")


#Graphs
ggplot(data=filtered_neutral2, aes(x=Height_m, y=log(Average_Surface_Area_cm2)))+
  geom_point()+
  geom_smooth(method = "lm", se=F)+
  theme(panel.background = element_rect("#ffffff"),
        text = element_text(size=15),
        axis.line = element_line("#000000"))+
  xlab("Height (m)")+
  ylab("Log-Transformed Neutral Lip Print \nSurface Area")+
  stat_cor(method = "pearson", label.x = 1.6, label.y = 2.6)


ggplot(data=filtered_pucker2, aes(x=Weight_kgs, y=log(Average_Surface_Area_cm2)))+
  geom_point()+
  geom_smooth(method = "lm", se=F)+
  theme(panel.background = element_rect("#ffffff"),
        text = element_text(size=15),
        axis.line = element_line("#000000"))+
  xlab("Weight (kg)")+
  ylab("Log-Transformed Pucker Lip Print \nSurface Area")+
  stat_cor(method = "pearson", label.x = 55, label.y = 2.7)

##Just to see
ggplot(data=filtered_neutral2, aes(x=Weight_kgs, y=log(Average_Surface_Area_cm2)))+
  geom_point()+
  geom_smooth(method = "lm", se=F)+
  theme(panel.background = element_rect("#ffffff"),
        text = element_text(size=15),
        axis.line = element_line("#000000"))+
  xlab("Weight (kg)")+
  ylab("Log-Transformed Pucker Lip Print \nSurface Area")+
  stat_cor(method = "pearson", label.x = 55, label.y = 2.7)

#By Gender

Mouth_Study_All$Print_Type[Mouth_Study_All$Print_Type=="Apple Bite"] <- "Open Mouth"

Mouth_Study_All$Print_Type <- factor(Mouth_Study_All$Print_Type , levels=c("Neutral", "Pucker", "Cup", "Straw", "Open Mouth"))


ggplot(data=Mouth_Study_All, aes(x=as.factor(Print_Type), y=Average_Surface_Area_cm2))+
  geom_boxplot(aes(fill=Gender))+
  theme(panel.background = element_rect("#ffffff"),
        text = element_text(size=15),
        axis.line = element_line("#000000"))+
  scale_fill_manu(labels = c("M"="Male", "F"="Female"),
                      values = c("M"="darkorchid3", "F"="lightseagreen"))+
  xlab("")+
  ylab(bquote("Surface Area "~(cm^2)))


ggplot(data=Mouth_Study_All, aes(x=as.factor(Print_Type), y=Average_Surface_Area_cm2))+
  geom_boxplot(aes(fill=Ethnicity))+
  theme(panel.background = element_rect("#ffffff"),
        text = element_text(size=15),
        axis.line = element_line("#000000"))+
  scale_fill_manual(breaks = c("Hispanic", "Non-Hispanic"), 
                    values=c("#add8e6", "#ffa500"))+
  xlab("")+
  ylab(bquote("Surface Area "~(cm^2)))

#BMI ANOVAs + Tukey's if significant
#                       
BMI_Cat_Neutral <- aov(Average_Surface_Area_cm2 ~ BMI_Category, data=subset(Mouth_Study_All, Mouth_Study_All$Print_Type=="Neutral"))
summary(BMI_Cat_Neutral)

BMI_Cat_Pucker <- aov(Average_Surface_Area_cm2 ~ BMI_Category, data=subset(Mouth_Study_All, Mouth_Study_All$Print_Type=="Pucker"))
summary(BMI_Cat_Pucker)

BMI_Cat_Cup <- aov(Average_Surface_Area_cm2 ~ BMI_Category, data=subset(Mouth_Study_All, Mouth_Study_All$Print_Type=="Cup"))
summary(BMI_Cat_Cup)

BMI_Cat_Straw <- aov(Average_Surface_Area_cm2 ~ BMI_Category, data=subset(Mouth_Study_All, Mouth_Study_All$Print_Type=="Straw"))
summary(BMI_Cat_Straw)

BMI_Cat_Open <- aov(Average_Surface_Area_cm2 ~ BMI_Category, data=subset(Mouth_Study_All, Mouth_Study_All$Print_Type=="Open Mouth"))
summary(BMI_Cat_Open)

Mouth_Study_Volume$BMI_Category <- c("Obese", "Healthy", "Overweight", "Overweight", "Overweight", "Healthy", "Healthy", "Obese", "Obese", "Obese", "Obese", "Healthy", "Overweight", "Overweight", "Healthy", "Healthy", "Overweight", "Overweight", "Overweight", "Overweight")

#BMI_Cat_Bite <- aov(Volume_Full_Missing_Piece_mL ~ BMI_Category, data=Mouth_Study_Volume)
#summary(BMI_Cat_Bite)