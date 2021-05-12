## load required packages
library(data.table)
library(foreign)
library(fastmatch)
library(readstata13)
library(ggplot2)
library(scales)
library(lubridate)
library(zoo)


rm(list=ls())
## set working directory
setwd("/Users/ziao/Desktop/ALP301/Data")
## load data, latest version is 2021_05_11_21_29_00 and it has 2380 obs
data = as.data.table(read.csv("Academic_Survey_Research_in_Africa_2021_05_11_21_29_00.csv", 
                                  na.strings=c("","NA"), header = T))
names(data)


## filter, consent = YES
data = data[consent_response %in% c("yes","Yes", "YES"),]
## filter, got a assignment treatment
data = data[!is.na(treatment_format)]



## Summary plot 1: age distribution
age = data.table(group=1:7,
                 label=c("below 24", "25-34", "35-44", "45-54", "55-64","65-74", "above 75"),
                 actual=c(6732,10987, 8303,5429,3785,2141,1058),
                 survey=rep(0,7))
age$survey[1] = sum(data$cv_age<24, na.rm=T)
age$survey[2] = sum(data$cv_age>=25 & data$cv_age<=34,na.rm=T)
age$survey[3] = sum(data$cv_age>=35 & data$cv_age<=44,na.rm=T)
age$survey[4] = sum(data$cv_age>=45 & data$cv_age<=54,na.rm=T)
age$survey[5] = sum(data$cv_age>=55 & data$cv_age<=64,na.rm=T)
age$survey[6] = sum(data$cv_age>=65 & data$cv_age<=74,na.rm=T)
age$survey[7] = sum(data$cv_age>=75 & data$cv_age<=150,na.rm=T)
age$actual_p = age$actual/sum(age$actual)
age$survey_p = age$survey/sum(age$survey, na.rm=T)

age_plot = ggplot(melt.data.table(age[,c(1,5,6)], id.var = "group"),  
              aes(x = group, y=value, fill=variable)) + theme_light() + 
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(name="Data Source", values = c("lightblue", "darkblue"), labels=c("Word Bank", "Survey Data"))+
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.88,0.88), axis.text=element_text(size=6)) +
  xlab("\n Age Group")+ylab("Percentage \n") + labs(title = "Age Distribution") + 
  scale_x_discrete(limits =  c("below 24", "25-34", "35-44", "45-54", "55-64","65-74", "above 75"))
age_plot
ggsave("summary_age_plot.pdf", age_plot, width = 7, height = 5, units = "in")

## Summary plot 2: geography distribution
# library(maptools)
# # source: https://en.wikipedia.org/wiki/List_of_South_African_provinces_by_population
# population <- data.frame(province = c("Gauteng", "KwaZulu-Natal", "Western Cape", "Eastern Cape", "Limpopo", "Mpumalanga", "North West", "Free State", "Northern Cape"),
#                          population = c(14278669, 11074784, 6510312, 6498683, 5778442, 4444212, 3856174, 2866678, 1213996)
# )
# 
# # source: http://www.tageo.com/index-e-sf-cities-ZA.htm
# coordinates_cities <- data.frame(city = c("Cape Town", "Durban", "Johannesburg", "Pretoria", "Soweto"),
#                                  lat = c(-33.930, -29.870, -26.190, -25.730, -26.280),
#                                  lon = c(18.460, 30.990, 28.040, 28.220, 27.840)
# )
# 
# # Read data
# shapefile <- readShapeSpatial("ZAF_adm1.shp")
# summary(shapefile)
# ## plot(shapefile)
# shapefile@data <- merge(shapefile@data, population, by.x = c("name"), by.y = c("province"))
# shapefile@data
# 
# # Assign colors to provinces based on population
# population_data <- shapefile@data$population
# colors <- (population_data - min(population_data)) / (max(population_data) - min(population_data))*254+1
# shapefile@data$color =  colorRampPalette(c('#9eceff', '#004081'))(255)[colors]
# plot = plot(shapefile, col = shapefile@data$color, border=NA)
# ggsave("map.pdf", plot, width = 7, height = 5, units = "in")
# 
# # Create legend
# library(dplyr)
# shapefile@data <- shapefile@data %>%
#   arrange(population, color)
# shapefile@data$population_bins <- cut(shapefile@data$population/1000000, 5)
# shapefile@data$color_bins =  colorRampPalette(c('#9eceff', '#004081'))(5)[shapefile@data$population_bins]
# legend("topleft", fill = unique(shapefile@data$color_bins), legend = unique(shapefile@data$population_bins), col = unique(shapefile@data$color_bins))
# 
# 
# map <- ggplot() + geom_polygon(data = shapefile, aes(x = long, y = lat, group = group), colour = "black", fill = NA)


## Summary plot 3: gender distribution
gender = data.table(group=1:2,
                 label=c("male", "female"),
                 actual=c(28.8, 29.7),
                 survey=rep(0,2))

gender$survey[1] = sum(data$cv_gender=="male", na.rm=T)
gender$survey[2] =  sum(data$cv_gender=="female", na.rm=T)

gender$actual_p = gender$actual/sum(gender$actual)
gender$survey_p = gender$survey/sum(gender$survey, na.rm=T)


gender_plot = ggplot(melt.data.table(gender[,c(1,5,6)], id.var = "group"),  
                  aes(x = group, y=value, fill=variable)) + theme_light() + 
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(name="Data Source", values = c("indianred1", "indianred4"), labels=c("Word Bank", "Survey Data"))+
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.13,0.88), axis.text=element_text(size=6)) +
  xlab("\n Gender")+ylab("Percentage \n") + labs(title = "Gender Distribution") + 
  scale_x_discrete(limits =  c("male", "female"))
gender_plot
ggsave("summary_gender_plot.pdf", gender_plot, width = 7, height = 5, units = "in")



## Summary plot 4: Ethnicity distribution
summary(data$cv_ethnic)
race = data.table(group=1:5,
                    label=c("Black", "White", "Colored", "Asian", "Other"),
                    actual=c(76.4, 9.1, 8.9,2.5,0.5),
                    survey=rep(0,5))

race$survey[1] = sum(data$cv_ethnic =="Black African" | data$cv_ethnic =="Black", na.rm=T) 
race$survey[2] = sum(data$cv_ethnic =="White", na.rm=T)
race$survey[3] = sum(data$cv_ethnic =="Coloured", na.rm=T)
race$survey[4] = sum(data$cv_ethnic =="Indian/Asian" , na.rm=T)
race$survey[5] = sum(data$cv_ethnic =="Other" , na.rm=T)

race$actual_p = race$actual/sum(race$actual)
race$survey_p = race$survey/sum(race$survey, na.rm=T)

race_plot = ggplot(melt.data.table(race[,c(1,5,6)], id.var = "group"),  
                     aes(x = group, y=value, fill=variable)) + theme_light() + 
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(name="Data Source", values = c("indianred1", "indianred4"), labels=c("Word Bank", "Survey Data"))+
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.88,0.88), axis.text=element_text(size=6)) +
  xlab("\n Race")+ylab("Percentage \n") + labs(title = "Race Distribution") + 
  scale_x_discrete(limits =  c("Black", "White", "Colored", "Asian", "Other"))
race_plot
ggsave("summary_race_plot.pdf", race_plot, width = 7, height = 5, units = "in")



## Summary plot 5: General attitude towards gov and risk
trust_gov = as.data.table(table(data$trust_gov))[N>30]
risk = as.data.table(table(data$risk_level))[N>30]
attitude = merge(trust_gov, risk, by = "V1")
attitude$group = c(3,2,4,1,5)
names(attitude) = c("label","trust_gov", "risk","group")
attitude$trust_gov_p = attitude$trust_gov / sum(attitude$trust_gov)
attitude$risk_p = attitude$risk / sum(attitude$risk)

attitude_plot = ggplot(melt.data.table(attitude[,c(4,5,6)], id.var = "group"),  
                   aes(x = group, y=value, fill=variable)) + theme_light() + 
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(name="Data Source", values = c("indianred1", "indianred4"), 
                    labels=c("Trust Gov", "Take Risk"))+
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.88,0.88), axis.text=element_text(size=6)) +
  xlab("\n Gender")+ylab("Percentage \n") + labs(title = "General Attitude towards Government and Risk") + 
  scale_x_discrete(limits =  c("Strongly agree", "Somewhat agree", "Neutral", "Somewhat disagree","Strongly disagree" ))
attitude_plot
ggsave("summary_attitude_plot.pdf", attitude_plot, width = 7, height = 5, units = "in")





## check treatment probabilities
summary(data$treatment_format)
summary(data$treatment_nudges)
data[treatment_format=="VID", treatmentX:="video"]
data[treatment_format=="MOG", treatmentX:="graphic"]
data[treatment_format=="TXT", treatmentX:="text"]
data[treatment_format=="IMG", treatmentX:=treatment_nudges]
data$treatmentX = as.factor(data$treatmentX)
summary(data$treatmentX)
## hist of treatment probabilities
table = as.data.frame(table(data$treatmentX))
sum(table$count)
table = table[table$Freq>40,]
names(table) = c("group", "count")

pic = ggplot(table, aes(x = group, y = count, width=.7)) + 
  geom_bar(stat="identity", position="dodge", fill="lightskyblue", colour="navy") + theme_light() + 
  xlab("\n Group") + ylab("Number of Respondents \n") + 
  labs(title = "Number of Respondents by Treatment Assignment (N=2299)") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(label = c("concern", "control",  "endorsement", "graphic", "real info",
                             "relatable", "safety others","video")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5))
pic
ggsave("treatment_hist.pdf", pic, width = 7, height = 5, units = "in")

# ## look at attrition by treatment group
# data2 = data[!is.na(dv_post3),]
# table2 = as.data.frame(table(data2$treatmentX))
# names(table2) = c("group", "count")
# attrition = merge(table, table2, by = "group")
# attrition$remaining = attrition$count.y/ attrition$count.x
# 
# pic = ggplot(attrition, aes(x = group, y = remaining, width=.7)) + 
#   geom_bar(stat="identity", position="dodge", fill="lightskyblue", colour="navy") + theme_light() + 
#   xlab("\n Group") + ylab("Share of Remaining Respondents \n") + 
#   labs(title = "Share of Remaining Respondents by Treatment Assignments") +
#   theme(plot.title = element_text(hjust = 0.5)) + 
#   scale_x_discrete(label = c("concern", "control",  "endorsement", "graphic", "real info",
#                              "relatable", "safety others","video")) + 
#   theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5))
# ggsave("attrition_hist.pdf", pic, width = 7, height = 5, units = "in")

## look at pre vs post sharing methods
share_pre = as.data.frame(table(data$dv_pre1))[share_pre$Freq>5,]
share_post = as.data.frame(table(data$dv_post3))[share_post$Freq>5,]
share_merged = merge(share_pre, share_post, by = "Var1")
names(share_merged) = c("label", "pre","post")
share_merged$group = c(4,2,1,3)
share_merged = as.data.table(share_merged)

share_plot = ggplot(melt.data.table(share_merged[,c(4,2,3)], id.var = "group"),  
                       aes(x = group, y=value, fill=variable)) + theme_light() + 
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(name="Pre/Post", values = c("indianred1", "indianred4"), 
                    labels=c("Pre", "Post"))+
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.88,0.88), axis.text=element_text(size=6)) +
  xlab("\n Gender")+ylab("Count \n") + labs(title = "General Attitude towards Government and Risk") + 
  scale_x_discrete(limits =  c("Neither", "Messenger", "Timeline", "Both" ))
share_plot
ggsave("summary_share_plot.pdf", share_plot, width = 7, height = 5, units = "in")


## look at pre-post mean difference
data3 = data
data3[, pre:=ifelse(dv_pre1!="neither",1,0)]
data3[, post:=ifelse(dv_post3!="neither",1,0)]
data3[, diff:=post-pre]
summary(data3$diff)

table3 = as.data.frame(table(data3[diff==1,]$treatmentX))
table3 = table3[table3$Freq>0,]
names(table3) = c("group", "count")

pic = ggplot(table3, aes(x = group, y = count, width=.7)) + 
  geom_bar(stat="identity", position="dodge", fill="lightskyblue", colour="navy") + theme_light() + 
  xlab("\n Group") + ylab("Number of Respondents \n") + 
  labs(title = "Number of Respondents who Switched from Not Sharing to Sharing") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(label = c("concern", "control",  "endorsement", "graphic", "real info",
                              "relatable", "safety others","video")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5))
pic
ggsave("pos_change_share.pdf", pic, width = 7, height = 5, units = "in")


table4 = as.data.frame(table(data3[diff==-1,]$treatmentX))
table4 = table4[table4$Freq>0,]
names(table4) = c("group", "count")

pic = ggplot(table4, aes(x = group, y = count, width=.7)) + 
  geom_bar(stat="identity", position="dodge", fill="indianred1", colour="indianred4") + theme_light() + 
  xlab("\n Group") + ylab("Number of Respondents \n") + 
  labs(title = "Number of Respondents who Switched from Sharing to Not Sharing") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(label = c("concern", "control",  "endorsement", "graphic", "real info",
                             "relatable", "safety others","video")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5))
ggsave("neg_change_share.pdf", pic, width = 7, height = 5, units = "in")






