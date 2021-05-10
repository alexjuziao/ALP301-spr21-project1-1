## Vaccine Analysis Codes
library(data.table)
library(foreign)
library(fastmatch)
library(readstata13)
library(ggplot2)
library(scales)
library(lubridate)
library(zoo)


rm(list=ls())
setwd("/Users/ziao/Desktop/ALP301/Data")
data = as.data.table(read.csv("Academic_Survey_Research_in_Africa_2021_05_04_15_55_42.csv", 
                                  na.strings=c("","NA"), header = T))
names(data)
## consent = YES, 393 responses
data = data[consent_response %in% c("yes","Yes", "YES"),]
## got assignment a treatment
data = data[!is.na(treatment_format)]
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
names(table) = c("group", "count")

pic = ggplot(table, aes(x = group, y = count, width=.7)) + 
  geom_bar(stat="identity", position="dodge", fill="lightskyblue", colour="navy") + theme_light() + 
  xlab("\n Group") + ylab("Number of Respondents \n") + 
  labs(title = "Number of Respondents by Treatment Assignment (N=276)") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(label = c("concern", "control", "deliberation", "endorsement", "graphic", "real info",
                             "relatable", "safety others", "safety self", "text", "video")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5))
ggsave("treatment_hist.pdf", pic, width = 7, height = 5, units = "in")

## look at attrition
data2 = data[!is.na(dv_send_post4),]
table2 = as.data.frame(table(data2$treatmentX))
names(table2) = c("group", "count")
attrition = merge(table, table2, by = "group")
attrition$remaining = attrition$count.y/ attrition$count.x

pic = ggplot(attrition, aes(x = group, y = remaining, width=.7)) + 
  geom_bar(stat="identity", position="dodge", fill="lightskyblue", colour="navy") + theme_light() + 
  xlab("\n Group") + ylab("Share of Remaining Respondents \n") + 
  labs(title = "Share of Remaining Respondents by Treatment Assignments (avg = 0.52)") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(label = c("concern", "control", "deliberation", "endorsement", "graphic", "real info",
                             "relatable", "safety others", "safety self", "text", "video")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5))
ggsave("attrition_hist.pdf", pic, width = 7, height = 5, units = "in")



## look at pre-post mean difference
data3 = data2
data3[, pre:=ifelse(dv_send_pre1=="Yes",1,0)]
data3[, post:=ifelse(dv_send_post4=="Yes",1,0)]
data3[, diff:=post-pre]
summary(data3$diff)

table3 = as.data.frame(table(data3[diff==1,]$treatmentX))
names(table3) = c("group", "count")


pic = ggplot(table3, aes(x = group, y = count, width=.7)) + 
  geom_bar(stat="identity", position="dodge", fill="lightskyblue", colour="navy") + theme_light() + 
  xlab("\n Group") + ylab("Number of Respondents \n") + 
  labs(title = "Number of Respondents who Switched from Not Sharing to Sharing") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(label = c("concern", "control", "deliberation", "endorsement", "graphic", "real info",
                             "relatable", "safety others", "safety self", "text", "video")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5))
ggsave("pos_change_post_hist.pdf", pic, width = 7, height = 5, units = "in")


table4 = as.data.frame(table(data3[diff==-1,]$treatmentX))
names(table4) = c("group", "count")

pic = ggplot(table4, aes(x = group, y = count, width=.7)) + 
  geom_bar(stat="identity", position="dodge", fill="indianred1", colour="indianred4") + theme_light() + 
  xlab("\n Group") + ylab("Number of Respondents \n") + 
  labs(title = "Number of Respondents who Switched from Sharing to Not Sharing") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(label = c("concern", "control", "deliberation", "endorsement", "graphic", "real info",
                             "relatable", "safety others", "safety self", "text", "video")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5))
ggsave("neg_change_post_hist.pdf", pic, width = 7, height = 5, units = "in")



## look at pre-post mean difference
data3 = data2
data3[, pre:=ifelse(dv_timeline_pre1=="Yes",1,0)]
data3[, post:=ifelse(dv_timeline_post4=="Yes",1,0)]
data3[, diff:=post-pre]
summary(data3$diff)

table3 = as.data.frame(table(data3[diff==1,]$treatmentX))
names(table3) = c("group", "count")


pic = ggplot(table3, aes(x = group, y = count, width=.7)) + 
  geom_bar(stat="identity", position="dodge", fill="lightskyblue", colour="navy") + theme_light() + 
  xlab("\n Group") + ylab("Number of Respondents \n") + 
  labs(title = "Number of Respondents who Switched from Not Sharing to Sharing") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(label = c("concern", "control", "deliberation", "endorsement", "graphic", "real info",
                             "relatable", "safety others", "safety self", "text", "video")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5))
ggsave("pos_change_timeline_hist.pdf", pic, width = 7, height = 5, units = "in")


table4 = as.data.frame(table(data3[diff==-1,]$treatmentX))
names(table4) = c("group", "count")

pic = ggplot(table4, aes(x = group, y = count, width=.7)) + 
  geom_bar(stat="identity", position="dodge", fill="indianred1", colour="indianred4") + theme_light() + 
  xlab("\n Group") + ylab("Number of Respondents \n") + 
  labs(title = "Number of Respondents who Switched from Sharing to Not Sharing") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(label = c("concern", "control", "deliberation", "endorsement", "graphic", "real info",
                             "relatable", "safety others", "safety self", "text", "video")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5))
ggsave("neg_change_timeline_hist.pdf", pic, width = 7, height = 5, units = "in")








