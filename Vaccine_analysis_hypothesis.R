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
data = as.data.table(read.csv("Academic_Survey_Research_in_Africa_2021_05_07_16_20_16.csv", 
                                  na.strings=c("","NA"), header = T))
## keep obs with pre and post outcomes
data = data[!is.na(dv_pre1) & !is.na(dv_post3),]
## text cleaning of pre and post outcomes (to lower cases)
data[dv_pre1=="Both", dv_pre1:="both"]
data[dv_pre1=="Messenger", dv_pre1:="messenger"]
data[dv_pre1=="Neither", dv_pre1:="neither"]
data[dv_pre1=="Timeline", dv_pre1:="timeline"]
data[dv_post3=="Both", dv_post3:="both"]
data[dv_post3=="Messenger", dv_post3:="messenger"]
data[dv_post3=="Neither", dv_post3:="neither"]
data[dv_post3=="Timeline", dv_post3:="timeline"]
summary(data$dv_pre1)
summary(data$dv_post3)


## Study 1: Effectiveness of nudges
nudge = c("control", "concern_addressing", "endorsement", "real_information","relatable_content", "safety_of_others")
nudge_data = data[treatment_nudges %in% nudge] 
nrow(nudge_data) ## 1001 observations
summary(nudge_data$treatment_nudges)
summary(nudge_data$dv_post3)

## generate treatment dummies
nudge_data[, control:=ifelse(treatment_nudges=="control" & !is.na(treatment_nudges), 1,0)]
nudge_data[, concern:=ifelse(treatment_nudges=="concern_addressing" & !is.na(treatment_nudges), 1,0)]
nudge_data[, endorse:=ifelse(treatment_nudges=="endorsement" & !is.na(treatment_nudges), 1,0)]
nudge_data[, realinfo:=ifelse(treatment_nudges=="real_information" & !is.na(treatment_nudges), 1,0)]
nudge_data[, relate:=ifelse(treatment_nudges=="relatable_content" & !is.na(treatment_nudges), 1,0)]
nudge_data[, safety:=ifelse(treatment_nudges=="safety_of_others" & !is.na(treatment_nudges), 1,0)]

## generate outcome variables
nudge_data[, post_share:=ifelse(dv_post3!="neither", 1,0)]
nudge_data[, pre_share:=ifelse(dv_pre1!="neither", 1,0)]
nudge_data[, pre_post:=post_share-pre_share]

## helper functions
table_function = function(study = "nudge", model = model){
  if (study=="nudge"){
    table = data.table(coeffs = model$coefficients[2:6], pvalues= pt(model$statistic, model$df, lower = FALSE)[2:6])
    table[, group:=1:5]
  } else {
    table = data.table(coeffs = model$coefficients[2:3], pvalues= pt(model$statistic, model$df, lower = FALSE)[2:3])
    table[, group:=1:2]
  }
  table$pvalues = as.character(round(table$pvalues,digits=3))
  table[, signif:= ifelse(table$pvalues<0.01,1,0)]
  table[, position:=ifelse(coeffs>=0, coeffs+0.005,0.005)]
  table = as.data.frame(table)
  return(table)
}

  
plot_function = function(study = "nudge", table = table, filename = filename){
  plot = ggplot(data = table, aes (x = as.factor(group), y=coeffs)) + 
    geom_bar(aes (fill = as.factor(signif)), width = 0.5, stat="identity") + 
    scale_fill_manual(name = "significant", values=c("blue", "red"), 
                      labels = c("not significant", "significant")) + 
    xlab("Treatment")+ ylab("Coefficients") + 
    ## scale_x_continuous(breaks = c(2010:2018)) + 
    geom_text(aes(label = pvalues, y= position), position = position_dodge(0.9), size = 3, color= "black") + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5), 
                       legend.position = "right",
                       panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                       ## legend.title = element_blank(), 
                       axis.title.x=element_text(size=15),
                       axis.title.y=element_text(size=15)) 
  if (study == "nudge"){
    plot = plot + scale_x_discrete(label = c("concern", "endorsement", "real info", "relatable", "safety")) 
  } else {
    plot = plot + scale_x_discrete(label = c("video", "graphic")) 
  }
  
  
 
  scale_x_discrete(label = c("concern", "endorsement", "real info", "relatable", "safety")) 
  output_file = paste(filename,".pdf", sep = '')
  ggsave(output_file, plot, width = 8, height = 4, units = "in")
}
  

## Model 1: post only, no covariates
model1 = lm_robust(post_share ~ concern+endorse+realinfo+relate+safety, data = nudge_data)
table1 = table_function(model1)
plot_function(table1, filename = "nudge_post_nocov")

## Model 2: post only, with demographic controls
model2 = lm_robust(post_share ~ concern+endorse+realinfo+relate+safety+
                     as.factor(cv_gender) + as.factor(province) + cv_age, data = nudge_data)
table2 = table_function(model2)
plot_function(table2, filename = "nudge_post_demographics")

## Model 3: pre-post , no covariates
model3 = lm_robust(pre_post ~ concern+endorse+realinfo+relate+safety, data = nudge_data)
table3 = table_function(model3)
plot_function(table3, filename = "nudge_prepost_nocov")

## Model 4: pre-post , with demographic controls
model4 = lm_robust(pre_post ~ concern+endorse+realinfo+relate+safety+
                     as.factor(cv_gender) + as.factor(province) + cv_age, data = nudge_data)
table4 = table_function(model4)
plot_function(table4, filename = "nudge_prepost_demographics")



## Study 2: Effectiveness of media formats
format_data = data[treatment_format %in% c("VID", "MOG") | treatment_nudges =="control"] 
nrow(format_data) ## 426 observations
summary(format_data$dv_post3)


## generate treatment dummies
format_data[, control:=ifelse(treatment_nudges=="control" & !is.na(treatment_nudges), 1,0)]
format_data[, video:=ifelse(treatment_format=="VID"  , 1,0)]
format_data[, graphic:=ifelse(treatment_format=="MOG", 1,0)]

## generate outcome variables
format_data[, post_share:=ifelse(dv_post3!="neither", 1,0)]
format_data[, pre_share:=ifelse(dv_pre1!="neither", 1,0)]
format_data[, pre_post:=post_share-pre_share]



## Model 1: post only, no covariates
model1 = lm_robust(post_share ~ video+graphic, data = format_data)
table1 = table_function("format", model1)
plot_function("format", table1, filename = "format_post_nocov")

## Model 2: post only, with demographic controls
model2 = lm_robust(post_share ~ video+graphic+
                     as.factor(cv_gender) , data = format_data)
table2 = table_function("format",model2)
plot_function("format", table2, filename = "format_post_demographics")

## Model 3: pre-post , no covariates
model3 = lm_robust(pre_post ~ video+graphic, data = format_data)
table3 = table_function("format", model3)
plot_function("format", table3, filename = "format_prepost_nocov")

## Model 4: pre-post , with demographic controls
model4 = lm_robust(pre_post ~ video+graphic + 
                     as.factor(cv_gender) , data = format_data)
table4 = table_function("format", model4)
plot_function("format", table4, filename = "format_prepost_demographics")


