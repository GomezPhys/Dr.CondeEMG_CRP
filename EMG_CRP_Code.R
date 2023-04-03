### Required packages
library(tidyverse) ## To load excel sheet

library(table1) #tables
library(kableExtra) #tables

library(ggplot2) #data visualization
library(ggpubr)#data visualization
library(ggprism)##makes plots look like graphad


###
library(readxl)
EMG_CRP <- read_excel("~/Ergonomics Lab (PERROS 69)/EMG_CRP.xlsx", sheet = "EMG")

View(EMG_CRP)







###Convert from character to factor data
EMG_CRP$Site <- as.factor(EMG_CRP$Site)
EMG_CRP$Condition <- as.factor(EMG_CRP$Condition)
EMG_CRP$Day <- as.factor(EMG_CRP$Day)

###Order Day
EMG_CRP$Day <- ordered(EMG_CRP$Day, levels = c("1", "2", "3", "4", "5"))

###Order Condtion
EMG_CRP$Condition <- ordered(EMG_CRP$Condition, levels = c("Beginning", "End"))

###Order Site
EMG_CRP$Site <- ordered(EMG_CRP$Site, levels = c("ECRL", "Shoulder"))






                  