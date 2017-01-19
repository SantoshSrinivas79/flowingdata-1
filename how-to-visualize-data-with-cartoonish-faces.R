library(dplyr)
library(ggplot2)
library(testthat)
library(data.table)
library(aplpack)

crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")

glimpse(crime)

# ?faces to see which features get mapped where
faces(crime %>% select(-state))
crime[10,]
faces(crime %>% select(-state), labels = crime$state)
