library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(stringi)
library(data.table)
library(testthat)

# Load data
income <- read.csv("downloads/data/ACS_13_1YR_S2401-by-occ.csv", stringsAsFactors=FALSE, sep=",")
income1 <- subset(income, level == 1)

glimpse(income1)

plot(income1$med_salary_male, income1$med_salary_female)
summary(income1)

income_range <- with(income1, range(c(med_salary_male, med_salary_female)))
ggplot(income1, aes(med_salary_male, med_salary_female)) + geom_point() +
  expand_limits(x = income_range, y = income_range) +
  geom_abline(intercept = 0, slope = 1) +
  geom_text(x = 20000, y = 50000, label = "Woman make more") +
  geom_text(x = 50000, y = 20000, label = "Man make more") +
  geom_text(x = 40000, y = 42000, angle = 35, label = "Equal pay")
# note the angle here needs to depend on the aspect ratio of the plotting window
# how to deal with that?