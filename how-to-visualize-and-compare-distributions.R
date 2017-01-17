library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(stringi)
library(data.table)
library(testthat)

# Load crime data
crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")
# Remove Washington, D.C.
crime.new <- crime[crime$state != "District of Columbia",]
# Remove national averages
crime.new <- crime.new[crime.new$state != "United States ",]


glimpse(crime)

ggpairs(crime %>% select(-state))
ggpairs(crime.new %>% select(-state))

boxplot(crime.new %>% select(-state), horizontal = TRUE)

crime_newt <- crime.new %>% gather(type, value, -state)
ggplot(crime_newt, aes(type, value)) + geom_boxplot() + coord_flip()

ggplot(crime_newt, aes(value)) +
  facet_wrap(~ type, nrow = 3, scales = "free_y") +
  geom_histogram(breaks=seq(0, 3500, 100), fill = "grey", color = "black")

crime_t <- crime %>% gather(type, value, -state)
ggplot(crime_t, aes(value)) +
  facet_wrap(~ type, nrow = 3, scales = "free_y") +
  geom_histogram(breaks=seq(0, 3500, 100), fill = "grey", color = "black")

# Multiple histograms      NOTE plotting crime, not crime.new
par(mfrow=c(3, 3))
colnames <- dimnames(crime.new)[[2]]
for (i in 2:8) {
  hist(crime[,i], xlim=c(0, 3500), breaks=seq(0, 3500, 100), main=colnames[i], probability=TRUE, col="gray", border="white")
}

ggplot(crime_newt, aes(value, ..density..)) +
  facet_wrap(~ type, nrow = 3, scales = "free") +
  geom_histogram(binwidth= 50) +
  geom_density(fill = "grey", alpha = 0.5) +
  geom_rug(aes(value), inherit.aes = FALSE)

ggplot(crime_newt %>% filter(type == "robbery"), aes(type, value)) + geom_violin() +
  coord_flip() + geom_rug()
