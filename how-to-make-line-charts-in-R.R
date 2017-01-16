# note flowing data course how to make line charts in R

library(dplyr)
library(tidyr)
library(ggplot2)
library(testthat)

# Load CSV file
life <- read.csv("downloads/data/life-expectancy-cleaned.csv", stringsAsFactors=FALSE)
glimpse(life)
length(life$Country.Code)
length(unique(life$Country.Code))

library(reshape)
x1 <- melt(life, id = c("Country.Name", "Country.Code"))

life2 <- gather(life, variable, value, -Country.Name, -Country.Code) %>%
  mutate(variable = factor(variable))

expect_true(all.equal(x1, x2))

usa <- life2 %>% filter(Country.Code == "USA")

plot(usa$value, type = "l")
plot(life2[,c(-1,-2)])

ggplot(life2, aes(x = variable, y = value)) + geom_boxplot()
expect_equal(sum(is.na(life)), 683)

life3 <- life2 %>% mutate(year = as.integer(substr(variable, 2, 100)))
ggplot(life3, aes(x = year, y = value, fill = Country.Code)) +
  geom_line() +
  guides(color = FALSE)

countries <- read.csv("downloads/data/country-regions.csv", stringsAsFactors = FALSE)
glimpse(countries)
life4 <- merge(life3, countries, by.x = "Country.Code", by.y = "CountryCode")
glimpse(life4)

ggplot(life4, aes(x = year, y = value, fill = Country.Code)) +
  facet_wrap("RegionName", ncol = 3) +
  geom_line() +
  ylab("Years from birth")
