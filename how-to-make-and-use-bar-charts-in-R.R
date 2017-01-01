library(dplyr)
library(data.table)

income <- read.csv("downloads/data/income-totals.csv",
                   stringsAsFactors = FALSE,
                   colClasses = c(FIPS = "character"))

str(income)
summary(income)
glimpse(income)

sapply(names(income), function(cn) anyDuplicated(income[[cn]]))
sapply(income, function(col) length(unique(col))/length(col))

income_dt <- as.data.table(income)
income_dt[,length(unique())]

plot(income[,4:ncol(income)])

barplot(income$med_income, names.arg = income$name, las = 2)
# las: 0 = always parallel to the axis, 1 = always horizontal, 2 = always
#   perpendicular, 3 = always vertical.
barplot(income$med_income, names.arg = income$name,
        main = "Title for barplot",
        las = 2,
        mgp = c(3, 1, 0))

barplot(income$med_income, names.arg = income$name,
        main = "Title for barplot",
        las = 2,
        mgp = c(3, 1, 0),
        mar = c(1,2,2,0),
        horiz = TRUE)

library(ggplot2)
ggplot(income, aes(name, med_income)) +
  geom_col() +
  ggplot2::coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# ggplot uses the order of the levels in a factor to determine the oder of
# the columns.  If the input is a character vector, it is turned into a
# factor that is sorted with sort.list.  So if we want to determine the
# order of the columns, we make it a factor ourselves, with the levels in the
# order we want.
income_with_order <- income %>% arrange(med_income) %>%
  mutate(oname = factor(name, levels = name))

ggplot(income_with_order, aes(oname, med_income)) +
  geom_col() +
  ggplot2::coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

income_with_order <- income %>% arrange(med_income) %>%
  mutate(oname = factor(name, levels = rev(name)))

ggplot(income_with_order, aes(oname, med_income)) +
  geom_col() +
  ggplot2::coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

income_with_order <- income %>%
  mutate(oname = factor(name, levels = name[order(med_income, decreasing = TRUE)]))

ggplot(income_with_order, aes(oname, med_income)) +
  geom_col() +
  ggplot2::coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


income_with_order <- income %>% arrange(med_income) %>%
  mutate(oname = factor(name, levels = name[sort.list(name)]))

ggplot(income_with_order, aes(oname, med_income)) +
  geom_col() +
  ggplot2::coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

## alternating colors
income_with_order <- income %>% arrange(med_income) %>%
  mutate(oname = factor(name, levels = rev(name))) %>%
  mutate(col = factor(ifelse((1:length(name)) %% 2 == 1, "A", "B")))

ggplot(income_with_order, aes(oname, med_income, fill = col)) +
  geom_col() +
  ggplot2::coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("red", "blue"))


inc <- income[order(income$med_income, decreasing = TRUE),]
barplot(inc$med_income, names.arg = inc$name,
        main = "med incomes decreasing",
        cex.names = 0.5,
        las = 1,
        horiz = TRUE)

# Merge with state names
income0813 <- read.csv("downloads/data/income-2008-13.csv",
                       stringsAsFactors = FALSE,
                       sep = ",",
                       colClasses = c("FIPS" = "character"))
income0813 <- merge(income[,c("FIPS", "name")], income0813, by = "FIPS")

barplot(as.matrix(income0813[1:3, c("med2008", "med2013")]),
        beside = TRUE,
        legend.text = income0813$name[1:3])
barplot(t(as.matrix(income0813[1:3, c("med2008", "med2013")])),
        beside = TRUE,
        names.arg = income0813$name[1:3],
        legend.text = c("2008", "2013"))

barplot(matrix(c(1,1,1,2,1,3), byrow = TRUE, ncol = 2))
barplot(matrix(c(1,1,1,2,1,3), byrow = TRUE, ncol = 2), beside = TRUE)

library(tidyr)

income0813[1:3,] %>% gather(med_year, med_val, med2008, med2013) %>% separate(med_year, c("ignore", "year"), sep = 3)

inc2 <- income0813[1:10,] %>% gather(med_year, med_val, med2008, med2013) %>%
  extract(med_year, "year", regex = "([:digit:]+)", remove = FALSE)

ggplot(inc2, aes(year, med_val)) +
  geom_col(position = "dodge") + facet_grid(name ~ .) +
  coord_flip() +
  theme(strip.text.y = element_text(angle = 0))


#### Example from the course; you can encode groups into the heights and
#### names arguments for barplot.  This is really just "abusing" the way
#### barplot deals with NA
# Indexed categories
regions <- read.csv("downloads/data/state_geocodes_v2011.csv",
                    stringsAsFactors=FALSE,
                    colClasses=c("Region"="character", "Division"="character",
                                 s"FIPS"="character", "Name"="character"))
income_more <- merge(income, regions, by="FIPS")
unique_regions <- unique(income_more$Region)

bar_names <- c()
bar_heights <- c()

for (i in 1:length(unique_regions)) {
  
  curr_region <- subset(regions, Region==unique_regions[i] & FIPS=="00" & Division == "0")
  bar_names <- c(bar_names, NA, toupper(curr_region$Name))
  bar_heights <- c(bar_heights, NA, NA)
  
  states <- subset(income_more, Region == unique_regions[i])
  states <- states[order(states$med_income, decreasing=TRUE),]
  bar_names <- c(bar_names, states$Name)
  bar_heights <- c(bar_heights, states$med_income)
}


par(mar = c(5, 8, 2, 2))
barplot(rev(bar_heights), names.arg=rev(bar_names), las=2, horiz=TRUE, cex.names=0.7, cex.axis=0.7)

## method shown more clearly
barplot(c(3,4,NA,3,4), horiz = TRUE, names.arg = c(1,2,3,4,5), las = 1)
barplot(c(3,4,NA,NA,4), horiz = TRUE, names.arg = c(1,2,3,4,5), las = 1)
barplot(c(3,4,NA,NA,4), horiz = TRUE, names.arg = c(1,2,"CAT_NAME",NA,5), las = 1)
