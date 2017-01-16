library(testthat)

education <- read.csv("downloads/2009education.csv")
high.order <- order(education$high)
education.high <- education[high.order,]

############################
summary(education)
str(education)

library(data.table)

ed_dt <- as.data.table(education)
ed_dt.high <- ed_dt[order(high)]

expect_true(all.equal(education.high, ed_dt.high, check.attributes = FALSE))

##

library(dplyr)
ed_dp <- education
ed_dp.high <- ed_dp %>% arrange(high)

expect_true(all.equal(education.high, ed_dp.high, check.attributes = FALSE))

glimpse(education)
############################

plot(education$high)
plot(education.high$high)

with(education, plot(high, bs))

plot(education[,])

###########################
library(ggplot2)

ggplot(education, aes(x = high, y = bs)) + geom_point()
ggplot(education) + geom_point(aes(x = high, y = bs), size = 3)

library(GGally)
ggpairs(education[,2:4])

###########################

plot(education.high$high)
plot(education.high$high, type = "l")
plot(education.high$high, type = "h")  # high density chart (bar chart with skinny bars)
plot(education.high$high, type = "s")  # step

plot(education.high$high,
     las = 1,  # labels y-axis horizontal
     xlab = "XXX",
     ylab = "YYY",
     main = "Plot title")

barplot(education$high)
barplot(education$high, names.arg = education$state, horiz = TRUE, las = 1,
        cex.names = 0.5, border = NA)

boxplot(education$high)
boxplot(education[, 2:4])

###########################
gp <- ggplot(education.high, aes(1:nrow(education), high))
gp + geom_line()
gp + geom_line() + ylim(c(0, 100))
gp + geom_point()
gp + geom_col(width = 0.1)
gp + coord_cartesian(ylim = c(75, 100)) + geom_col(width = 0.1) 
gp + geom_col(width = 0.1) + ylim(c(75, 100))   ## empty plot


gp + geom_col()  # corresponds to barplot
gp + geom_col() + coord_flip()
gp + geom_col() +
  geom_text(aes(y = 1.5, hjust = 0), label = education$state, size = 2.5, color = "white") +
  coord_flip()

ggplot(education.high, aes(1:nrow(education), high)) + geom_col() + coord_flip()

ggplot(education.high, aes(state, high)) + geom_col() + coord_flip()

education.high <- within(education.high, stateo <- factor(state, levels = rev(as.character(state)), ordered = TRUE))

ggplot(education.high, aes(stateo, high)) + geom_col() + coord_flip()

ggplot(education.high, aes(as.character(1:nrow(education)), high)) +
  geom_col() +
  scale_x_discrete(breaks=as.character(1:nrow(education)), labels = education$state) +
  coord_flip()


ggplot(education.high, aes("x", high)) + geom_boxplot()
ggplot(education.high, aes("x", high)) + geom_boxplot()

library(tidyr)

xx <- education.high %>% gather(type, value, 2:4)
ggplot(xx, aes(type, value)) + geom_boxplot()

par(mfrow=c(2,3), mar=c(2,5,2,1), las=1, bty="n")
# then run some plots
# mfrow = 2 rows, 3 columns
# mar = margin
# other plotting options that are passed to all future plots (e.g. las = 1, all
#  labels on y-axes horizontal)