# plot with type = "n" to set up axes etc fitting data to be plotted later
# barplot
# hist
# to get density histogram in ggplot use ..density.. as y aesthetic.

library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(stringi)
library(data.table)
library(testthat)

players <- read.csv("downloads/nba-players.csv", stringsAsFactors=FALSE)

glimpse(players)
ggpairs()

pl2 <- players %>%
  mutate(Team = as.factor(Team), School = as.factor(School)) %>%
  mutate(X2013salary = as.integer(stri_replace(substr(X2013salary, 2, 100), "", fixed = ",", mode = "all")))
glimpse(pl2)

ggpairs(pl2 %>% select(Age, X2013salary, Ht_inches, Wt, Exp, First_year))
table(pl2$Wt)

warriors <- subset(players, Team=="Warriors")

players_dt <- as.data.table(players)
expect_equal(as.data.frame(players_dt[Team == "Warriors"]), players %>% filter(Team == "Warriors"))

warriors.o <- warriors[order(warriors$Ht_inches),]
par(mar=c(5,10,5,5))
barplot(warriors.o$Ht_inches, names.arg=warriors.o$Name, horiz=TRUE, border=NA, las=1, main="Heights of Golden State Warriors")

ggplot(warriors %>% mutate(Name = factor(Name, levels = as.character(warriors.o$Name))),
       aes(x = Name, y = Ht_inches)) + geom_col() + ggplot2::coord_flip()

avgHeights <- aggregate(Ht_inches ~ POS, data=players, mean)
avgHeights.o <- avgHeights[order(avgHeights$Ht_inches, decreasing=FALSE),]
barplot(avgHeights.o$Ht_inches, names.arg=avgHeights.o$POS, border=NA, las=1)

ah <- players %>% group_by(POS) %>% summarize(aht = mean(Ht_inches)) %>%
  mutate(POS = factor(POS, levels = c("G", "G/F", "F", "F/C", "C")))
ggplot(ah, aes(x = POS, y = aht)) + geom_col()

pl_tab <- table(players$Ht_inches)
yy <- c(sapply(names(pl_tab), function(n) 1:pl_tab[n]), recursive = TRUE)

# make room for all points to be plotted
plot(sort(players$Ht_inches), yy, type="n", main="Player heights", xlab="inches", ylab="count")
points(sort(players$Ht_inches), yy, pch=21, col=NA, bg="#999999")

ggplot(data.frame(x = sort(players$Ht_inches), y = yy), aes(x, y)) + geom_point()

computed_point_coords <- as.data.frame(table(players$Ht_inches)) %>%
  rowwise() %>%
  do({data.frame(ht = .$Var1, y = 1:.$Freq)})

cpc_dt <- as.data.table(table(players$Ht_inches))[,.(ht = V1, N)][,.(y = 1:N), ht]

ggplot(computed_point_coords, aes(ht, y)) + geom_point()
ggplot(cpc_dt, aes(ht, y)) + geom_point()

barplot(pl_tab)
ggplot(as.data.frame(pl_tab), aes(Var1, Freq)) + geom_col()

hist(players$Ht_inches)
ggplot(players, aes(Ht_inches)) + geom_histogram(binwidth = 1, fill = "grey", color = "black")
ggplot(players, aes(Ht_inches)) + geom_histogram(breaks=c(60,70,72,75,80,85,90), fill = "grey", color = "black")
hist(players$Ht_inches, breaks=c(60,70,72,75,80,85,90))
ggplot(players, aes(Ht_inches, ..density..)) +
  geom_histogram(breaks = c(60,70,72,75,80,85,90), fill = "grey", color = "black")

ggplot(players %>% group_by(POS) %>% mutate(m = mean(Ht_inches)), aes(Ht_inches, ..density..)) +
  geom_histogram(bins = diff(range(players$Ht_inches)), fill = "grey", color = "black") +
  geom_vline(aes(xintercept = m, color = "red")) +
  facet_wrap(~ POS, ncol = 3)
