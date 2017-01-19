library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)

crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv",
                  header=TRUE, sep="\t")

with(crime, {
  radius <- sqrt(population/ pi )
  symbols(murder, burglary, circles=radius, inches = .35, bg = "red")
  text(murder, burglary, state, cex = 0.5)
})

# add some nice things to the graph from comment

# different y-axis label
text( -1.4, 1350, expression(bold('Burglaries per\n100,000 population')), cex = 0.8, pos = 4, xpd = TRUE )


ylim <- c(200, 1250)
legPop <- c( 4e7, 2e7, 3e6 )
legRad <- sqrt( legPop / pi )
hin <- par('pin')[2]
burgPerInch <- ( ylim[2] - ylim[1] ) / hin
radPerInch <- max(radius)/0.35
heightAdj <- legRad/radPerInch*burgPerInch
symbols( rep(9,3), rep(200,3) + heightAdj, circles = legRad, inches = 0.35, add = TRUE)
tAdj <- strheight('40m', cex = 0.5)
text(rep(9,3), rep(200,3) + heightAdj*2 - tAdj, c('40m', '20m', '3m'), cex = 0.5)

with(crime, )

ggplot(crime, aes(murder, burglary, size = population)) +
  geom_point(shape = 21, fill = "red", color = "black") +
  scale_size_continuous(range = c(1, 15))
