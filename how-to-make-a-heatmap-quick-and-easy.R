nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv", sep=",")

nba <- nba[order(nba$PTS),]
row.names(nba) <- nba$Name
nba <- nba[,2:20]
nba_matrix <- data.matrix(nba)
nba_heatmap <- heatmap(nba_matrix, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))

nba2 <- read.csv("http://datasets.flowingdata.com/ppg2008.csv", sep=",")
nba_tidy <- nba2 %>%
  gather(type, value, -Name) %>%
  group_by(type) %>%
  mutate(nvalue = value/max(value))

ggplot(nba_tidy, aes(type, Name, fill = nvalue)) + geom_tile() +
  scale_fill_gradient(low = "green", high = 'red')

ggplot(nba_tidy, aes(type, Name, fill = nvalue)) + geom_tile() +
  scale_fill_gradient2(low = "green", mid = "white",  high = 'red', midpoint = 0.5)
