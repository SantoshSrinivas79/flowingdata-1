library(portfolio)

data <- read.csv("http://datasets.flowingdata.com/post-data.txt")
map.market(id=data$id, area=data$views, group=data$category, color=data$comments, main="FlowingData Map")
