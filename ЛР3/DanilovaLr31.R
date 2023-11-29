nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T)
edges <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T)
 
nodes
edges

nrow(edges)
nrow(unique(edges[,c("from", "to")]))
edges <- aggregate(edges[,3], edges[,-3], sum)
colnames(edges)[4] <- "weight"

install.packages("igraph")
library('igraph')
net <- graph.data.frame(edges, nodes, directed=T)
net
plot(net)

net <- simplify(net, remove.loops = T)
plot(net, vertex.color='green', vertex.size=15, edge.color='blue', edge.arrow.size=.5)

colour <- c("red", "yellow", "green")
V(net)$color <- colour[V(net)$media.type]
V(net)$size <- V(net)$audience.size*0.5
E(net)$width <- E(net)$weight/5
#V(net)$label=V(net)$media
plot(net, vertex.label.cex=0.8,  edge.arrow.size=0.3, vertex.label.font=2)
legend(x=-1, y=-1, c("Newspaper","Television", "Online News"), pch=21, pt.bg=colour)


###########     часть 2
data <- read.csv("TreeMap.csv", header=T)
data
find_na <- function(data){
  sum =0
  for(i in 1:nrow(data)){
    for (j in 1:ncol(data))
    {
      sum<-sum+is.na(data[i,j])
    }
  }
  print(sum)
}

find_na(data)

install.packages("treemap")
library(treemap)

treemap(data, index = c("Category","Sub.Category", "Brand"),
        
        vSize = "High.Bid", 
        
        align.labels = list(c("center", "top"), c("center", "center"), c("center", "bottom")),
        
        title = "Карта деревьев")

############

library(readxl)
data2 <- read.csv("gdp.csv")
data2
find_na(data2)

install.packages("mosaic")
library(mosaic)
mWorldMap(data2, key = "Country.Name", fill = "Gdp")
